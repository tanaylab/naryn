#include <fcntl.h>
#include <memory>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "EMRDb.h"
#include "EMRTrack.h"
#include "naryn.h"

//-------------------------------- EMRTrack::DataFetcher -----------------------------------

EMRTrack::DataFetcher::~DataFetcher()
{
    if (m_track_ownership)
        delete m_track;
}

void EMRTrack::DataFetcher::init(EMRTrack *track, bool track_ownership, unordered_set<double> &&vals)
{
	m_track = track;
    m_track_ownership = track_ownership;
    m_vals2compare = move(vals);
	m_data_idx = (unsigned)0;
	m_rec_idx = (unsigned)0;
    m_last_id = 0;
    m_val = numeric_limits<double>::quiet_NaN();
    m_sp.reset();
}

void EMRTrack::DataFetcher::register_function(EMRTrack::Func func)
{
	if (func == QUANTILE)
		m_sp.init(g_naryn->max_data_size(), g_naryn->quantile_edge_data_size(), g_naryn->quantile_edge_data_size());

    m_function = func;
}


//-------------------------------- EMRTrack::Iterator -----------------------------------

void EMRTrack::Iterator::init(EMRTrack *track, unsigned stime, unsigned etime, unordered_set<double> &&vals, EMRTimeStamp::Hour expiration)
{
    m_track = track;
    m_data_idx = (unsigned)0;
    m_rec_idx = (unsigned)0;
    m_isend = false;
    m_stime = stime;
    m_etime = etime;
    m_vals = move(vals);
    m_expiration = expiration;
}


//-------------------------------- EMRTrack -------------------------------------------------

const int    EMRTrack::SIGNATURE = 0xc0ffee;
const double EMRTrack::DENSE_TRACK_MIN_DENSITY = 0.4;

const char *EMRTrack::TRACK_TYPE_NAMES[NUM_TRACK_TYPES] = { "sparse", "dense" };
const char *EMRTrack::DATA_TYPE_NAMES[NUM_DATA_TYPES] = { "float", "double" };

// When adding a new function do not forget to update BinsManager::BinsManager()
const EMRTrack::FuncInfo EMRTrack::FUNC_INFOS[EMRTrack::NUM_FUNCS] = {
    // name                   categorical quantitative  keepref
    { "value",                true,       false,        true  },
    { "exists",               true,       false,        true  },
    { "frequent",             true,       false,        false },
    { "sample",               true,       true,         false },
    { "sample.time",          true,       true,         false },
    { "avg",                  false,      true,         true  },
    { "size",                 true,       true,         false },
    { "min",                  false,      true,         false },
    { "max",                  false,      true,         false },
    { "earliest",             true,       true,         false },
    { "latest",               true,       true,         false },
    { "closest",              true,       true,         false },
    { "earliest.time",        true,       true,         true  },
    { "latest.time",          true,       true,         true  },
    { "closest.earlier.time", true,       true,         true  },
    { "closest.later.time",   true,       true,         true  },
    { "stddev",               false,      true,         false },
    { "sum",                  false,      true,         false },
    { "quantile",             false,      true,         false },
    { "percentile.upper",     false,      true,         true  },
    { "percentile.lower",     false,      true,         true  },
    { "percentile.upper.min", false,      true,         false },
    { "percentile.lower.min", false,      true,         false },
    { "percentile.upper.max", false,      true,         false },
    { "percentile.lower.max", false,      true,         false },
    { "lm.slope",             false,      true,         false },
    { "lm.intercept",         false,      true,         false },
    { "dt1.earliest",         true,       true,         false },
    { "dt1.latest",           true,       true,         false },
    { "dt2.earliest",         true,       true,         false },
    { "dt2.latest",           true,       true,         false }
};

EMRTrack *EMRTrack::unserialize(const char *name, const char *filename)
{
    int fd = -1;
    struct stat sb;
    void *mem = MAP_FAILED;

    try {
        if ((fd = open(filename, O_RDONLY, 0)) == -1)
            verror("Opening file %s: %s", filename, strerror(errno));

        if (fstat(fd, &sb) == -1)
            verror("stat failed on file %s: %s", filename, strerror(errno));

        if (!sb.st_size)
            TGLError<EMRTrack>(BAD_FORMAT, "Track file %s is empty (0)", filename);

        if ((mem = mmap(NULL, sb.st_size, PROT_READ, MAP_PRIVATE | MAP_POPULATE, fd, 0)) == MAP_FAILED)
            verror("mmap failed on file %s: %s", filename, strerror(errno));

        close(fd);
        fd = -1;

        size_t pos = 0;
    	int signature;
    	int track_type;
    	int data_type;
        unsigned flags;
        unsigned minid;
        unsigned maxid;
        unsigned mintime;
        unsigned maxtime;

        read_datum(mem, pos, sb.st_size, signature, name);
        read_datum(mem, pos, sb.st_size, track_type, name);
        read_datum(mem, pos, sb.st_size, data_type, name);
        read_datum(mem, pos, sb.st_size, flags, name);
        read_datum(mem, pos, sb.st_size, minid, name);
        read_datum(mem, pos, sb.st_size, maxid, name);
        read_datum(mem, pos, sb.st_size, mintime, name);
        read_datum(mem, pos, sb.st_size, maxtime, name);

    	if (signature != SIGNATURE) 
    		TGLError<EMRTrack>(BAD_FORMAT, "Invalid format of a track %s (1)", name);

        EMRTrack *track = NULL;

    	if (track_type == SPARSE) {
    		if (data_type == FLOAT) 
    			track = new EMRTrackSparse<float>(name, FLOAT, flags, mem, pos, sb.st_size, minid, maxid, mintime, maxtime);
    		else if (data_type == DOUBLE) 
    			track = new EMRTrackSparse<double>(name, DOUBLE, flags, mem, pos, sb.st_size, minid, maxid, mintime, maxtime);
    	} else if (track_type == DENSE) {
    		if (data_type == FLOAT) 
    			track = new EMRTrackDense<float>(name, FLOAT, flags, mem, pos, sb.st_size, minid, maxid, mintime, maxtime);
    		else if (data_type == DOUBLE) 
    			track = new EMRTrackDense<double>(name, DOUBLE, flags, mem, pos, sb.st_size, minid, maxid, mintime, maxtime);
    	}

        if (!track)
            TGLError<EMRTrack>(BAD_FORMAT, "Invalid format of a track %s (5)", name);

        track->m_timestamp = sb.st_mtim;
    	return track;
    }
    catch (...) {
        if (fd != -1)
            close(fd);
        if (mem != MAP_FAILED)
            munmap(mem, sb.st_size);
        throw;
    }

    return NULL;
}
