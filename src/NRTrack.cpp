#include <fcntl.h>
#include <memory>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include <R.h>
#include <Rinternals.h>

#ifdef length
#undef length
#endif

#include "naryn.h"
#include "NRDb.h"
#include "NRTrack.h"

//-------------------------------- NRTrack::DataFetcher -----------------------------------

void NRTrack::DataFetcher::init(NRTrack *track, unordered_set<double> &&vals)
{
	m_track = track;
    m_vals2compare = move(vals);
	m_data_idx = (unsigned)0;
	m_rec_idx = (unsigned)0;
    m_last_id = 0;
    m_val = numeric_limits<double>::quiet_NaN();
    m_sp.reset();
}

void NRTrack::DataFetcher::register_function(NRTrack::Func func)
{
	if (func == QUANTILE)
		m_sp.init(g_naryn->max_data_size(), g_naryn->quantile_edge_data_size(), g_naryn->quantile_edge_data_size());

    m_function = func;
}


//-------------------------------- NRTrack::Iterator -----------------------------------

void NRTrack::Iterator::init(NRTrack *track, unsigned stime, unsigned etime, unordered_set<double> &&vals, NRTimeStamp::Hour expiration)
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


//-------------------------------- NRTrack -------------------------------------------------

const int    NRTrack::SIGNATURE = 0xc0ffee;
const double NRTrack::DENSE_TRACK_MIN_DENSITY = 0.4;

const char *NRTrack::TRACK_TYPE_NAMES[NUM_TRACK_TYPES] = { "sparse", "dense" };
const char *NRTrack::DATA_TYPE_NAMES[NUM_DATA_TYPES] = { "float", "double" };

const NRTrack::FuncInfo NRTrack::FUNC_INFOS[NRTrack::NUM_FUNCS] = {
    // name                   categorial  quantitative  keepref
    { "value",                true,       false,        true  },
    { "exists",               true,       false,        true  },
    { "frequent",             true,       false,        false },
    { "sample",               true,       true,         false },
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

NRTrack *NRTrack::unserialize(const char *name, const char *filename)
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
            TGLError<NRTrack>(BAD_FORMAT, "Track file %s is empty (0)", filename);

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
    		TGLError<NRTrack>(BAD_FORMAT, "Invalid format of a track %s (1)", name);

        NRTrack *track;

    	if (track_type == SPARSE) {
    		if (data_type == FLOAT) 
    			track = new NRTrackSparse<float>(name, FLOAT, flags, mem, pos, sb.st_size, minid, maxid, mintime, maxtime);
    		else if (data_type == DOUBLE) 
    			track = new NRTrackSparse<double>(name, DOUBLE, flags, mem, pos, sb.st_size, minid, maxid, mintime, maxtime);
    	} else if (track_type == DENSE) {
    		if (data_type == FLOAT) 
    			track = new NRTrackDense<float>(name, FLOAT, flags, mem, pos, sb.st_size, minid, maxid, mintime, maxtime);
    		else if (data_type == DOUBLE) 
    			track = new NRTrackDense<double>(name, DOUBLE, flags, mem, pos, sb.st_size, minid, maxid, mintime, maxtime);
    	}

        if (!track)
            TGLError<NRTrack>(BAD_FORMAT, "Invalid format of a track %s (5)", name);

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

extern "C" {

SEXP emr_track_rm(SEXP _track, SEXP _envir)
{
	try {
		Naryn naryn(_envir);

		// check the arguments
		if (!isString(_track) || Rf_length(_track) != 1)
			verror("Track argument is not a string");

		const char *trackname = CHAR(STRING_ELT(_track, 0));
        vdebug("Removing track %s\n", trackname);
        SEXP answer;
        const NRDb::TrackInfo *track_info = g_db->track_info(trackname);

        if (!track_info)
            verror("Track %s does not exist", trackname);

        vdebug("Removing track file %s\n", track_info->filename.c_str());
        if (unlink(track_info->filename.c_str()))
            verror("Deleting file %s: %s", track_info->filename.c_str(), strerror(errno));

        g_db->unload_track(trackname);
	} catch (TGLException &e) {
		rerror("%s", e.msg());
	}
	return R_NilValue;
}

SEXP emr_track_info(SEXP _track, SEXP _envir)
{
	try {
		Naryn naryn(_envir);

		// check the arguments
		if (!isString(_track) || Rf_length(_track) != 1)
			verror("Track argument is not a string");

        enum { PATH, TYPE, DATA_TYPE, CATEGORIAL, NUM_VALS, NUM_UNIQUE_VALS, MIN_VAL, MAX_VAL, MIN_ID, MAX_ID, MIN_TIME, MAX_TIME, NUM_COLS };

        const char *COL_NAMES[NUM_COLS] = { "path", "type", "data.type", "categorial", "num.vals", "num.unique.vals", "min.val", "max.val", "min.id", "max.id", "min.time", "max.time" };

		const char *trackname = CHAR(STRING_ELT(_track, 0));
        SEXP answer;
        SEXP names;
        NRTrack *track = g_db->track(trackname);
        const NRDb::TrackInfo *track_info = g_db->track_info(trackname);

        if (!track)
            verror("Track %s does not exist", trackname);

        rprotect(answer = allocVector(VECSXP, NUM_COLS));
        rprotect(names = allocVector(STRSXP, NUM_COLS));

        // path
        SET_VECTOR_ELT(answer, PATH, allocVector(STRSXP, 1));
        SET_STRING_ELT(VECTOR_ELT(answer, PATH), 0, mkChar(track_info->filename.c_str()));

        // type
        SET_VECTOR_ELT(answer, TYPE, allocVector(STRSXP, 1));
        SET_STRING_ELT(VECTOR_ELT(answer, TYPE), 0, mkChar(NRTrack::TRACK_TYPE_NAMES[track->track_type()]));

        // data.type
        SET_VECTOR_ELT(answer, DATA_TYPE, allocVector(STRSXP, 1));
        SET_STRING_ELT(VECTOR_ELT(answer, DATA_TYPE), 0, mkChar(NRTrack::DATA_TYPE_NAMES[track->data_type()]));

        // categorial
        SET_VECTOR_ELT(answer, CATEGORIAL, allocVector(LGLSXP, 1));
        LOGICAL(VECTOR_ELT(answer, CATEGORIAL))[0] = track->is_categorial();

        // num.vals
        SET_VECTOR_ELT(answer, NUM_VALS, allocVector(INTSXP, 1));
        INTEGER(VECTOR_ELT(answer, NUM_VALS))[0] = track->size();
        
        // num.unique.vals
        SET_VECTOR_ELT(answer, NUM_UNIQUE_VALS, allocVector(INTSXP, 1));
        INTEGER(VECTOR_ELT(answer, NUM_UNIQUE_VALS))[0] = track->unique_size();

        // min.val
        SET_VECTOR_ELT(answer, MIN_VAL, allocVector(REALSXP, 1));
        REAL(VECTOR_ELT(answer, MIN_VAL))[0] = track->minval();

        // max.val
        SET_VECTOR_ELT(answer, MAX_VAL, allocVector(REALSXP, 1));
        REAL(VECTOR_ELT(answer, MAX_VAL))[0] = track->maxval();

        // min.id
        SET_VECTOR_ELT(answer, MIN_ID, allocVector(INTSXP, 1));
        INTEGER(VECTOR_ELT(answer, MIN_ID))[0] = track->minid();

        // max.id
        SET_VECTOR_ELT(answer, MAX_ID, allocVector(INTSXP, 1));
        INTEGER(VECTOR_ELT(answer, MAX_ID))[0] = track->maxid();

        // min.time
        SET_VECTOR_ELT(answer, MIN_TIME, allocVector(INTSXP, 1));
        INTEGER(VECTOR_ELT(answer, MIN_TIME))[0] = track->mintime();

        // max.time
        SET_VECTOR_ELT(answer, MAX_TIME, allocVector(INTSXP, 1));
        INTEGER(VECTOR_ELT(answer, MAX_TIME))[0] = track->maxtime();

        for (int i = 0; i < NUM_COLS; ++i)
            SET_STRING_ELT(names, i, mkChar(COL_NAMES[i]));

        setAttrib(answer, R_NamesSymbol, names);

        return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
	}
	return R_NilValue;
}

SEXP emr_track_ids(SEXP _track, SEXP _envir)
{
	try {
		Naryn naryn(_envir);

		// check the arguments
		if (!isString(_track) || Rf_length(_track) != 1)
			verror("Track argument is not a string");

		const char *trackname = CHAR(STRING_ELT(_track, 0));
        SEXP answer;
        vector<unsigned> ids;
        NRTrack *track = g_db->track(trackname);

        if (!track)
            verror("Track %s does not exist", trackname);

        track->ids(ids);
        g_naryn->verify_max_data_size(ids.size(), "IDs");
        return NRPoint::convert_ids(ids, 1, false);
	} catch (TGLException &e) {
		rerror("%s", e.msg());
	}
	return R_NilValue;
}

SEXP emr_track_unique(SEXP _track, SEXP _envir)
{
	try {
		Naryn naryn(_envir);

		// check the arguments
		if (!isString(_track) || Rf_length(_track) != 1)
			verror("Track argument is not a string");

		const char *trackname = CHAR(STRING_ELT(_track, 0));
        SEXP answer;
        NRTrack *track = g_db->track(trackname);

        if (!track)
            verror("Track %s does not exist", trackname);

        g_naryn->verify_max_data_size(track->unique_size(), "Result");
        rprotect(answer = allocVector(REALSXP, track->unique_size()));

        vector<double> unique_vals;
        track->unique_vals(unique_vals);

        for (size_t i = 0; i < unique_vals.size(); ++i)
            REAL(answer)[i] = unique_vals[i];

        return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
	}
	return R_NilValue;
}

SEXP emr_track_percentile(SEXP _track, SEXP _value, SEXP _lower, SEXP _envir)
{
	try {
		Naryn naryn(_envir);

		// check the arguments
		if (!isString(_track) || Rf_length(_track) != 1)
			verror("The value of 'track' parameter must be a string");

        if (!isReal(_value) && !isInteger(_value))
            verror("'val' argument must be numeric");

        if (!isLogical(_lower) || Rf_length(_lower) != 1)
            verror("'lower' argument must be a logical value");

		const char *trackname = CHAR(STRING_ELT(_track, 0));
        NRTrack *track = g_db->track(trackname);

        if (!track)
            verror("Track %s does not exist", trackname);

        if (track->is_categorial()) 
            verror("Track %s is categorial: percentile queries are not supported", trackname);

        SEXP answer;
        int num_vals = Rf_length(_value);

        rprotect(answer = allocVector(REALSXP, num_vals));

        if (asLogical(_lower)) {
            if (isReal(_value)) {
                for (int i = 0; i < num_vals; ++i)
                    REAL(answer)[i] = R_FINITE(REAL(_value)[i]) ? track->percentile_lower(REAL(_value)[i]) : numeric_limits<double>::quiet_NaN();
            } else {
                for (int i = 0; i < num_vals; ++i)
                    REAL(answer)[i] = INTEGER(_value)[i] == NA_INTEGER ? track->percentile_lower(INTEGER(_value)[i]) : numeric_limits<double>::quiet_NaN();
            }
        } else {
            if (isReal(_value)) {
                for (int i = 0; i < num_vals; ++i)
                    REAL(answer)[i] = R_FINITE(REAL(_value)[i]) ? track->percentile_lower(REAL(_value)[i]) : numeric_limits<double>::quiet_NaN();
            } else {
                for (int i = 0; i < num_vals; ++i)
                    REAL(answer)[i] = INTEGER(_value)[i] == NA_INTEGER ? track->percentile_lower(INTEGER(_value)[i]) : numeric_limits<double>::quiet_NaN();
            }
        }

        return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
	}
	return R_NilValue;
}

}
