#ifndef EMRTRACK_H_INCLUDED
#define EMRTRACK_H_INCLUDED

#include <cmath>
#include <limits>
#include <map>
#include <memory>
#include <string.h>
#include <typeinfo>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unordered_set>
#include <vector>

#include "EMR.h"
#include "BinFinder.h"
#include "BufferedFile.h"
#include "EMRInterval.h"
#include "EMRPoint.h"
#include "EMRTrackData.h"
#include "StreamPercentiler.h"

class EMRTrack {
public:
	enum { FILE_ERROR, BAD_DATA_TYPE, BAD_FORMAT, BAD_ATTRS };

	enum TrackType { SPARSE, DENSE, NUM_TRACK_TYPES };

	enum DataType { FLOAT, DOUBLE, NUM_DATA_TYPES };

	enum Func { VALUE, EXISTS, FREQUENT, SAMPLE, SAMPLE_TIME, AVG, SIZE, MIN, MAX, EARLIEST, LATEST, CLOSEST,
        EARLIEST_TIME, LATEST_TIME, CLOSEST_EARLIER_TIME, CLOSEST_LATER_TIME, STDDEV, SUM, QUANTILE,
        PV_UPPER, PV_LOWER, PV_MIN_UPPER, PV_MIN_LOWER, PV_MAX_UPPER, PV_MAX_LOWER,
        LM_SLOPE, LM_INTERCEPT, DT1_EARLIEST, DT1_LATEST, DT2_EARLIEST, DT2_LATEST, NUM_FUNCS
    };

    enum Flags { IS_CATEGORICAL = 0x1 };

	static const int    SIGNATURE;
	static const double DENSE_TRACK_MIN_DENSITY;
	static const char *TRACK_TYPE_NAMES[NUM_TRACK_TYPES];
	static const char *DATA_TYPE_NAMES[NUM_DATA_TYPES];

    struct FuncInfo {
        const char *name;
        bool        categorical;
        bool        quantitative;
        bool        keepref;
    };

    typedef map<string, string> TrackAttrs;

    static const FuncInfo FUNC_INFOS[NUM_FUNCS];

	virtual ~EMRTrack();

    const char *name() const { return m_name.c_str(); }
    const struct timespec &timestamp() const { return m_timestamp; }
	TrackType track_type() const { return m_track_type; }
	DataType data_type() const { return m_data_type; }

    unsigned flags() const { return m_flags; }
    bool     is_categorical() const { return m_flags & IS_CATEGORICAL; }
    bool     is_quantitative() const { return !is_categorical(); }

    unsigned minid() const { return m_min_id; }
    unsigned maxid() const { return m_max_id; }
    unsigned idrange() const { return maxid() - minid() + 1; }
    unsigned mintime() const { return m_min_time; }
    unsigned maxtime() const { return m_max_time; }
    unsigned timerange() const { return maxtime() - mintime() + 1; }

    virtual unsigned size() const = 0;
    virtual unsigned unique_size() const = 0;
    virtual void     unique_vals(vector<double> &vals) const = 0;
    virtual double   minval() const = 0;
    virtual double   maxval() const = 0;
    virtual float    percentile_upper(void *rec) const = 0;
    virtual float    percentile_upper(double val) const = 0;
    virtual float    percentile_lower(void *rec) const = 0;
    virtual float    percentile_lower(double val) const = 0;

    virtual void ids(vector<unsigned> &ids) = 0;
    virtual void ids(vector<unsigned> &ids, unordered_set<double> &vals2compare) = 0;
    virtual void data_recs(EMRTrackData<double> &data_recs) = 0;
    virtual void data_recs(EMRTrackData<float> &data_recs) = 0;

    virtual uint64_t count_ids(const vector<unsigned> &ids) const = 0;

    // Construct an intermediate track for virtual track queries based on EMRTrackData and the original track
    template <class T>
    static EMRTrack *construct(const char *name, EMRTrack *base_track, unsigned flags, EMRTrackData<T> &data);

    // Construct an intermediate track for virtual track queries based on EMRTrackData.
    // func must be specified to spare the built of percentiles/quantiles if they are not needed.
    template <class T>
    static EMRTrack *construct(const char *name, Func func, unsigned flags, EMRTrackData<T> &data);

    // data must be already finalized
	template <class T>
	static TrackType serialize(const char *filename, unsigned flags, EMRTrackData<T> &data);

	static EMRTrack *unserialize(const char *name, const char *filename);

	static TrackAttrs load_attrs(const char *track, const char *filename);
	static void save_attrs(const char *track, const char *filename, const TrackAttrs &attrs);

public:
	class DataFetcher {
	public:
		DataFetcher() : m_track(NULL) {}
		DataFetcher(EMRTrack *track, bool track_ownership, unordered_set<double> vals2compare) : m_track(NULL) { init(track, track_ownership, std::move(vals2compare)); }

        ~DataFetcher();

		void init(EMRTrack *track, bool track_ownership, unordered_set<double> &&vals2compare);
		void register_function(Func func);
		void set_vals(const EMRInterval &interv);

        EMRTrack *track() const { return m_track; }
        Func func() const { return m_function; }
        const unordered_set<double> &vals2compare() const { return m_vals2compare; }

        // works for all functions except QUANTILE
        double val() const { return m_val; }
		double quantile(double percentile);

protected:
		friend class EMRTrack;
        template <class T> friend class EMRTrackDense;
        template <class T> friend class EMRTrackSparse;

		EMRTrack              *m_track = NULL;
        bool                   m_track_ownership;
        unsigned               m_last_id;
        Func                   m_function;
        unordered_set<double>  m_vals2compare;
		unsigned               m_data_idx;   // last patient idx that is greater or equal than the last query
		unsigned               m_rec_idx;    // last record idx that is greater or equal than the last query
        double                 m_val;

        vector<double>         m_frequent_vals;
		StreamPercentiler<double> m_sp;
	};

public:
	class Iterator {
	public:
        enum OPS { eq, gt, lt, lte, gte } ops;
		Iterator() : m_track(NULL), m_isend(true) {}
        Iterator(EMRTrack *track, unsigned stime = 0, unsigned etime = (unsigned)-1, unordered_set<double> &&vals = unordered_set<double>(), EMRTimeStamp::Hour expiration = 0, Iterator::OPS op = OPS::eq);

        void init(EMRTrack *track, unsigned stime = 0, unsigned etime = (unsigned)-1, unordered_set<double> &&vals = unordered_set<double>(), EMRTimeStamp::Hour expiration = 0, Iterator::OPS op = OPS::eq);
        bool passed_operator(double val);

		bool begin() { return m_track->begin(*this); }
		bool next() { return m_track->next(*this); }
        bool next(const EMRPoint &jumpto) { return m_track->next(*this, jumpto); }   // reference in jumpto is ignored
		bool isend() { return m_isend; }
        

		EMRPoint &point() { return m_point; }

		// returns the maximal number of points iterator might produce;
		// note size() returns only the potential upper bound which can be used in conjunction with idx() for progress estimation
		unsigned size() const { return m_track->size(); }

		// returns current running index within [0, size()] range
		unsigned idx() const { return m_running_idx; }          // returns current index in [0, size()] range

        const EMRTrack *track() const { return m_track; }

	protected:
		friend class EMRTrack;
		template <class T> friend class EMRTrackDense;
		template <class T> friend class EMRTrackSparse;

		EMRTrack             *m_track;
		EMRPoint              m_point;       // current iterator point
		unsigned              m_running_idx; // current iterator index in [0, size()] range
		unsigned              m_data_idx;    // last patient idx
		unsigned              m_rec_idx;     // last record idx
		unsigned              m_stime;       // time scope
		unsigned              m_etime;       // time scope
        unordered_set<double> m_vals;        // slice
        EMRTimeStamp::Hour    m_expiration;
		bool                  m_isend;
        OPS                   m_vals_op;

	};

protected:
    char           *m_mem{NULL};             // used for an intermediate track built in memory
    void           *m_shmem{MAP_FAILED};
    uint64_t          m_shmem_size;
    struct timespec m_timestamp;
    string          m_name;
	TrackType       m_track_type;
	DataType        m_data_type;
    unsigned        m_flags;
    EMRTrack        *m_base_track{NULL};     // if the track is intermediate (for virtual track queries), then base track is the one that is used as a source
    unsigned        m_min_id;
    unsigned        m_max_id;
    unsigned        m_min_time;
    unsigned        m_max_time;

    EMRTrack(const char *name, TrackType track_type, DataType data_type, unsigned flags, void *&mem, uint64_t size, unsigned minid, unsigned maxid, unsigned mintime, unsigned maxtime);
	EMRTrack(const char *name, TrackType track_type, DataType data_type, unsigned flags, EMRTrack *base_track, unsigned minid, unsigned maxid, unsigned mintime, unsigned maxtime);

    template <class T>
    static EMRTrack *construct(const char *name, EMRTrack *base_track, Func func, unsigned flags, EMRTrackData<T> &data);

    template <class T>
    static int read_datum(void *mem, uint64_t &pos, uint64_t size, T &t, const char *trackname);

	virtual void set_vals(DataFetcher &df, const EMRInterval &interv) = 0;
    void set_nan_vals(DataFetcher &df);

	virtual bool begin(Iterator &itr) = 0;
	virtual bool next(Iterator &itr) = 0;
    virtual bool next(Iterator &itr, const EMRPoint &jumpto) = 0; // reference in jumpto is ignored

    template <class T>
    void calc_vals(DataFetcher &df, const EMRInterval &interv, const T &srec, const T &erec);
};


//------------------------------ IMPLEMENTATION ----------------------------------------

#include "naryn.h"
#include "EMRTrackDense.h"
#include "EMRTrackSparse.h"

inline double EMRTrack::DataFetcher::quantile(double percentile)
{
	if (m_sp.stream_size()) {
		bool is_estimated;
		return m_sp.get_percentile(percentile, is_estimated);
	}
	return numeric_limits<float>::quiet_NaN();
}

inline void EMRTrack::DataFetcher::set_vals(const EMRInterval &interv)
{
    if (m_last_id != interv.id) {
        m_data_idx = (unsigned)0;
        m_rec_idx = (unsigned)0;
        m_last_id = interv.id;
    }
	m_track->set_vals(*this, interv);
}

inline EMRTrack::Iterator::Iterator(EMRTrack *track, unsigned stime, unsigned etime, unordered_set<double> &&vals, EMRTimeStamp::Hour expiration, Iterator::OPS op) :
    m_track(NULL),
    m_isend(true)
{
    init(track, stime, etime, std::move(vals), expiration, op);
}

inline bool EMRTrack::Iterator::passed_operator(double val){
    //we assume that if vals_op is not OPS::eq 
    //than m_vals is a vector of one value exactly.
    switch(m_vals_op){
        case OPS::eq:
            return (m_vals.find(val) != m_vals.end());
        case OPS::gt:
            return (val > *m_vals.begin());
        case OPS::gte:
            return (val >= *m_vals.begin());
        case OPS::lt:
            return (val < *m_vals.begin());
        case OPS::lte:
            return (val <= *m_vals.begin());
    }
    return false;

}

inline EMRTrack::EMRTrack(const char *name, TrackType track_type, DataType data_type, unsigned flags,
                        void *&mem, uint64_t size, unsigned minid, unsigned maxid, unsigned mintime, unsigned maxtime) :
    m_shmem_size(size),
    m_name(name),
	m_track_type(track_type),
    m_data_type(data_type),
    m_flags(flags),
    m_min_id(minid),
    m_max_id(maxid),
    m_min_time(mintime),
    m_max_time(maxtime)
{
    swap(m_shmem, mem); // move the ownership of the shared memory to the class
}

inline EMRTrack::EMRTrack(const char *name, TrackType track_type, DataType data_type, unsigned flags, EMRTrack *base_track,
                        unsigned minid, unsigned maxid, unsigned mintime, unsigned maxtime) :
    m_shmem_size(0),
    m_name(name),
	m_track_type(track_type),
	m_data_type(data_type),
    m_flags(flags),
    m_base_track(base_track),
    m_min_id(minid),
    m_max_id(maxid),
    m_min_time(mintime),
    m_max_time(maxtime)
{}

inline EMRTrack::~EMRTrack()
{
    free(m_mem);
    if (m_shmem != MAP_FAILED) {
        munmap(m_shmem, m_shmem_size);
    }
}

template <class T>
EMRTrack *EMRTrack::construct(const char *name, EMRTrack *base_track, unsigned flags, EMRTrackData<T> &data)
{
    return construct(name, base_track, NUM_FUNCS, flags, data);
}

template <class T>
EMRTrack *EMRTrack::construct(const char *name, Func func, unsigned flags, EMRTrackData<T> &data)
{
    return construct(name, NULL, func, flags, data);
}

template <class T>
EMRTrack *EMRTrack::construct(const char *name, EMRTrack *base_track, Func func, unsigned flags, EMRTrackData<T> &data)
{
    TrackType track_type;
    DataType data_type;

    // determine min / max patient id
    unsigned minid = numeric_limits<unsigned>::max();
    unsigned maxid = 0;
    unsigned mintime = EMRTimeStamp::MAX_HOUR;
    unsigned maxtime = 0;

    // determine the density of patient ids
    double data_density = 0;
    unsigned num_unique_ids = 0;

    data.finalize();
    if (data.data.size()) {
        unsigned last_id = numeric_limits<unsigned>::max();

        minid = data.data.front().id;
        maxid = data.data.back().id;

        for (auto idata = data.data.begin(); idata != data.data.end(); ++idata) {
            mintime = min(mintime, (unsigned)idata->timestamp.hour());
            maxtime = max(maxtime, (unsigned)idata->timestamp.hour());
            if (idata->id != last_id) {
                last_id = idata->id;
                ++num_unique_ids;
            }
        }

        data_density = num_unique_ids / (double)(maxid - minid + 1);
    }

    // based on patients density determine the format of the track
    track_type = data_density > DENSE_TRACK_MIN_DENSITY ? DENSE : SPARSE;

    if (typeid(T) == typeid(float))
        data_type = FLOAT;
    else if (typeid(T) == typeid(double))
        data_type = DOUBLE;
    else
        TGLError<EMRTrack>(BAD_DATA_TYPE, "Invalid data type %s used to create a track", typeid(T).name());

    if (minid > maxid) {
        minid = 1;
        maxid = 0;
    }

    if (mintime > maxtime) {
        mintime = 1;
        maxtime = 0;
    }

    EMRTrack *track = NULL;
    bool build_percentiles = false;

    // Percentiles require additional processing and space. Build them only if the virtual track really needs them (check func).
    // If the currently built track is based on another track, percentiles are taken from the base => no need to build them.
    if (!base_track) {
        // "case" is faster than "if" with many "and"s
        switch (func) {
        case PV_UPPER:
        case PV_LOWER:
        case PV_MIN_UPPER:
        case PV_MIN_LOWER:
        case PV_MAX_UPPER:
        case PV_MAX_LOWER:
            build_percentiles = true;
        default:
            break;
        }
    }

    if (track_type == SPARSE)
        track = new EMRTrackSparse<T>(name, base_track, data, num_unique_ids, data_type, build_percentiles, flags, minid, maxid, mintime, maxtime);
    else if (track_type == DENSE)
        track = new EMRTrackDense<T>(name, base_track, data, data_type, build_percentiles, flags, minid, maxid, mintime, maxtime);

    return track;
}

template <class T>
EMRTrack::TrackType EMRTrack::serialize(const char *filename, unsigned flags, EMRTrackData<T> &data)
{
    TrackType track_type;
	DataType data_type;

	// determine min / max patient id
	unsigned minid = numeric_limits<unsigned>::max();
	unsigned maxid = 0;
    unsigned mintime = EMRTimeStamp::MAX_HOUR;
    unsigned maxtime = 0;

	// determine the density of patient ids
	double data_density = 0;
	unsigned num_unique_ids = 0;

    data.finalize(true);
    if (data.data.size()) {
        unsigned last_id = numeric_limits<unsigned>::max();

        minid = data.data.front().id;
        maxid = data.data.back().id;

        for (auto idata = data.data.begin(); idata != data.data.end(); ++idata) {
            mintime = min(mintime, (unsigned)idata->timestamp.hour());
            maxtime = max(maxtime, (unsigned)idata->timestamp.hour());
            if (idata->id != last_id) {
                last_id = idata->id;
                ++num_unique_ids;
            }
        }

        data_density = num_unique_ids / (double)(maxid - minid + 1);
    }

	// based on patients density determine the format of the track
	track_type = data_density > DENSE_TRACK_MIN_DENSITY ? DENSE : SPARSE;

	if (typeid(T) == typeid(float))
		data_type = FLOAT;
	else if (typeid(T) == typeid(double))
		data_type = DOUBLE;
	else
		TGLError<EMRTrack>(BAD_DATA_TYPE, "Invalid data type %s used to create a track file %s", typeid(T).name(), filename);

	BufferedFile bfile;

	umask(07);

    if (minid > maxid) {
        minid = 1;
        maxid = 0;
    }

    if (mintime > maxtime) {
        mintime = 1;
        maxtime = 0;
    }

    vdebug("Creating a track file %s\n", filename);
	if (bfile.open(filename, "w"))
		TGLError<EMRTrack>(FILE_ERROR, "Opening a track file %s: %s", filename, strerror(errno));

	if (bfile.write(&SIGNATURE, sizeof(SIGNATURE)) != sizeof(SIGNATURE) ||
		bfile.write(&track_type, sizeof(track_type)) != sizeof(track_type) ||
		bfile.write(&data_type, sizeof(data_type)) != sizeof(data_type) ||
        bfile.write(&flags, sizeof(flags)) != sizeof(flags) ||
        bfile.write(&minid, sizeof(minid)) != sizeof(minid) ||
        bfile.write(&maxid, sizeof(maxid)) != sizeof(maxid) ||
        bfile.write(&mintime, sizeof(mintime)) != sizeof(mintime) ||
        bfile.write(&maxtime, sizeof(maxtime)) != sizeof(maxtime))
	{
		if (bfile.error())
			TGLError<EMRTrack>(FILE_ERROR, "Failed to write a track file %s: %s", filename, strerror(errno));
		TGLError<EMRTrack>(FILE_ERROR, "Failed to write a track file %s", filename);
	}

	if (track_type == SPARSE) 
		EMRTrackSparse<T>::serialize(bfile, data, num_unique_ids, flags);
	else if (track_type == DENSE) 
		EMRTrackDense<T>::serialize(bfile, data, minid, maxid, flags);

    return track_type;
}

inline void EMRTrack::set_nan_vals(EMRTrack::DataFetcher &df)
{
    df.m_val = df.m_function == SIZE ? 0 : numeric_limits<float>::quiet_NaN();

    if (df.m_function == QUANTILE)
        df.m_sp.reset();
}

template <class T>
int EMRTrack::read_datum(void *mem, uint64_t &pos, uint64_t size, T &t, const char *trackname)
{
    if (pos + sizeof(t) > size)
        TGLError<EMRTrack>(FILE_ERROR, "Invalid format of a track %s", trackname);
    memcpy(&t, (char *)mem + pos, sizeof(t));
    pos += sizeof(t);
    return sizeof(t);
}

template <class T>
void EMRTrack::calc_vals(DataFetcher &df, const EMRInterval &interv, const T &srec, const T &erec)
{
    switch (df.m_function) {
    case VALUE:
        {
            df.m_val = numeric_limits<double>::quiet_NaN();

            for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT) &&
                    (df.m_vals2compare.empty() || df.m_vals2compare.find((float)irec->v()) != df.m_vals2compare.end()))
                {
                    if (std::isnan(df.m_val))
                        df.m_val = irec->v();
                    else if (df.m_val != irec->v()) {
                        df.m_val = -1;
                        break;
                    }
                }
            }
        }
        break;
    case EXISTS:
        {
            df.m_val = 0;
            for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT) &&
                    (df.m_vals2compare.empty() || df.m_vals2compare.find((float)irec->v()) != df.m_vals2compare.end()))
                {
                    df.m_val = 1;
                    break;
                }
            }
        }
        break;
    case FREQUENT:
        {
            df.m_frequent_vals.clear();

            for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT) &&
                    (df.m_vals2compare.empty() || df.m_vals2compare.find((float)irec->v()) != df.m_vals2compare.end()))
                    df.m_frequent_vals.push_back(irec->v());
            }

            sort(df.m_frequent_vals.begin(), df.m_frequent_vals.end());
            if (df.m_frequent_vals.empty())
                df.m_val = numeric_limits<double>::quiet_NaN();
            else {
                int max_frequency = 1;
                int frequency = 1;
                df.m_val = df.m_frequent_vals.front();

                for (vector<double>::const_iterator ival = df.m_frequent_vals.begin() + 1; ival < df.m_frequent_vals.end(); ++ival) {
                    if (*ival == *(ival - 1)) {
                        ++frequency;
                        if (frequency == max_frequency)
                            df.m_val = -1;
                        else if (frequency > max_frequency)
                            df.m_val = *ival;
                    } else {
                        if (frequency > max_frequency)
                            max_frequency = frequency;
                        else if (max_frequency == 1)
                            df.m_val = -1;
                        frequency = 1;
                    }
                }
            }
        }
        break;
    case SAMPLE:
        {
            unsigned num_vs = 0;

            for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT) &&
                    (df.m_vals2compare.empty() || df.m_vals2compare.find((float)irec->v()) != df.m_vals2compare.end()))
                    ++num_vs;
            }

            if (num_vs) {
                unsigned cnt = unif_rand() * num_vs;

                for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                    if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT) &&
                        (df.m_vals2compare.empty() || df.m_vals2compare.find((float)irec->v()) != df.m_vals2compare.end()))
                    {
                        if (!cnt) {
                            df.m_val = irec->v();
                            break;
                        }
                        --cnt;
                    }
                }
            } else
                df.m_val = numeric_limits<double>::quiet_NaN();
        }
        break;
    case SAMPLE_TIME:
        {
            unsigned num_vs = 0;

            for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT) &&
                    (df.m_vals2compare.empty() || df.m_vals2compare.find((float)irec->v()) != df.m_vals2compare.end()))
                    ++num_vs;
            }

            if (num_vs) {
                unsigned cnt = unif_rand() * num_vs;

                for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                    if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT) &&
                        (df.m_vals2compare.empty() || df.m_vals2compare.find((float)irec->v()) != df.m_vals2compare.end()))
                    {
                        if (!cnt) {
                            df.m_val = irec->time().hour();
                            break;
                        }
                        --cnt;
                    }
                }
            } else
                df.m_val = numeric_limits<double>::quiet_NaN();
        }
        break;
    case AVG:
        {
            unsigned num_vs = 0;
            double sum = 0;

            for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT)) {
                    sum += irec->v();
                    ++num_vs;
                }
            }
            df.m_val = num_vs > 0 ? sum / num_vs : numeric_limits<double>::quiet_NaN();
        }
        break;
    case SIZE:
        {
            df.m_val = 0;

            for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT) &&
                    (df.m_vals2compare.empty() || df.m_vals2compare.find((float)irec->v()) != df.m_vals2compare.end()))
                    ++df.m_val;
            }
        }
        break;
    case SUM:
        {
            unsigned num_vs = 0;

            df.m_val = 0;
            for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT)) {
                    df.m_val += irec->v();
                    ++num_vs;
                }
            }
            if (!num_vs)
                df.m_val = numeric_limits<double>::quiet_NaN();
        }
        break;
    case MIN:
        {
            unsigned num_vs = 0;

            df.m_val = numeric_limits<double>::max();
            for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT)) {
                    df.m_val = min(df.m_val, irec->v());
                    ++num_vs;
                }
            }
            if (!num_vs)
                df.m_val = numeric_limits<double>::quiet_NaN();
        }
        break;
    case MAX:
        {
            unsigned num_vs = 0;

            df.m_val = -numeric_limits<double>::max();
            for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT)) {
                    df.m_val = max(df.m_val, irec->v());
                    ++num_vs;
                }
            }
            if (!num_vs)
                df.m_val = numeric_limits<double>::quiet_NaN();
        }
        break;
    case STDDEV:
        {
            unsigned num_vs = 0;
            double sum = 0;
            double mean_square_sum = 0;

            for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT)) {
                    sum += irec->v();
                    mean_square_sum += irec->v() * irec->v();
                    ++num_vs;
                }
            }

            if (num_vs > 1) {
                double avg = sum / num_vs;
                df.m_val = sqrt(mean_square_sum / (num_vs - 1) - (avg * avg) * (num_vs / (num_vs - 1)));
            }
            else
                df.m_val = numeric_limits<double>::quiet_NaN();
        }
        break;
    case PV_UPPER:
        {
            unsigned num_vs = 0;
            double sum = 0;

            for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT)) {
                    sum += percentile_upper(irec);
                    ++num_vs;
                }
            }
            df.m_val = num_vs > 0 ? sum / num_vs : numeric_limits<double>::quiet_NaN();
        }
        break;
    case PV_MIN_UPPER:
        {
            unsigned num_vs = 0;

            df.m_val = numeric_limits<double>::max();
            for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT)) {
                    df.m_val = min(df.m_val, (double)percentile_upper(irec));
                    ++num_vs;
                }
            }
            if (!num_vs)
                df.m_val = numeric_limits<double>::quiet_NaN();
        }
        break;
    case PV_MAX_UPPER:
        {
            unsigned num_vs = 0;

            df.m_val = -numeric_limits<double>::max();
            for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT)) {
                    df.m_val = max(df.m_val, (double)percentile_upper(irec));
                    ++num_vs;
                }
            }
            if (!num_vs)
                df.m_val = numeric_limits<double>::quiet_NaN();
        }
        break;
    case PV_LOWER:
        {
            unsigned num_vs = 0;
            double sum = 0;

            for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT)) {
                    sum += percentile_lower(irec);
                    ++num_vs;
                }
            }
            df.m_val = num_vs > 0 ? sum / num_vs : numeric_limits<double>::quiet_NaN();
        }
        break;
    case PV_MIN_LOWER:
        {
            unsigned num_vs = 0;

            df.m_val = numeric_limits<double>::max();
            for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT)) {
                    df.m_val = min(df.m_val, (double)percentile_lower(irec));
                    ++num_vs;
                }
            }
            if (!num_vs)
                df.m_val = numeric_limits<double>::quiet_NaN();
        }
        break;
    case PV_MAX_LOWER:
        {
            unsigned num_vs = 0;

            df.m_val = -numeric_limits<double>::max();
            for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT)) {
                    df.m_val = max(df.m_val, (double)percentile_lower(irec));
                    ++num_vs;
                }
            }
            if (!num_vs)
                df.m_val = numeric_limits<double>::quiet_NaN();
        }
        break;
    case QUANTILE:
        {
            df.m_sp.reset();
            for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT))
                    df.m_sp.add(irec->v(), unif_rand);
            }
        }
        break;
    case LM_SLOPE:
    case LM_INTERCEPT:
        {
            unsigned num_vs = 0;
            double x_sum = 0;
            double y_sum = 0;
            double xy_sum = 0;
            double x2_sum = 0;

            for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT)) {
                    double hour = irec->time().hour();
                    x_sum += hour;
                    y_sum += irec->v();
                    xy_sum += hour * irec->v();
                    x2_sum += hour * hour;
                    ++num_vs;
                }
            }

            if (num_vs > 1) {
                double x_avg = x_sum / num_vs;
                double y_avg = y_sum / num_vs;
                df.m_val = (xy_sum - x_sum * y_avg) / (x2_sum - x_sum * x_avg);
                if (df.m_function == LM_INTERCEPT)
                    df.m_val = y_avg - df.m_val * x_avg;
            } else
                df.m_val = numeric_limits<double>::quiet_NaN();
        }
        break;
    case EARLIEST:
        {
            if (is_categorical()) {
                unsigned hour = 0;
                df.m_val = numeric_limits<double>::quiet_NaN();

                for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                    if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT) &&
                        (df.m_vals2compare.empty() || df.m_vals2compare.find((float)irec->v()) != df.m_vals2compare.end()))
                    {
                        if (std::isnan(df.m_val))
                            df.m_val = irec->v();
                        else {
                            if (irec->time().hour() != hour)
                                break;
                            if (df.m_val != irec->v()) {
                                df.m_val = -1;
                                break;
                            }
                        }
                        hour = irec->time().hour();
                    }
                }
            } else {
                unsigned hour = 0;
                unsigned num_vs = 0;
                double sum = 0;

                for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                    if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT)) {
                        if (num_vs && irec->time().hour() != hour)
                            break;
                        hour = irec->time().hour();
                        sum += irec->v();
                        ++num_vs;
                    }
                }
                df.m_val = num_vs ? sum / num_vs : numeric_limits<double>::quiet_NaN();
            }
        }
        break;
    case LATEST:
        {
            if (is_categorical()) {
                unsigned hour = 0;
                df.m_val = numeric_limits<double>::quiet_NaN();

                for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                    if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT) &&
                        (df.m_vals2compare.empty() || df.m_vals2compare.find((float)irec->v()) != df.m_vals2compare.end()))
                    {
                        if (irec->time().hour() != hour)
                            df.m_val = numeric_limits<double>::quiet_NaN();
                        if (std::isnan(df.m_val))
                            df.m_val = irec->v();
                        else if (df.m_val != irec->v())
                            df.m_val = -1;
                        hour = irec->time().hour();
                    }
                }
            } else {
                unsigned hour = 0;
                unsigned num_vs = 0;
                double sum = 0;

                for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                    if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT)) {
                        if (num_vs && irec->time().hour() != hour) {
                            sum = 0;
                            num_vs = 0;
                        }
                        hour = irec->time().hour();
                        sum += irec->v();
                        ++num_vs;
                    }
                }
                df.m_val = num_vs ? sum / num_vs : numeric_limits<double>::quiet_NaN();
            }
        }
        break;
    case CLOSEST:
        {
            if (is_categorical()) {
                double mid_time = (interv.stime + interv.etime) / 2.;
                double min_distance = numeric_limits<double>::max();
                df.m_val = numeric_limits<double>::quiet_NaN();

                for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                    if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT) &&
                        (df.m_vals2compare.empty() || df.m_vals2compare.find((float)irec->v()) != df.m_vals2compare.end()))
                    {
                        double distance = fabs(mid_time - irec->time().hour());

                        if (min_distance < distance)
                            break;

                        if (min_distance > distance) {
                            df.m_val = numeric_limits<double>::quiet_NaN();
                            min_distance = distance;
                        }

                        if (std::isnan(df.m_val))
                            df.m_val = irec->v();
                        else if (df.m_val != irec->v())
                            df.m_val = -1;
                    }
                }
            } else {
                double mid_time = (interv.stime + interv.etime) / 2.;
                double min_distance = numeric_limits<double>::max();
                unsigned num_vs = 0;
                double sum = 0;

                for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                    if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT)) {
                        double distance = fabs(mid_time - irec->time().hour());

                        if (min_distance < distance)
                            break;

                        if (min_distance > distance) {
                            num_vs = 0;
                            sum = 0;
                            min_distance = distance;
                        }
                        sum += irec->v();
                        ++num_vs;
                    }
                }
                df.m_val = num_vs ? sum / num_vs : numeric_limits<double>::quiet_NaN();
            }
        }
        break;
    case EARLIEST_TIME:
        {
            df.m_val = numeric_limits<double>::quiet_NaN();

            for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT) &&
                    (df.m_vals2compare.empty() || df.m_vals2compare.find((float)irec->v()) != df.m_vals2compare.end()))
                {
                    df.m_val = irec->time().hour();
                    break;
                }
            }
        }
        break;
    case LATEST_TIME:
        {
            df.m_val = numeric_limits<double>::quiet_NaN();

            for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT) &&
                    (df.m_vals2compare.empty() || df.m_vals2compare.find((float)irec->v()) != df.m_vals2compare.end()))
                    df.m_val = irec->time().hour();
            }
        }
        break;
    case CLOSEST_EARLIER_TIME:
        {
            double mid_time = (interv.stime + interv.etime) / 2.;
            double min_distance = numeric_limits<double>::max();

            df.m_val = numeric_limits<double>::quiet_NaN();

            for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT) &&
                    (df.m_vals2compare.empty() || df.m_vals2compare.find((float)irec->v()) != df.m_vals2compare.end()))
                {
                    EMRTimeStamp::Hour hour = irec->time().hour();
                    double distance = fabs(mid_time - hour);

                    if (min_distance < distance)
                        break;

                    if (min_distance > distance) {
                        df.m_val = hour;
                        min_distance = distance;
                    }
                }
            }
        }
        break;
    case CLOSEST_LATER_TIME:
        {
            double mid_time = (interv.stime + interv.etime) / 2.;
            double min_distance = numeric_limits<double>::max();

            df.m_val = numeric_limits<double>::quiet_NaN();

            for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT) &&
                    (df.m_vals2compare.empty() || df.m_vals2compare.find((float)irec->v()) != df.m_vals2compare.end())) {
                    EMRTimeStamp::Hour hour = irec->time().hour();
                    double distance = fabs(mid_time - hour);

                    if (min_distance < distance)
                        break;

                    if (min_distance >= distance) {
                        df.m_val = hour;
                        min_distance = distance;
                    }
                }
            }
        }
        break;
    case DT1_EARLIEST:
        {
            df.m_val = numeric_limits<double>::quiet_NaN();

            for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT) &&
                    (df.m_vals2compare.empty() || df.m_vals2compare.find((float)irec->v()) != df.m_vals2compare.end()))
                {
                    df.m_val = irec->time().hour() - interv.stime;
                    break;
                }
            }
        }
        break;
    case DT1_LATEST:
        {
            df.m_val = numeric_limits<double>::quiet_NaN();

            for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT) &&
                    (df.m_vals2compare.empty() || df.m_vals2compare.find((float)irec->v()) != df.m_vals2compare.end()))
                    df.m_val = irec->time().hour() - interv.stime;
            }
        }
        break;
    case DT2_EARLIEST:
        {
            df.m_val = numeric_limits<double>::quiet_NaN();

            for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT) &&
                    (df.m_vals2compare.empty() || df.m_vals2compare.find((float)irec->v()) != df.m_vals2compare.end()))
                {
                    df.m_val = interv.etime - irec->time().hour();
                    break;
                }
            }
        }
        break;
    case DT2_LATEST:
        {
            df.m_val = numeric_limits<double>::quiet_NaN();

            for (T irec = srec; irec != erec && (int)irec->time().hour() <= interv.etime; ++irec) {
                if (!std::isnan(irec->v()) && (irec->time().refcount() == interv.refcount || interv.refcount == EMRTimeStamp::NA_REFCOUNT) &&
                    (df.m_vals2compare.empty() || df.m_vals2compare.find((float)irec->v()) != df.m_vals2compare.end()))
                    df.m_val = interv.etime - irec->time().hour();
            }
        }
        break;
    default:
        break;
    }
}

#endif

