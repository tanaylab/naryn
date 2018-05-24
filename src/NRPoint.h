#ifndef NRPOINT_H_INCLUDED
#define NRPOINT_H_INCLUDED

#include <R.h>
#include <Rinternals.h>

#ifdef length
#undef length
#endif
#ifdef error
#undef error
#endif

#include "NRTimeStamp.h"

class NRPoint {
public:
    enum Errors { BAD_FORMAT, BAD_VALUE };
    enum { ID, TIME, REF, NUM_COLS };

    static const char *COL_NAMES[NUM_COLS];

	NRPoint() : id((unsigned)-1) {}
	NRPoint(unsigned _id, const NRTimeStamp &_timestamp) { init(_id, _timestamp); }
    NRPoint(unsigned _id, NRTimeStamp &_timestamp) { init(_id, _timestamp); }
    template <class... TSArgs> NRPoint(unsigned _id, TSArgs &&..._t) { init(_id, std::forward<TSArgs>(_t)...); }     // TSArgs: arguments for NRTimeStamp::init()

    bool operator==(const NRPoint &o) const { return id == o.id && timestamp == o.timestamp; }
    bool operator<(const NRPoint &o) const { return id < o.id || id == o.id && timestamp < o.timestamp; }

    // returns true if the points are equal or they differ only by reference when one of the references is NA_REFCOUNT
    bool match(const NRPoint &o) const;

	void init(unsigned _id, const NRTimeStamp &_timestamp);
    void init(unsigned _id, NRTimeStamp &_timestamp) { init(_id, (const NRTimeStamp &)_timestamp); }
    void init(unsigned _id, NRTimeStamp &&_timestamp);
    template <class... TSArgs> void init(unsigned _id, TSArgs &&..._t);  // TSArgs: arguments for NRTimeStamp::init()

    void pack(void *buf) const;
    void unpack(const void *buf);

    static size_t packed_size() { return sizeof(unsigned) + NRTimeStamp::packed_size(); }

	string tostr() const;

    static SEXP create_rpoints_skeleton(size_t size, unsigned num_cols = NUM_COLS, bool null_if_empty = true);

    // use non NULL sorted_ppoints argument if you wish to sort the points; sorted_ppoints will be then constructed and filled with sorted pointers to points
    static SEXP convert_points(const vector<NRPoint> &points, unsigned num_cols = NUM_COLS, bool null_if_empty = true, bool do_sort = false,
                               vector<NRPoint *> *sorted_ppoints = NULL);

    static SEXP convert_ids(const vector<unsigned> &ids, unsigned num_cols = 1, bool null_if_empty = true);

    static void convert_rpoints(SEXP rpoints, vector<NRPoint> *points, const char *error_msg_prefix = "");

    static void convert_rids(SEXP rids, vector<unsigned> *ids, const char *error_msg_prefix = "");

	unsigned      id;          // patient id
	NRTimeStamp   timestamp;   // start time in hours
};

typedef vector<NRPoint> NRPoints;


//------------------------------ IMPLEMENTATION ----------------------------------------

inline bool NRPoint::match(const NRPoint &o) const
{
    return id == o.id && timestamp.hour() == o.timestamp.hour() &&
        (timestamp.refcount() == o.timestamp.refcount() || timestamp.refcount() == NRTimeStamp::NA_REFCOUNT || o.timestamp.refcount() == NRTimeStamp::NA_REFCOUNT);
}

inline void NRPoint::init(unsigned _id, const NRTimeStamp &_timestamp)
{
	id = _id;
	timestamp = _timestamp;
}

inline void NRPoint::init(unsigned _id, NRTimeStamp &&_timestamp)
{
	id = _id;
	timestamp = std::move(_timestamp);
}

template <class... TSArgs> 
void NRPoint::init(unsigned _id, TSArgs &&..._t)
{
	id = _id;
    timestamp.init(std::forward<TSArgs>(_t)...);
}

inline string NRPoint::tostr() const
{
	char buf[200];
	sprintf(buf, "id %d, %s", id, timestamp.tostr().c_str());
	return buf;
}

inline void NRPoint::pack(void *buf) const
{
    *(unsigned *)buf = id;
    timestamp.pack((char *)buf + sizeof(unsigned));
}

inline void NRPoint::unpack(const void *buf)
{
    id = *(unsigned *)buf;
    timestamp.unpack((char *)buf + sizeof(unsigned));
}

#endif

