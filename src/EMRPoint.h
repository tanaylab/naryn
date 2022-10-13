#ifndef EMRPOINT_H_INCLUDED
#define EMRPOINT_H_INCLUDED

#include "EMR.h"
#include "EMRTimeStamp.h"

class EMRPoint {
public:
	EMRPoint() : id((unsigned)-1) {}
	EMRPoint(unsigned _id, const EMRTimeStamp &_timestamp) { init(_id, _timestamp); }
    EMRPoint(unsigned _id, EMRTimeStamp &_timestamp) { init(_id, _timestamp); }
    template <class... TSArgs> EMRPoint(unsigned _id, TSArgs &&..._t) { init(_id, std::forward<TSArgs>(_t)...); }     // TSArgs: arguments for EMRTimeStamp::init()

    bool operator==(const EMRPoint &o) const { return id == o.id && timestamp == o.timestamp; }
    bool operator<(const EMRPoint &o) const { return id < o.id || (id == o.id && timestamp < o.timestamp); }

    // returns true if the points are equal or they differ only by reference when one of the references is NA_REFCOUNT
    bool match(const EMRPoint &o) const;

	void init(unsigned _id, const EMRTimeStamp &_timestamp);
    void init(unsigned _id, EMRTimeStamp &_timestamp) { init(_id, (const EMRTimeStamp &)_timestamp); }
    void init(unsigned _id, EMRTimeStamp &&_timestamp);
    template <class... TSArgs> void init(unsigned _id, TSArgs &&..._t);  // TSArgs: arguments for EMRTimeStamp::init()

    void pack(void *buf) const;
    void unpack(const void *buf);

    static uint64_t packed_size() { return sizeof(unsigned) + EMRTimeStamp::packed_size(); }

	string tostr() const;

	unsigned      id;          // patient id
	EMRTimeStamp   timestamp;   // start time in hours
};

typedef vector<EMRPoint> EMRPoints;


//------------------------------ IMPLEMENTATION ----------------------------------------

inline bool EMRPoint::match(const EMRPoint &o) const
{
    return id == o.id && timestamp.hour() == o.timestamp.hour() &&
        (timestamp.refcount() == o.timestamp.refcount() || timestamp.refcount() == EMRTimeStamp::NA_REFCOUNT || o.timestamp.refcount() == EMRTimeStamp::NA_REFCOUNT);
}

inline void EMRPoint::init(unsigned _id, const EMRTimeStamp &_timestamp)
{
	id = _id;
	timestamp = _timestamp;
}

inline void EMRPoint::init(unsigned _id, EMRTimeStamp &&_timestamp)
{
	id = _id;
	timestamp = std::move(_timestamp);
}

template <class... TSArgs> 
void EMRPoint::init(unsigned _id, TSArgs &&..._t)
{
	id = _id;
    timestamp.init(std::forward<TSArgs>(_t)...);
}

inline string EMRPoint::tostr() const
{
	char buf[200];
	sprintf(buf, "id %d, %s", id, timestamp.tostr().c_str());
	return buf;
}

inline void EMRPoint::pack(void *buf) const
{
    *(unsigned *)buf = id;
    timestamp.pack((char *)buf + sizeof(unsigned));
}

inline void EMRPoint::unpack(const void *buf)
{
    id = *(unsigned *)buf;
    timestamp.unpack((char *)buf + sizeof(unsigned));
}

#endif

