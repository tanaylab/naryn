#ifndef EMRTIMEINTERVAL_H_INCLUDED
#define EMRTIMEINTERVAL_H_INCLUDED

#include <vector>

#include "EMRTimeStamp.h"

using namespace std;

//------------------------------ EMRTimeInterval ----------------------------------------

class EMRTimeInterval {
public:
    EMRTimeStamp::Hour stime;
    EMRTimeStamp::Hour etime;

    EMRTimeInterval() : stime(EMRTimeStamp::NA_HOUR), etime(EMRTimeStamp::NA_HOUR) {}
    EMRTimeInterval(EMRTimeStamp::Hour _stime, EMRTimeStamp::Hour _etime);

    bool operator==(const EMRTimeInterval &o) const { return stime == o.stime && etime == o.etime; }
    bool operator<(const EMRTimeInterval &o) const { return stime < o.stime || (stime == o.stime && etime < o.etime); }

    bool do_overlap(unsigned time) const { return time >= stime && time <= etime; }

    string tostr() const;
};


//------------------------------ EMRTimeIntervals ---------------------------------------

class EMRTimeIntervals : public std::vector<EMRTimeInterval> {
public:
    void sort_and_unify_overlaps(unsigned stime, unsigned etime);

    // returns the interval that overlaps the time; if no overlap is found then the latest interval that preceeds the time is returned or end()
    const_iterator lower_bound(unsigned time);
};


//------------------------------ IMPLEMENTATION -----------------------------------------

inline EMRTimeInterval::EMRTimeInterval(EMRTimeStamp::Hour _stime, EMRTimeStamp::Hour _etime)
{
    if (_stime > _etime)
        TGLError("Start time (%d) exceeds end time (%d)", _stime, _etime);
    stime = _stime;
    etime = _etime;
}

inline string EMRTimeInterval::tostr() const
{
	char buf[200];
	sprintf(buf, "(%d, %d)", stime, etime);
	return buf;
}

inline EMRTimeIntervals::const_iterator EMRTimeIntervals::lower_bound(unsigned time)
{
    // run binary search
    const_iterator istart_interval = begin();
    const_iterator iend_interval = end();

    while (iend_interval - istart_interval > 1) {
        const_iterator imid_interval = istart_interval + (iend_interval - istart_interval) / 2;

        if (imid_interval->do_overlap(time))
            return imid_interval;

        // is mid_interval < interval?
        if (time < imid_interval->stime)
            iend_interval = imid_interval;
        else
            istart_interval = imid_interval;
    }

    if (iend_interval - istart_interval == 1 && time >= istart_interval->stime)
        return istart_interval;

    return end();
}

#endif

