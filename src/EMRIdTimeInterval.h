#ifndef EMRIDTIMEINTERVAL_H_INCLUDED
#define EMRIDTIMEINTERVAL_H_INCLUDED

#include <vector>

#include "EMRTimeInterval.h"

using namespace std;

//------------------------------ EMRIdTimeInterval ----------------------------------------

class EMRIdTimeInterval {
public:
    unsigned        id;
    EMRTimeInterval tinterv;

    EMRIdTimeInterval() : id((unsigned)-1) {}
    template <class... TIArgs> EMRIdTimeInterval(unsigned _id, TIArgs &&..._t) : id(_id), tinterv(std::forward<TIArgs>(_t)...) {}

    bool operator==(const EMRIdTimeInterval &o) const { return id == o.id && tinterv == o.tinterv; }
    bool operator<(const EMRIdTimeInterval &o) const { return id < o.id || (id == o.id && tinterv < o.tinterv); }

    bool do_overlap(unsigned _id, unsigned _time) const { return id == _id && tinterv.do_overlap(_time); }

    string tostr() const;
};


//------------------------------ EMRIdTimeIntervals ---------------------------------------

class EMRIdTimeIntervals : public std::vector<EMRIdTimeInterval> {
public:
    void sort_and_unify_overlaps(unsigned stime, unsigned etime);

    // returns the interval that matches the id and overlaps the time; if no overlap is found then the latest interval that preceeds the id/time is returned or end()
    const_iterator lower_bound(unsigned id, unsigned time);
};


//------------------------------ IMPLEMENTATION -----------------------------------------

inline string EMRIdTimeInterval::tostr() const
{
	char buf[200];
	sprintf(buf, "(%u, %s)", id, tinterv.tostr().c_str());
	return buf;
}

inline EMRIdTimeIntervals::const_iterator EMRIdTimeIntervals::lower_bound(unsigned id, unsigned time)
{
    // run binary search
    const_iterator istart_interval = begin();
    const_iterator iend_interval = end();

    while (iend_interval - istart_interval > 1) {
        const_iterator imid_interval = istart_interval + (iend_interval - istart_interval) / 2;

        if (imid_interval->do_overlap(id, time))
            return imid_interval;


        // is mid_interval < interval?
        if (id < imid_interval->id || (id == imid_interval->id && time < imid_interval->tinterv.stime))
            iend_interval = imid_interval;
        else
            istart_interval = imid_interval;
    }

    if (iend_interval - istart_interval == 1 &&
        (id > istart_interval->id || (id == istart_interval->id && time >= istart_interval->tinterv.stime)))
        return istart_interval;

    return end();
}

#endif

