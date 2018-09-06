#ifndef NRIDTIMEINTERVAL_H_INCLUDED
#define NRIDTIMEINTERVAL_H_INCLUDED

#include <vector>

#include <R.h>
#include <Rinternals.h>

#ifdef length
#undef length
#endif
#ifdef error
#undef error
#endif

#include "NRTimeInterval.h"

using namespace std;

//------------------------------ NRIdTimeInterval ----------------------------------------

class NRIdTimeInterval {
public:
    unsigned       id;
    NRTimeInterval tinterv;

    NRIdTimeInterval() : id((unsigned)-1) {}
    template <class... TIArgs> NRIdTimeInterval(unsigned _id, TIArgs &&..._t) : id(_id), tinterv(std::forward<TIArgs>(_t)...) {}

    bool operator==(const NRIdTimeInterval &o) const { return id == o.id && tinterv == o.tinterv; }
    bool operator<(const NRIdTimeInterval &o) const { return id < o.id || (id == o.id && tinterv < o.tinterv); }

    bool do_overlap(unsigned _id, unsigned _time) const { return id == _id && tinterv.do_overlap(_time); }

    string tostr() const;
};


//------------------------------ NRIdTimeIntervals ---------------------------------------

class NRIdTimeIntervals : public std::vector<NRIdTimeInterval> {
public:
    enum Errors { BAD_FORMAT, BAD_VALUE };
    enum { ID_COL, STIME_COL, ETIME_COL, NUM_COLS };

    static const char *COL_NAMES[NUM_COLS];

    void sort_and_unify_overlaps(unsigned stime, unsigned etime);

    // returns the interval that matches the id and overlaps the time; if no overlap is found then the latest interval that preceeds the id/time is returned or end()
    const_iterator lower_bound(unsigned id, unsigned time);

    static void convert_rid_time_intervals(SEXP rintervs, NRIdTimeIntervals *intervs, const char *error_msg_prefix = "");
};


//------------------------------ IMPLEMENTATION -----------------------------------------

inline string NRIdTimeInterval::tostr() const
{
	char buf[200];
	sprintf(buf, "(%u, %s)", id, tinterv.tostr().c_str());
	return buf;
}

inline NRIdTimeIntervals::const_iterator NRIdTimeIntervals::lower_bound(unsigned id, unsigned time)
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

