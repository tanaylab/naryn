#include <cmath>

#include "EMRTimeInterval.h"
#include "naryn.h"

void EMRTimeIntervals::sort_and_unify_overlaps(unsigned stime, unsigned etime)
{
    if (empty())
        return;

    for (vector<EMRTimeInterval>::iterator iinterv = begin(); iinterv < end(); ) {
        if (iinterv->stime > iinterv->etime)
            verror("Start time (%d) exceeds end time (%d) at time intervals, row %d", stime, etime, iinterv - begin() + 1);

        // whole interval lays outside of scope => remove it
        if (iinterv->etime < stime || iinterv->stime > etime) {
            if (iinterv != end() - 1)
                *iinterv = back();
            pop_back();
            continue;
        }

        iinterv->stime = max(iinterv->stime, stime);
        iinterv->etime = min(iinterv->etime, etime);
        ++iinterv;
    }

    sort(begin(), end());

    uint64_t cur_idx = 0;

    for (uint64_t i = 1; i < size(); i++) {
        if (at(cur_idx).etime < at(i).stime)
            at(++cur_idx) = at(i);
        // unite overlapping intervals
        else if (at(cur_idx).etime < at(i).etime)
            at(cur_idx).etime = at(i).etime;
    }
    erase(begin() + cur_idx + 1, end());
}

