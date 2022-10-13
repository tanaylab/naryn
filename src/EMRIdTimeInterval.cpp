#include <cmath>

#include "EMRDb.h"
#include "EMRIdTimeInterval.h"
#include "naryn.h"

void EMRIdTimeIntervals::sort_and_unify_overlaps(unsigned stime, unsigned etime)
{
    if (empty())
        return;

    for (vector<EMRIdTimeInterval>::iterator iinterv = begin(); iinterv < end(); ) {
        if (iinterv->tinterv.stime > iinterv->tinterv.etime)
            verror("Start time (%d) exceeds end time (%d) at time intervals, row %d", stime, etime, iinterv - begin() + 1);

        // whole interval lays outside of scope => remove it
        if (!g_db->id_exists(iinterv->id) || iinterv->tinterv.etime < stime || iinterv->tinterv.stime > etime) {
            if (iinterv != end() - 1)
                *iinterv = back();
            pop_back();
            continue;
        }

        iinterv->tinterv.stime = max(iinterv->tinterv.stime, stime);
        iinterv->tinterv.etime = min(iinterv->tinterv.etime, etime);
        ++iinterv;
    }

    sort(begin(), end());

    uint64_t cur_idx = 0;

    for (uint64_t i = 1; i < size(); i++) {
        if (at(cur_idx).id != at(i).id || at(cur_idx).tinterv.etime < at(i).tinterv.stime)
            at(++cur_idx) = at(i);
        // unite overlapping intervals
        else if (at(cur_idx).tinterv.etime < at(i).tinterv.etime)
            at(cur_idx).tinterv.etime = at(i).tinterv.etime;
    }
    erase(begin() + cur_idx + 1, end());
}

