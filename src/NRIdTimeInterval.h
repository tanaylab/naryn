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

#include "EMRIdTimeInterval.h"

using namespace std;

//------------------------------ NRIdTimeIntervals ---------------------------------------

class NRIdTimeIntervals {
public:
    enum Errors { BAD_FORMAT, BAD_VALUE };
    enum { ID_COL, STIME_COL, ETIME_COL, NUM_COLS };

    static const char *COL_NAMES[NUM_COLS];

    static void convert_rid_time_intervals(SEXP rintervs, EMRIdTimeIntervals *intervs, const char *error_msg_prefix = "");
};

#endif

