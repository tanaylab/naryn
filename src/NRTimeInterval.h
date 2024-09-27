#ifndef NRTIMEINTERVAL_H_INCLUDED
#define NRTIMEINTERVAL_H_INCLUDED

#include <vector>

#ifndef R_NO_REMAP
#  define R_NO_REMAP
#endif
#include <R.h>
#include <Rinternals.h>

#ifdef length
#undef length
#endif
#ifdef error
#undef error
#endif

#include "naryn.h"
#include "EMRTimeInterval.h"
#include "EMRTimeStamp.h"

using namespace std;

//------------------------------ NRTimeIntervals ---------------------------------------

class NRTimeIntervals {
public:
    enum Errors { BAD_FORMAT, BAD_VALUE };
    enum { STIME_COL, ETIME_COL, NUM_COLS };

    static const char *COL_NAMES[NUM_COLS];

    static void convert_rtime_intervals(SEXP rintervs, EMRTimeIntervals *intervs, const char *error_msg_prefix = "");
};

#endif

