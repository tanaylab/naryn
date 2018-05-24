#include <cmath>

#include "naryn.h"
#include "NRTimeInterval.h"

const char *NRTimeIntervals::COL_NAMES[NUM_COLS] = { "stime", "etime" };

void NRTimeIntervals::sort_and_unify_overlaps(unsigned stime, unsigned etime)
{
    if (empty())
        return;

    for (vector<NRTimeInterval>::iterator iinterv = begin(); iinterv < end(); ) {
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

    size_t cur_idx = 0;

    for (size_t i = 1; i < size(); i++) {
        if (at(cur_idx).etime < at(i).stime)
            at(++cur_idx) = at(i);
        // unite overlapping intervals
        else if (at(cur_idx).etime < at(i).etime)
            at(cur_idx).etime = at(i).etime;
    }
    erase(begin() + cur_idx + 1, end());
}

void NRTimeIntervals::convert_rtime_intervals(SEXP rintervs, NRTimeIntervals *intervs, const char *error_msg_prefix)
{
    intervs->clear();

    if (TYPEOF(rintervs) == PROMSXP) {
        if (PRENV(rintervs) == R_NilValue)
            rintervs = PRVALUE(rintervs);
        else
            rintervs = eval_in_R(PRCODE(rintervs), PRENV(rintervs));
    }

    if (!isVector(rintervs))
        TGLError<NRTimeIntervals>(BAD_FORMAT, "%sInvalid format of time intervals", error_msg_prefix);

    SEXP colnames = getAttrib(rintervs, R_NamesSymbol);

    if (!isString(colnames) || Rf_length(colnames) < NUM_COLS)
        TGLError<NRTimeIntervals>(BAD_FORMAT, "%sInvalid format of time intervals", error_msg_prefix);

    for (unsigned i = 0; i < NUM_COLS; i++) {
        if (strcmp(CHAR(STRING_ELT(colnames, i)), COL_NAMES[i]))
            TGLError<NRTimeIntervals>(BAD_FORMAT, "%sInvalid format of time intervals", error_msg_prefix);
    }

    SEXP rstimes = VECTOR_ELT(rintervs, STIME_COL);
    SEXP retimes = VECTOR_ELT(rintervs, ETIME_COL);
    unsigned num_intervs = (unsigned)Rf_length(rstimes);

    for (unsigned i = 1; i < NUM_COLS; i++) {
        if (Rf_length(VECTOR_ELT(rintervs, i)) != Rf_length(VECTOR_ELT(rintervs, i - 1)))
            TGLError<NRTimeIntervals>(BAD_FORMAT, "%sInvalid format of time intervals", error_msg_prefix);
    }

    if (!isReal(rstimes) && !isInteger(rstimes) || !isReal(retimes) && !isInteger(retimes))
        TGLError<NRTimeIntervals>(BAD_FORMAT, "%sInvalid format of time intervals", error_msg_prefix);

    for (unsigned i = 0; i < num_intervs; i++) {
        if (isReal(rstimes) && std::isnan(REAL(rstimes)[i]) || isReal(retimes) && std::isnan(REAL(retimes)[i]))
            TGLError<NRTimeIntervals>(BAD_VALUE, "%sInvalid format of time intervals, row %d", error_msg_prefix, i + 1);

        int stime = isReal(rstimes) ? REAL(rstimes)[i] : INTEGER(rstimes)[i];
        int etime = isReal(retimes) ? REAL(retimes)[i] : INTEGER(retimes)[i];

        if (stime < 0)
            TGLError<NRTimeIntervals>(BAD_VALUE, "%sInvalid start time (%d) at time intervals, row %d", error_msg_prefix, stime, i + 1);

        if (etime < 0)
            TGLError<NRTimeIntervals>(BAD_VALUE, "%sInvalid end time (%d) at time intervals, row %d", error_msg_prefix, etime, i + 1);

        if (stime > etime)
            TGLError<NRTimeIntervals>(BAD_VALUE, "%sStart time (%d) exceeds end time (%d) at time intervals, row %d", error_msg_prefix, stime, etime, i + 1);

        intervs->push_back(NRTimeInterval((NRTimeStamp::Hour)stime, (NRTimeStamp::Hour)etime));
    }
}

