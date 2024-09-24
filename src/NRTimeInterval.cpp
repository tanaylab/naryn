#include "naryn.h"
#include "NRTimeInterval.h"

const char *NRTimeIntervals::COL_NAMES[NUM_COLS] = { "stime", "etime" };

void NRTimeIntervals::convert_rtime_intervals(SEXP rintervs, EMRTimeIntervals *intervs, const char *error_msg_prefix)
{
    intervs->clear();

    if (TYPEOF(rintervs) == PROMSXP) {
        if (PRENV(rintervs) == R_NilValue)
            rintervs = PRVALUE(rintervs);
        else
            rintervs = eval_in_R(PRCODE(rintervs), PRENV(rintervs));
    }

    if (!Rf_isVector(rintervs))
        TGLError<NRTimeIntervals>(BAD_FORMAT, "%sInvalid format of time intervals", error_msg_prefix);

    SEXP colnames = Rf_getAttrib(rintervs, R_NamesSymbol);

    if (!Rf_isString(colnames) || Rf_length(colnames) < NUM_COLS)
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

    if ((!Rf_isReal(rstimes) && !Rf_isInteger(rstimes)) || (!Rf_isReal(retimes) && !Rf_isInteger(retimes))){
        TGLError<NRTimeIntervals>(BAD_FORMAT, "%sInvalid format of time intervals", error_msg_prefix);
    }

    for (unsigned i = 0; i < num_intervs; i++) {
        if ((Rf_isReal(rstimes) && std::isnan(REAL(rstimes)[i])) || (Rf_isReal(retimes) && std::isnan(REAL(retimes)[i]))){
            TGLError<NRTimeIntervals>(BAD_VALUE, "%sInvalid format of time intervals, row %d", error_msg_prefix, i + 1);
        }

        int stime = Rf_isReal(rstimes) ? REAL(rstimes)[i] : INTEGER(rstimes)[i];
        int etime = Rf_isReal(retimes) ? REAL(retimes)[i] : INTEGER(retimes)[i];

        if (stime < 0)
            TGLError<NRTimeIntervals>(BAD_VALUE, "%sInvalid start time (%d) at time intervals, row %d", error_msg_prefix, stime, i + 1);

        if (etime < 0)
            TGLError<NRTimeIntervals>(BAD_VALUE, "%sInvalid end time (%d) at time intervals, row %d", error_msg_prefix, etime, i + 1);

        if (stime > etime)
            TGLError<NRTimeIntervals>(BAD_VALUE, "%sStart time (%d) exceeds end time (%d) at time intervals, row %d", error_msg_prefix, stime, etime, i + 1);

        intervs->push_back(EMRTimeInterval((EMRTimeStamp::Hour)stime, (EMRTimeStamp::Hour)etime));
    }
}

