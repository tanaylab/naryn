#include "EMRDb.h"
#include "naryn.h"
#include "NRIdTimeInterval.h"

const char *NRIdTimeIntervals::COL_NAMES[NUM_COLS] = { "id", "stime", "etime" };

void NRIdTimeIntervals::convert_rid_time_intervals(SEXP rintervs, EMRIdTimeIntervals *intervs, const char *error_msg_prefix)
{
    intervs->clear();

    if (TYPEOF(rintervs) == PROMSXP) {
        if (PRENV(rintervs) == R_NilValue)
            rintervs = PRVALUE(rintervs);
        else
            rintervs = eval_in_R(PRCODE(rintervs), PRENV(rintervs));
    }

    if (!isVector(rintervs))
        TGLError<NRIdTimeIntervals>(BAD_FORMAT, "%sInvalid format of ID - time intervals", error_msg_prefix);

    SEXP colnames = getAttrib(rintervs, R_NamesSymbol);

    if (!isString(colnames) || Rf_length(colnames) < NUM_COLS)
        TGLError<NRIdTimeIntervals>(BAD_FORMAT, "%sInvalid format of ID - time intervals", error_msg_prefix);

    for (unsigned i = 0; i < NUM_COLS; i++) {
        if (strcmp(CHAR(STRING_ELT(colnames, i)), COL_NAMES[i]))
            TGLError<NRIdTimeIntervals>(BAD_FORMAT, "%sInvalid format of ID - time intervals", error_msg_prefix);
    }

    SEXP rids = VECTOR_ELT(rintervs, ID_COL);
    SEXP rstimes = VECTOR_ELT(rintervs, STIME_COL);
    SEXP retimes = VECTOR_ELT(rintervs, ETIME_COL);
    unsigned num_intervs = (unsigned)Rf_length(rids);

    for (unsigned i = 1; i < NUM_COLS; i++) {
        if (Rf_length(VECTOR_ELT(rintervs, i)) != Rf_length(VECTOR_ELT(rintervs, i - 1))){
            TGLError<NRIdTimeIntervals>(BAD_FORMAT, "%sInvalid format of ID - time intervals", error_msg_prefix);
        }
    }

    if ((!isReal(rids) && !isInteger(rids)) || (!isReal(rstimes) && !isInteger(rstimes)) || (!isReal(retimes) && !isInteger(retimes))){
        TGLError<NRIdTimeIntervals>(BAD_FORMAT, "%sInvalid format of ID - time intervals", error_msg_prefix);
    }

    for (unsigned i = 0; i < num_intervs; i++) {
        if ((isReal(rids) && std::isnan(REAL(rids)[i])) || (isReal(rstimes) && std::isnan(REAL(rstimes)[i])) || (isReal(retimes) && std::isnan(REAL(retimes)[i]))){
            TGLError<NRIdTimeIntervals>(BAD_VALUE, "%sInvalid format of ID - time intervals, row %d", error_msg_prefix, i + 1);
        }

        int id = isReal(rids) ? REAL(rids)[i] : INTEGER(rids)[i];
        int stime = isReal(rstimes) ? REAL(rstimes)[i] : INTEGER(rstimes)[i];
        int etime = isReal(retimes) ? REAL(retimes)[i] : INTEGER(retimes)[i];

        if (id < 0){
            TGLError<NRIdTimeIntervals>(BAD_VALUE, "%sInvalid id (%d) at ID - time intervals, row %d", error_msg_prefix, id, i + 1);
        }

        if (stime < 0){
            TGLError<NRIdTimeIntervals>(BAD_VALUE, "%sInvalid start time (%d) at ID - time intervals, row %d", error_msg_prefix, stime, i + 1);
        }

        if (etime < 0){
            TGLError<NRIdTimeIntervals>(BAD_VALUE, "%sInvalid end time (%d) at ID - time intervals, row %d", error_msg_prefix, etime, i + 1);
        }

        if (stime > etime){
            TGLError<NRIdTimeIntervals>(BAD_VALUE, "%sStart time (%d) exceeds end time (%d) at ID - time intervals, row %d", error_msg_prefix, stime, etime, i + 1);
        }

        intervs->push_back(EMRIdTimeInterval(id, (EMRTimeStamp::Hour)stime, (EMRTimeStamp::Hour)etime));
    }
}

