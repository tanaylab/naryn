#ifndef NRPOINT_H_INCLUDED
#define NRPOINT_H_INCLUDED

#include "EMRPoint.h"
#include "EMRTrack.h"

struct NRPoint {
    enum Errors { BAD_FORMAT, BAD_VALUE };
    enum { ID, NUM_ID_COLS,
           TIME = NUM_ID_COLS, REF, NUM_POINT_COLS,
           VALUE = NUM_POINT_COLS, NUM_PVAL_COLS };

    static const char *COL_NAMES[NUM_PVAL_COLS];

    // use non NULL sorted_ppoints argument if you wish to sort the points; sorted_ppoints will be then constructed and filled with sorted pointers to points
    static SEXP convert_points(const vector<EMRPoint> &points, unsigned num_cols = NUM_POINT_COLS, bool null_if_empty = true, bool do_sort = false,
                               vector<EMRPoint *> *sorted_ppoints = NULL);

    static SEXP convert_ids(const vector<unsigned> &ids, unsigned num_cols = 1, bool null_if_empty = true);

    static void convert_rpoints(SEXP rpoints, vector<EMRPoint> *points, const char *error_msg_prefix = "");

    static void convert_rids(SEXP rids, vector<unsigned> *ids, const char *error_msg_prefix = "");

    template<class T> static void convert_rpoints_vals(SEXP rsrc, EMRTrackData<T> &data, const char *error_msg_prefix = "");
};


//---------------------------------- IMPLEMENTATION -------------------------------------------

template<class T>
void NRPoint::convert_rpoints_vals(SEXP rsrc, EMRTrackData<T> &data, const char *error_msg_prefix)
{
    if (TYPEOF(rsrc) == PROMSXP) {
        if (PRENV(rsrc) == R_NilValue)
            rsrc = PRVALUE(rsrc);
        else
            rsrc = eval_in_R(PRCODE(rsrc), PRENV(rsrc));
    }

    SEXP colnames = getAttrib(rsrc, R_NamesSymbol);

    if (!isVector(rsrc) || !isString(colnames) || Rf_length(colnames) < NUM_PVAL_COLS - 1)
        TGLError<NRPoint>(BAD_FORMAT, "%sInvalid format", error_msg_prefix);

    bool ref_used = Rf_length(colnames) > REF && !strcmp(CHAR(STRING_ELT(colnames, REF)), COL_NAMES[REF]);
    SEXP rcol[NUM_PVAL_COLS];

    if ((ref_used && Rf_length(colnames) < NUM_PVAL_COLS) || (!ref_used && Rf_length(colnames) < NUM_PVAL_COLS - 1))
        TGLError<NRPoint>(BAD_FORMAT, "%sInvalid format", error_msg_prefix);

    for (unsigned i = 0, rcolidx = 0; i < NUM_PVAL_COLS; i++) {
        if (i == REF && !ref_used) {
            rcol[REF] = R_NilValue;
            continue;
        }

        rcol[i] = VECTOR_ELT(rsrc, rcolidx);

        if (strcmp(CHAR(STRING_ELT(colnames, rcolidx)), COL_NAMES[i]) || !isReal(rcol[i]) && !isInteger(rcol[i]) ||
            rcolidx && Rf_length(VECTOR_ELT(rsrc, rcolidx - 1)) != Rf_length(rcol[i]))
            TGLError<NRPoint>(BAD_FORMAT, "%sInvalid format", error_msg_prefix);

        ++rcolidx;
    }

    unsigned num_points = (unsigned)Rf_length(rcol[ID]);

    for (unsigned i = 0; i < num_points; ++i) {
        unsigned id = (unsigned)(isReal(rcol[ID]) ? REAL(rcol[ID])[i] : INTEGER(rcol[ID])[i]);
        EMRTimeStamp::Hour hour = (EMRTimeStamp::Hour)(isReal(rcol[TIME]) ? REAL(rcol[TIME])[i] : INTEGER(rcol[TIME])[i]);
        EMRTimeStamp::Refcount ref = ref_used ? (EMRTimeStamp::Refcount)(isReal(rcol[REF]) ? REAL(rcol[REF])[i] : INTEGER(rcol[REF])[i]) : EMRTimeStamp::NA_REFCOUNT;
        T val = (T)(isReal(rcol[VALUE]) ? REAL(rcol[VALUE])[i] : INTEGER(rcol[VALUE])[i]);

        data.add_data(id, EMRTimeStamp(hour, ref), val);
    }
}

#endif

