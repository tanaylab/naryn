#include <cmath>

#include "EMRPoint.h"
#include "naryn.h"
#include "NRPoint.h"

struct EMRPPointsSort {
    bool operator()(const EMRPoint *p1, const EMRPoint *p2) const { return *p1 < *p2; }
};

const char *NRPoint::COL_NAMES[NUM_COLS] = { "id", "time", "ref" };

SEXP NRPoint::create_rpoints_skeleton(size_t size, unsigned num_cols, bool null_if_empty)
{
    if (null_if_empty && !size)
        return R_NilValue;

    SEXP answer;
    SEXP row_names;
    SEXP col_names;

    rprotect(answer = RSaneAllocVector(VECSXP, num_cols));

    SET_VECTOR_ELT(answer, ID, RSaneAllocVector(INTSXP, size));
    SET_VECTOR_ELT(answer, TIME, RSaneAllocVector(INTSXP, size));
    SET_VECTOR_ELT(answer, REF, RSaneAllocVector(INTSXP, size));

    setAttrib(answer, R_NamesSymbol, (col_names = RSaneAllocVector(STRSXP, num_cols)));
    setAttrib(answer, R_ClassSymbol, mkString("data.frame"));
    setAttrib(answer, R_RowNamesSymbol, (row_names = RSaneAllocVector(INTSXP, size)));

    for (size_t i = 0; i < size; ++i)
        INTEGER(row_names)[i] = i + 1;

    for (int i = 0; i < NUM_COLS; i++)
        SET_STRING_ELT(col_names, i, mkChar(COL_NAMES[i]));

    return answer;
}

SEXP NRPoint::convert_points(const vector<EMRPoint> &points, unsigned num_cols, bool null_if_empty, bool do_sort, vector<EMRPoint *> *ppoints)
{
    if (null_if_empty && !points.size())
        return R_NilValue;

    if (ppoints) {
        bool need_sort = false;

        ppoints->clear();
        ppoints->reserve(points.size());

        if (!points.empty())
            ppoints->push_back((EMRPoint *)&points.front());

        for (vector<EMRPoint>::const_iterator ipoint = points.begin() + 1; ipoint < points.end(); ++ipoint) {
            ppoints->push_back((EMRPoint *)&*ipoint);
            need_sort = need_sort | *ipoint < *(ipoint - 1);
        }

        if (do_sort && need_sort)
            sort(ppoints->begin(), ppoints->end(), EMRPPointsSort());
        else
            ppoints = NULL;
    }

    SEXP answer = create_rpoints_skeleton(points.size(), num_cols, null_if_empty);
    SEXP ids = VECTOR_ELT(answer, ID);
    SEXP times = VECTOR_ELT(answer, TIME);
    SEXP refs = VECTOR_ELT(answer, REF);

    if (ppoints) {
        for (vector<EMRPoint *>::const_iterator ippoint = ppoints->begin(); ippoint != ppoints->end(); ++ippoint) {
            size_t index = ippoint - ppoints->begin();
            INTEGER(ids)[index] = (*ippoint)->id;
            INTEGER(times)[index] = (*ippoint)->timestamp.hour();
            INTEGER(refs)[index] = (*ippoint)->timestamp.refcount() == EMRTimeStamp::NA_REFCOUNT ? -1 : (*ippoint)->timestamp.refcount();
        }
    } else {
        for (EMRPoints::const_iterator ipoint = points.begin(); ipoint != points.end(); ++ipoint) {
            size_t index = ipoint - points.begin();
            INTEGER(ids)[index] = ipoint->id;
    		INTEGER(times)[index] = ipoint->timestamp.hour();
    		INTEGER(refs)[index] = ipoint->timestamp.refcount() == EMRTimeStamp::NA_REFCOUNT ? -1 : ipoint->timestamp.refcount();
        }
    }

    return answer;
}

void NRPoint::convert_rpoints(SEXP rpoints, vector<EMRPoint> *points, const char *error_msg_prefix)
{
    points->clear();

    if (TYPEOF(rpoints) == PROMSXP) {
        if (PRENV(rpoints) == R_NilValue)
            rpoints = PRVALUE(rpoints);
        else
            rpoints = eval_in_R(PRCODE(rpoints), PRENV(rpoints));
    }

    if (!isVector(rpoints))
        TGLError<NRPoint>(BAD_FORMAT, "%sInvalid format of id-time points", error_msg_prefix);

    SEXP colnames = getAttrib(rpoints, R_NamesSymbol);

    if (!isString(colnames) || Rf_length(colnames) < NUM_COLS - 1)
        TGLError<NRPoint>(BAD_FORMAT, "%sInvalid format of id-time points", error_msg_prefix);

    for (unsigned i = 0; i < NUM_COLS; i++) {
        if (i == REF) // reference column is optional
            continue;

        if (strcmp(CHAR(STRING_ELT(colnames, i)), COL_NAMES[i]))
            TGLError<NRPoint>(BAD_FORMAT, "%sInvalid format of id-time points", error_msg_prefix);
    }

    SEXP ids = VECTOR_ELT(rpoints, ID);
    SEXP hours = VECTOR_ELT(rpoints, TIME);
    SEXP refs = Rf_length(colnames) >= REF + 1 && !strcmp(CHAR(STRING_ELT(colnames, REF)), COL_NAMES[REF]) ? VECTOR_ELT(rpoints, REF) : R_NilValue;
    unsigned num_points = (unsigned)Rf_length(ids);

    for (unsigned i = 1; i < NUM_COLS; i++) {
        if ((i != REF || i == REF && refs != R_NilValue) && Rf_length(VECTOR_ELT(rpoints, i)) != Rf_length(VECTOR_ELT(rpoints, i - 1)))
            TGLError<NRPoint>(BAD_FORMAT, "%sInvalid format of id-time points", error_msg_prefix);
    }

    if (!isReal(ids) && !isInteger(ids) || !isReal(hours) && !isInteger(hours) || refs != R_NilValue && !isReal(refs) && !isInteger(refs))
        TGLError<NRPoint>(BAD_FORMAT, "%sInvalid format of id-time points", error_msg_prefix);

    for (unsigned i = 0; i < num_points; i++) {
        if (isReal(ids) && std::isnan(REAL(ids)[i]) || isReal(hours) && std::isnan(REAL(hours)[i]) || refs != R_NilValue && isReal(refs) && std::isnan(REAL(refs)[i]))
            TGLError<NRPoint>(BAD_VALUE, "%sInvalid format of id-time points, row %d", error_msg_prefix, i + 1);

        int id = isReal(ids) ? REAL(ids)[i] : INTEGER(ids)[i];
        int hour = isReal(hours) ? REAL(hours)[i] : INTEGER(hours)[i];
        int ref = -1;

        if (refs != R_NilValue)
            ref = isReal(refs) ? REAL(refs)[i] : INTEGER(refs)[i];

        if (isReal(ids) && REAL(ids)[i] != id)
            TGLError<NRPoint>(BAD_VALUE, "%sInvalid id at id-time points, row %d", error_msg_prefix, i + 1);

		if (isReal(hours) && REAL(hours)[i] != hour || hour < 0 || hour > EMRTimeStamp::MAX_HOUR)
            TGLError<NRPoint>(BAD_VALUE, "%sInvalid time at id-time points, row %d", error_msg_prefix, i + 1);

        if (refs != R_NilValue && isReal(refs) && REAL(refs)[i] != ref || ref < -1 || ref > EMRTimeStamp::MAX_REFCOUNT)
            TGLError<NRPoint>(BAD_VALUE, "%sInvalid reference at id-time points, row %d", error_msg_prefix, i + 1);

		points->push_back(EMRPoint(id, EMRTimeStamp((EMRTimeStamp::Hour)hour, (EMRTimeStamp::Refcount)ref)));
    }
}

SEXP NRPoint::convert_ids(const vector<unsigned> &ids, unsigned num_cols, bool null_if_empty)
{
    if (null_if_empty && !ids.size())
        return R_NilValue;

    SEXP answer;
    SEXP rids;
    SEXP row_names;
    SEXP col_names;

    rprotect(answer = RSaneAllocVector(VECSXP, num_cols));

    SET_VECTOR_ELT(answer, 0, (rids = RSaneAllocVector(INTSXP, ids.size())));

    setAttrib(answer, R_NamesSymbol, (col_names = RSaneAllocVector(STRSXP, num_cols)));
    setAttrib(answer, R_ClassSymbol, mkString("data.frame"));
    setAttrib(answer, R_RowNamesSymbol, (row_names = RSaneAllocVector(INTSXP, ids.size())));

    for (vector<unsigned>::const_iterator iid = ids.begin(); iid != ids.end(); ++iid) {
        size_t index = iid - ids.begin();
        INTEGER(rids)[index] = *iid;
        INTEGER(row_names)[index] = index + 1;
    }

    SET_STRING_ELT(col_names, 0, mkChar("id"));

    return answer;
}

void NRPoint::convert_rids(SEXP rids, vector<unsigned> *ids, const char *error_msg_prefix)
{
    ids->clear();

    if (TYPEOF(rids) == PROMSXP) {
        if (PRENV(rids) == R_NilValue)
            rids = PRVALUE(rids);
        else
            rids = eval_in_R(PRCODE(rids), PRENV(rids));
    }

    if (!isVector(rids))
        TGLError<NRPoint>(BAD_FORMAT, "%sInvalid format of ids", error_msg_prefix);

    SEXP colnames = getAttrib(rids, R_NamesSymbol);

    if (!isString(colnames) || Rf_length(colnames) < 1 || strcmp(CHAR(STRING_ELT(colnames, 0)), "id"))
        TGLError<NRPoint>(BAD_FORMAT, "%sInvalid format of ids", error_msg_prefix);

    SEXP rvids = VECTOR_ELT(rids, 0);

    if (isReal(rvids)) {
        for (int i = 0; i < Rf_length(rvids); ++i) {
            double id = REAL(rvids)[i];
            if (id < 0 || (double)(int)id != id)
                TGLError<NRPoint>(BAD_VALUE, "%sInvalid id at row %d", error_msg_prefix, i + 1);
            ids->push_back((unsigned)id);
        }
    } else if (isInteger(rvids)) {
        for (int i = 0; i < Rf_length(rvids); ++i) {
            int id = INTEGER(rvids)[i];
            if (id < 0)
                TGLError<NRPoint>(BAD_VALUE, "%sInvalid id at row %d", error_msg_prefix, i + 1);
            ids->push_back((unsigned)id);
        }
    } else
        TGLError<NRPoint>(BAD_FORMAT, "%sInvalid format of ids", error_msg_prefix);
}
