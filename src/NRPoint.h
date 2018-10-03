#ifndef NRPOINT_H_INCLUDED
#define NRPOINT_H_INCLUDED

#include "EMRPoint.h"

struct NRPoint {
    enum Errors { BAD_FORMAT, BAD_VALUE };
    enum { ID, TIME, REF, NUM_COLS };

    static const char *COL_NAMES[NUM_COLS];

    static SEXP create_rpoints_skeleton(size_t size, unsigned num_cols = NUM_COLS, bool null_if_empty = true);

    // use non NULL sorted_ppoints argument if you wish to sort the points; sorted_ppoints will be then constructed and filled with sorted pointers to points
    static SEXP convert_points(const vector<EMRPoint> &points, unsigned num_cols = NUM_COLS, bool null_if_empty = true, bool do_sort = false,
                               vector<EMRPoint *> *sorted_ppoints = NULL);

    static SEXP convert_ids(const vector<unsigned> &ids, unsigned num_cols = 1, bool null_if_empty = true);

    static void convert_rpoints(SEXP rpoints, vector<EMRPoint> *points, const char *error_msg_prefix = "");

    static void convert_rids(SEXP rids, vector<unsigned> *ids, const char *error_msg_prefix = "");
};

#endif

