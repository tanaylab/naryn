#include <R.h>
#include <R_ext/Rdynload.h>
#include <Rinternals.h>
#include <stdlib.h>  // for NULL

extern "C" {
extern SEXP emr_covariance(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP,
                           SEXP);
extern SEXP emr_dbinit(SEXP, SEXP, SEXP, SEXP);
extern SEXP emr_dbreload(SEXP);
extern SEXP emr_dbunload(SEXP);
extern SEXP emr_db_subset(SEXP, SEXP, SEXP, SEXP);
extern SEXP emr_db_subset_ids(SEXP);
extern SEXP emr_db_subset_info(SEXP);
extern SEXP emr_track_exists(SEXP, SEXP, SEXP);
extern SEXP emr_logical_track_exists(SEXP, SEXP);
extern SEXP emr_track_names(SEXP);
extern SEXP emr_track_db_names(SEXP, SEXP);
extern SEXP emr_logical_track_names(SEXP);
extern SEXP C_emr_dist(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP,
                       SEXP);
extern SEXP C_emr_extract(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP,
                          SEXP);
extern SEXP emr_ids_dist(SEXP, SEXP, SEXP);
extern SEXP emr_ids_dist_with_iterator(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP emr_ids_vals_dist(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP emr_import(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP emr_check_named_filter(SEXP, SEXP, SEXP);
extern SEXP emr_check_filter_attr_src(SEXP, SEXP);
extern SEXP emr_check_filter_attr_time_shift(SEXP, SEXP);
extern SEXP emr_check_filter_attr_expiration(SEXP, SEXP);
extern SEXP emr_create_logical(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP emr_remove_logical(SEXP, SEXP, SEXP);
extern SEXP emr_is_logical(SEXP, SEXP);
extern SEXP emr_logical_track_info(SEXP, SEXP);
extern SEXP emr_expr_logical_tracks(SEXP, SEXP);
extern SEXP emr_expr_physical_tracks(SEXP, SEXP);
extern SEXP emr_expr_virtual_tracks(SEXP, SEXP);
extern SEXP emr_ltrack_dependencies(SEXP, SEXP);
extern SEXP emr_logical_track_user_info(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP,
                                        SEXP, SEXP, SEXP);
extern SEXP C_emr_quantiles(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP C_emr_screen(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP C_emr_summary(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP emr_test_pipe(SEXP, SEXP, SEXP);
extern SEXP emr_test_eval(SEXP, SEXP, SEXP);
extern SEXP C_emr_time2hour(SEXP, SEXP);
extern SEXP C_emr_time2dayofmonth(SEXP, SEXP);
extern SEXP C_emr_time2month(SEXP, SEXP);
extern SEXP C_emr_time2year(SEXP, SEXP);
extern SEXP C_emr_date2time(SEXP, SEXP);
extern SEXP emr_track_mv(SEXP, SEXP, SEXP, SEXP);
extern SEXP emr_track_rm(SEXP, SEXP);
extern SEXP emr_track_info(SEXP, SEXP);
extern SEXP emr_track_ids(SEXP, SEXP);
extern SEXP emr_track_unique(SEXP, SEXP);
extern SEXP emr_track_percentile(SEXP, SEXP, SEXP, SEXP);
extern SEXP emr_get_tracks_attrs(SEXP, SEXP, SEXP);
extern SEXP emr_set_track_attr(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP emr_track_dbs(SEXP, SEXP);
extern SEXP emr_track_db(SEXP, SEXP);
extern SEXP emr_track_create(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP,
                             SEXP, SEXP, SEXP);
extern SEXP C_emr_annotate(SEXP, SEXP, SEXP);
extern SEXP emr_check_vtrack(SEXP, SEXP, SEXP);
extern SEXP emr_check_vtrack_attr_src(SEXP, SEXP);
extern SEXP emr_check_vtrack_attr_func(SEXP, SEXP);
extern SEXP emr_check_vtrack_attr_time_shift(SEXP, SEXP);
extern SEXP emr_check_vtrack_attr_id_map(SEXP, SEXP);
extern SEXP emr_check_vtrack_attr_filter(SEXP, SEXP);
}

static const R_CallMethodDef CallEntries[] = {
    {"emr_covariance", (DL_FUNC) &emr_covariance, 10},
    {"emr_dbinit", (DL_FUNC) &emr_dbinit, 4},
    {"emr_dbreload", (DL_FUNC) &emr_dbreload, 1},
    {"emr_dbunload", (DL_FUNC) &emr_dbunload, 1},
    {"emr_db_subset", (DL_FUNC) &emr_db_subset, 3},
    {"emr_db_subset_ids", (DL_FUNC) &emr_db_subset_ids, 1},
    {"emr_db_subset_info", (DL_FUNC) &emr_db_subset_info, 1},
    {"emr_track_exists", (DL_FUNC) &emr_track_exists, 3},
    {"emr_logical_track_exists", (DL_FUNC) &emr_logical_track_exists, 2},
    {"emr_track_names", (DL_FUNC) &emr_track_names, 1},
    {"emr_track_db_names", (DL_FUNC) &emr_track_db_names, 2},
    {"emr_logical_track_names", (DL_FUNC) &emr_logical_track_names, 1},
    {"C_emr_dist", (DL_FUNC) &C_emr_dist, 10},
    {"C_emr_extract", (DL_FUNC) &C_emr_extract, 10},
    {"emr_ids_dist", (DL_FUNC) &emr_ids_dist, 3},
    {"emr_ids_dist_with_iterator", (DL_FUNC) &emr_ids_dist_with_iterator, 6},
    {"emr_ids_vals_dist", (DL_FUNC) &emr_ids_vals_dist, 6},
    {"emr_import", (DL_FUNC) &emr_import, 7},
    {"emr_check_named_filter", (DL_FUNC) &emr_check_named_filter, 3},
    {"emr_check_filter_attr_src", (DL_FUNC) &emr_check_filter_attr_src, 2},
    {"emr_check_filter_attr_time_shift",
     (DL_FUNC) &emr_check_filter_attr_time_shift, 2},
    {"emr_check_filter_attr_expiration",
     (DL_FUNC) &emr_check_filter_attr_expiration, 2},
    {"emr_create_logical", (DL_FUNC) &emr_create_logical, 5},
    {"emr_remove_logical", (DL_FUNC) &emr_remove_logical, 3},
    {"emr_is_logical", (DL_FUNC) &emr_is_logical, 2},
    {"emr_logical_track_info", (DL_FUNC) &emr_logical_track_info, 2},
    {"emr_expr_logical_tracks", (DL_FUNC) &emr_expr_logical_tracks, 2},
    {"emr_expr_physical_tracks", (DL_FUNC) &emr_expr_physical_tracks, 2},
    {"emr_expr_virtual_tracks", (DL_FUNC) &emr_expr_virtual_tracks, 2},
    {"emr_ltrack_dependencies", (DL_FUNC) &emr_ltrack_dependencies, 2},
    {"emr_logical_track_user_info", (DL_FUNC) &emr_logical_track_user_info, 9},
    {"C_emr_quantiles", (DL_FUNC) &C_emr_quantiles, 8},
    {"C_emr_screen", (DL_FUNC) &C_emr_screen, 8},
    {"C_emr_summary", (DL_FUNC) &C_emr_summary, 7},
    {"emr_test_pipe", (DL_FUNC) &emr_test_pipe, 3},
    {"emr_test_eval", (DL_FUNC) &emr_test_eval, 3},
    {"C_emr_time2hour", (DL_FUNC) &C_emr_time2hour, 2},
    {"C_emr_time2dayofmonth", (DL_FUNC) &C_emr_time2dayofmonth, 2},
    {"C_emr_time2month", (DL_FUNC) &C_emr_time2month, 2},
    {"C_emr_time2year", (DL_FUNC) &C_emr_time2year, 2},
    {"C_emr_date2time", (DL_FUNC) &C_emr_date2time, 2},
    {"emr_track_mv", (DL_FUNC) &emr_track_mv, 4},
    {"emr_track_rm", (DL_FUNC) &emr_track_rm, 2},
    {"emr_track_info", (DL_FUNC) &emr_track_info, 2},
    {"emr_track_ids", (DL_FUNC) &emr_track_ids, 2},
    {"emr_track_unique", (DL_FUNC) &emr_track_unique, 2},
    {"emr_track_percentile", (DL_FUNC) &emr_track_percentile, 4},
    {"emr_get_tracks_attrs", (DL_FUNC) &emr_get_tracks_attrs, 3},
    {"emr_set_track_attr", (DL_FUNC) &emr_set_track_attr, 5},
    {"emr_track_dbs", (DL_FUNC) &emr_track_dbs, 2},
    {"emr_track_db", (DL_FUNC) &emr_track_db, 2},
    {"emr_track_create", (DL_FUNC) &emr_track_create, 11},
    {"C_emr_annotate", (DL_FUNC) &C_emr_annotate, 3},
    {"emr_check_vtrack", (DL_FUNC) &emr_check_vtrack, 3},
    {"emr_check_vtrack_attr_src", (DL_FUNC) &emr_check_vtrack_attr_src, 2},
    {"emr_check_vtrack_attr_func", (DL_FUNC) &emr_check_vtrack_attr_func, 2},
    {"emr_check_vtrack_attr_time_shift",
     (DL_FUNC) &emr_check_vtrack_attr_time_shift, 2},
    {"emr_check_vtrack_attr_id_map", (DL_FUNC) &emr_check_vtrack_attr_id_map, 2},
    {"emr_check_vtrack_attr_filter", (DL_FUNC) &emr_check_vtrack_attr_filter, 2},
    {NULL, NULL, 0}};

void R_init_naryn(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
