#include <cmath>
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

#include "BinsManager.h"
#include "naryn.h"
#include "NRTrackExpressionScanner.h"

struct Avg {
    double avg() const { return n ? sum / n : numeric_limits<double>::quiet_NaN(); }
    void add(double v) { sum += v; ++n; }

    double sum {0};
    int    n {0};
};

typedef vector<vector<Avg>> AvgMatrix;

extern "C" {

SEXP emr_covariance(SEXP _exprs, SEXP _breaks, SEXP _include_lowest, SEXP _right, SEXP _stime, SEXP _etime, SEXP _iterator_policy, SEXP _keepref, SEXP _filter, SEXP _envir)
{
	try {
        Naryn naryn(_envir);

        enum { N, AVG, VAR, COV, COR, NUM_STATS };

        const char *STAT_NAMES[NUM_STATS] = { "n", "e", "var", "cov", "cor" };

		if (!Rf_isString(_exprs) || Rf_length(_exprs) < 1)
			verror("Track expressions argument must be a vector of strings");

        unsigned num_exprs = (unsigned)Rf_length(_exprs);
        unsigned num_breaks_exprs = (unsigned)Rf_length(_breaks);
        unsigned num_cov_exprs = num_exprs - num_breaks_exprs;

        if (num_cov_exprs < 1)
            verror("cor.exprs list is empty");

        NRTrackExprScanner scanner;
        scanner.begin(_exprs, NRTrackExprScanner::REAL_T, _stime, _etime, _iterator_policy, _keepref, _filter);

        BinsManager bins_manager(_breaks, _include_lowest, _right, &scanner.get_track_exprs(), &scanner.get_expr_vars());

        if (bins_manager.get_num_bin_finders() != num_breaks_exprs)
            verror("Number of breaks sets must be equal to the number of tracks used");

        unsigned totalbins = bins_manager.get_total_bins();
        uint64_t num_vals = totalbins * num_cov_exprs * num_cov_exprs;
        g_naryn->verify_max_data_size(num_vals, "Result");

        vector<AvgMatrix> avg_x(totalbins, AvgMatrix(num_cov_exprs, vector<Avg>(num_cov_exprs)));   // avg(x)   given y != Nan
        vector<AvgMatrix> avg_xx(totalbins, AvgMatrix(num_cov_exprs, vector<Avg>(num_cov_exprs)));  // avg(x*x) given y != Nan
        vector<AvgMatrix> avg_xy(totalbins, AvgMatrix(num_cov_exprs, vector<Avg>(num_cov_exprs)));  // avg(x*y)

        vector<uint64_t> distribution(totalbins, 0);
        vector<double> vals(bins_manager.get_num_bin_finders());

        for (; !scanner.isend(); scanner.next()) {
            for (unsigned i = 0; i < num_breaks_exprs; ++i)
                vals[i] = scanner.real(i);

            int index = bins_manager.vals2idx(vals);

            if (index >= 0) {
                for (uint64_t i = 0; i < num_cov_exprs; ++i) {
                    for (uint64_t j = 0; j < num_cov_exprs; ++j) {
                        double x = scanner.real(num_breaks_exprs + i);
                        double y = scanner.real(num_breaks_exprs + j);

                        if (!std::isnan(x) && !std::isnan(y)) {
                            avg_x[index][i][j].add(x);
                            avg_xx[index][i][j].add(x * x);
                            avg_xy[index][i][j].add(x * y);
                        }
                    }
                }
            }

            if (index >= 0)
                distribution[index]++;
        }

        // pack the answer
        SEXP answer, dim, dimnames, breaks, stat_names;
        rprotect(answer = RSaneAllocVector(VECSXP, NUM_STATS));

        SEXP rstat[NUM_STATS];
        double *stat[NUM_STATS];

        for (int i = 0; i < NUM_STATS; ++i) {
            rprotect(rstat[i] = RSaneAllocVector(REALSXP, num_vals));
            stat[i] = REAL(rstat[i]);
        }

        for (uint64_t ibin = 0; ibin < totalbins; ++ibin) {
            for (int i = 0; i < (int)num_cov_exprs; ++i) {
                for (int j = 0; j < (int)num_cov_exprs; ++j) {
                    uint64_t idx = ibin + totalbins * i + totalbins * num_cov_exprs * j;

                    stat[N][idx] = avg_x[ibin][i][j].n;

                    if (stat[N][idx]) {
                        double e_x = avg_x[ibin][i][j].avg();
                        double e_y = avg_x[ibin][j][i].avg();
                        double e_xx = avg_xx[ibin][i][j].avg();
                        double e_yy = avg_xx[ibin][j][i].avg();
                        double e_xy = avg_xy[ibin][i][j].avg();
                        double cov_xx = e_xx - e_x * e_x;
                        double cov_yy = e_yy - e_y * e_y;

                        stat[AVG][idx] = e_x;
                        stat[VAR][idx] = cov_xx;
                        stat[COV][idx] = e_xy - e_x * e_y;
                        stat[COR][idx] = stat[COV][idx] / sqrt(cov_xx * cov_yy);
                    } else {
                        stat[AVG][idx] = numeric_limits<double>::quiet_NaN();
                        stat[VAR][idx] = numeric_limits<double>::quiet_NaN();
                        stat[COV][idx] = numeric_limits<double>::quiet_NaN();
                        stat[COR][idx] = numeric_limits<double>::quiet_NaN();
                    }
                }
            }
        }

        rprotect(dim = RSaneAllocVector(INTSXP, num_breaks_exprs + 2));
        rprotect(dimnames = RSaneAllocVector(VECSXP, num_breaks_exprs + 2));
        rprotect(breaks = RSaneAllocVector(VECSXP, num_breaks_exprs));
        bins_manager.set_dims(dim, dimnames, breaks);

        INTEGER(dim)[num_breaks_exprs] = num_cov_exprs;
        INTEGER(dim)[num_breaks_exprs + 1] = num_cov_exprs;

        SEXP dimname[2];
        rprotect(dimname[0] = RSaneAllocVector(STRSXP, num_cov_exprs));
        rprotect(dimname[1] = RSaneAllocVector(STRSXP, num_cov_exprs));

        for (int i = 0; i < (int)num_cov_exprs; i++) {
            SET_STRING_ELT(dimname[0], i, STRING_ELT(_exprs, num_breaks_exprs + i));
            SET_STRING_ELT(dimname[1], i, STRING_ELT(_exprs, num_breaks_exprs + i));
        }
        SET_VECTOR_ELT(dimnames, num_breaks_exprs, dimname[0]);
        SET_VECTOR_ELT(dimnames, num_breaks_exprs + 1, dimname[1]);

        for (int i = 0; i < NUM_STATS; ++i) {
            Rf_setAttrib(rstat[i], R_DimSymbol, dim);
            Rf_setAttrib(rstat[i], R_DimNamesSymbol, dimnames);
            SET_VECTOR_ELT(answer, i, rstat[i]);
        }

        Rf_setAttrib(answer, Rf_install("breaks"), breaks);

        rprotect(stat_names = RSaneAllocVector(STRSXP, NUM_STATS));
        for (int i = 0; i < NUM_STATS; i++)
            SET_STRING_ELT(stat_names, i, Rf_mkChar(STAT_NAMES[i]));
        Rf_setAttrib(answer, R_NamesSymbol, stat_names);

        rreturn(answer);
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	rreturn(R_NilValue);
}

}

