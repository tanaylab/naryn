#include <cmath>
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

SEXP emr_covariance(SEXP _exprs, SEXP _breaks, SEXP _include_lowest, SEXP _stime, SEXP _etime, SEXP _iterator_policy, SEXP _keepref, SEXP _filter, SEXP _envir)
{
	try {
        Naryn naryn(_envir);

		if (!isString(_exprs) || Rf_length(_exprs) < 1)
			verror("Track expressions argument must be a vector of strings");

        unsigned num_exprs = (unsigned)Rf_length(_exprs);
        unsigned num_breaks_exprs = (unsigned)Rf_length(_breaks);
        unsigned num_cov_exprs = num_exprs - num_breaks_exprs;

        if (num_cov_exprs < 1)
            verror("cor.exprs list is empty");

        NRTrackExprScanner scanner;
        scanner.begin(_exprs, NRTrackExprScanner::REAL_T, _stime, _etime, _iterator_policy, _keepref, _filter);

        BinsManager bins_manager(_breaks, _include_lowest, &scanner.get_track_exprs(), &scanner.get_expr_vars());

        if (bins_manager.get_num_bin_finders() != num_breaks_exprs)
            verror("Number of breaks sets must be equal to the number of tracks used");

        unsigned totalbins = bins_manager.get_total_bins();
        g_naryn->verify_max_data_size(totalbins, "Result");

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
                for (size_t i = 0; i < num_cov_exprs; ++i) {
                    for (size_t j = 0; j < num_cov_exprs; ++j) {
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
        enum { N, AVG, VAR, COV, COR, NUM_STATS };

        const char *STAT_NAMES[NUM_STATS] = { "n", "e", "var", "cov", "cor" };

        SEXP answer, dim, dimnames, breaks, stat_names;
        size_t num_vals = totalbins * num_cov_exprs * num_cov_exprs;
        rprotect(answer = allocVector(VECSXP, NUM_STATS));

        double *stat[NUM_STATS];

        for (int i = 0; i < NUM_STATS; ++i) {
            SET_VECTOR_ELT(answer, i, allocVector(REALSXP, num_vals));
            stat[i] = REAL(VECTOR_ELT(answer, i));
        }

        for (size_t ibin = 0; ibin < totalbins; ++ibin) {
            for (int i = 0; i < num_cov_exprs; ++i) {
                for (int j = 0; j < num_cov_exprs; ++j) {
                    size_t idx = ibin + totalbins * i + totalbins * num_cov_exprs * j;

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

        rprotect(dim = allocVector(INTSXP, num_breaks_exprs + 2));
        rprotect(dimnames = allocVector(VECSXP, num_breaks_exprs + 2));
        rprotect(breaks = allocVector(VECSXP, num_breaks_exprs));
        bins_manager.set_dims(dim, dimnames, breaks);

        INTEGER(dim)[num_breaks_exprs] = num_cov_exprs;
        INTEGER(dim)[num_breaks_exprs + 1] = num_cov_exprs;

        SEXP dimname[2];
        rprotect(dimname[0] = allocVector(STRSXP, num_cov_exprs));
        rprotect(dimname[1] = allocVector(STRSXP, num_cov_exprs));

        for (int i = 0; i < num_cov_exprs; i++) {
            SET_STRING_ELT(dimname[0], i, STRING_ELT(_exprs, num_breaks_exprs + i));
            SET_STRING_ELT(dimname[1], i, STRING_ELT(_exprs, num_breaks_exprs + i));
        }
        SET_VECTOR_ELT(dimnames, num_breaks_exprs, dimname[0]);
        SET_VECTOR_ELT(dimnames, num_breaks_exprs + 1, dimname[1]);

        for (int i = 0; i < NUM_STATS; ++i) {
            setAttrib(VECTOR_ELT(answer, i), R_DimSymbol, dim);
            setAttrib(VECTOR_ELT(answer, i), R_DimNamesSymbol, dimnames);
        }

        setAttrib(answer, install("breaks"), breaks);

        setAttrib(answer, R_NamesSymbol, (stat_names = allocVector(STRSXP, NUM_STATS)));
        for (int i = 0; i < NUM_STATS; i++)
            SET_STRING_ELT(stat_names, i, mkChar(STAT_NAMES[i]));

        rreturn(answer);
	} catch (TGLException &e) {
		rerror("%s", e.msg());
	}
	rreturn(R_NilValue);
}

}

