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

extern "C" {

SEXP C_emr_dist(SEXP _exprs, SEXP _breaks, SEXP _include_lowest, SEXP _right, SEXP _stime, SEXP _etime, SEXP _iterator_policy, SEXP _keepref, SEXP _filter, SEXP _envir)
{
	try {
        Naryn naryn(_envir);

		if (!isString(_exprs) || Rf_length(_exprs) < 1)
			verror("Track expressions argument must be a vector of strings");

        unsigned num_exprs = (unsigned)Rf_length(_exprs);

        NRTrackExprScanner scanner;
        scanner.begin(_exprs, NRTrackExprScanner::REAL_T, _stime, _etime, _iterator_policy, _keepref, _filter);

        BinsManager bins_manager(_breaks, _include_lowest, _right, &scanner.get_track_exprs(), &scanner.get_expr_vars());

        if (bins_manager.get_num_bin_finders() != num_exprs)
            verror("Number of breaks sets must be equal to the number of tracks used");

        unsigned totalbins = bins_manager.get_total_bins();
        g_naryn->verify_max_data_size(totalbins, "Result");
        vector<uint64_t> distribution(totalbins, 0);
        vector<double> vals(bins_manager.get_num_bin_finders());

        for (; !scanner.isend(); scanner.next()) {
            for (unsigned i = 0; i < num_exprs; ++i)
                vals[i] = scanner.real(i);

            int index = bins_manager.vals2idx(vals);

            if (index >= 0)
                distribution[index]++;
        }

        // pack the answer
        SEXP answer, dim, dimnames, breaks;
        rprotect(answer = RSaneAllocVector(REALSXP, totalbins));
        double *panswer = REAL(answer);

        for (unsigned i = 0; i < totalbins; i++)
            panswer[i] = distribution[i];

        rprotect(dim = RSaneAllocVector(INTSXP, num_exprs));
        rprotect(dimnames = RSaneAllocVector(VECSXP, num_exprs));
        rprotect(breaks = RSaneAllocVector(VECSXP, num_exprs));
        bins_manager.set_dims(dim, dimnames, breaks);
        setAttrib(answer, R_DimSymbol, dim);
        setAttrib(answer, R_DimNamesSymbol, dimnames);
        setAttrib(answer, install("breaks"), breaks);
        rreturn(answer);
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	rreturn(R_NilValue);
}

}
