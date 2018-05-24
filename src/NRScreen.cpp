#include "naryn.h"
#include "NRTrackExpressionScanner.h"

#include <R.h>
#include <Rinternals.h>

#ifdef length
#undef length
#endif

extern "C" {

SEXP emr_screen(SEXP _expr, SEXP _sort, SEXP _stime, SEXP _etime, SEXP _iterator_policy, SEXP _keepref, SEXP _filter, SEXP _envir)
{
	try {
        Naryn naryn(_envir);

		if (!isString(_expr) || Rf_length(_expr) != 1)
			verror("The value of 'expr' parameter must be a string");

        if (!isLogical(_sort) && Rf_length(_sort) != 1)
            verror("The value of 'sort' parameter must be logical");

        int do_sort = asLogical(_sort);

        if (do_sort == NA_LOGICAL)
            verror("The value of 'sort' parameter cannot be NA");

		NRTrackExprScanner scanner;
        NRPoints out_points;

		for (scanner.begin(_expr, NRTrackExprScanner::LOGICAL_T, _stime, _etime, _iterator_policy, _keepref, _filter); !scanner.isend(); scanner.next()) {
			if (scanner.logical() == 1)  // beware: in addition to true / false the result might also be nan
				out_points.push_back(scanner.point());
			g_naryn->verify_max_data_size(out_points.size(), "Result");
		}

        vector<NRPoint *> ppoints;
		rreturn(NRPoint::convert_points(out_points, NRPoint::NUM_COLS, false, do_sort, &ppoints));
	} catch (TGLException &e) {
		rerror("%s", e.msg());
	}
	rreturn(R_NilValue);
}

}

