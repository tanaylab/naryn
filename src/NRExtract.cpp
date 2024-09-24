#include <cmath>

#include "naryn.h"
#include "NRPoint.h"
#include "NRTrackExpressionScanner.h"

extern "C" {

SEXP C_emr_extract(SEXP _exprs, SEXP _names, SEXP _tidy, SEXP _sort, SEXP _stime, SEXP _etime, SEXP _iterator_policy, SEXP _keepref, SEXP _filter, SEXP _envir)
{
	try {
        Naryn naryn(_envir);

		if (!Rf_isString(_exprs) || Rf_length(_exprs) < 1)
			verror("\"expr\" argument must be a vector of strings");

        if (!Rf_isLogical(_tidy) && Rf_length(_tidy) != 1)
            verror("\"tidy\" argument must be logical");

        int tidy = Rf_asLogical(_tidy);

        if (tidy == NA_LOGICAL)
            verror("\"tidy\" argument cannot be NA");

        if (!Rf_isLogical(_sort) && Rf_length(_sort) != 1)
            verror("\"sort\" argument must be logical");

        int do_sort = Rf_asLogical(_sort);

        if (do_sort == NA_LOGICAL)
            verror("\"sort\" argument cannot be NA");

		if (!Rf_isNull(_names)) {
			if (!Rf_isString(_names))
				verror("\"names\" argument must be a vector of strings");
			if (Rf_length(_names) != Rf_length(_exprs))
				verror("Length of \"names\" argument must match the number of track expressions");
		}

		unsigned num_exprs = (unsigned)Rf_length(_exprs);
		NRTrackExprScanner scanner;

        EMRPoints out_points;

        if (tidy) {
            vector<unsigned> expr_idx;
            vector<double> values;

            for (scanner.begin(_exprs, NRTrackExprScanner::REAL_T, _stime, _etime, _iterator_policy, _keepref, _filter); !scanner.isend(); scanner.next()) {
                for (unsigned iexpr = 0; iexpr < num_exprs; ++iexpr) {
                    double val = scanner.real(iexpr);

                    if (!std::isnan(val)) {
                        out_points.push_back(scanner.point());
                        expr_idx.push_back(iexpr);
                        values.push_back(val);
                    }
                }

                g_naryn->verify_max_data_size(out_points.size(), "Result");
            }

            enum { EXPR, VAL, NUM_COLS };
            const char *COLNAMES[NUM_COLS] = { "expr", "val" };

            // assemble the answer
            vector<EMRPoint *> ppoints;
            SEXP answer = NRPoint::convert_points(out_points, NRPoint::NUM_POINT_COLS + NUM_COLS, false, do_sort, &ppoints);
            SEXP rexprs, rexpr_idx, rexpr_vals;

            rprotect(rexpr_idx = RSaneAllocVector(INTSXP, out_points.size()));
            rprotect(rexpr_vals = RSaneAllocVector(REALSXP, out_points.size()));
            rprotect(rexprs = RSaneAllocVector(STRSXP, num_exprs));

            for (vector<EMRPoint *>::const_iterator ippoint = ppoints.begin(); ippoint != ppoints.end(); ++ippoint) {
                INTEGER(rexpr_idx)[ippoint - ppoints.begin()] = expr_idx[*ippoint - &out_points.front()] + 1;
                REAL(rexpr_vals)[ippoint - ppoints.begin()] = values[*ippoint - &out_points.front()];
            }

            for (unsigned iexpr = 0; iexpr < (unsigned)num_exprs; ++iexpr) {
                if (Rf_isNull(_names))
                    SET_STRING_ELT(rexprs, iexpr, STRING_ELT(_exprs, iexpr));
                else
                    SET_STRING_ELT(rexprs, iexpr, STRING_ELT(_names, iexpr));
            }

            SEXP col_names = Rf_getAttrib(answer, R_NamesSymbol);
            for (unsigned i = 0; i < NUM_COLS; ++i)
                SET_STRING_ELT(col_names, NRPoint::NUM_POINT_COLS + i, Rf_mkChar(COLNAMES[i]));

            SET_VECTOR_ELT(answer, NRPoint::NUM_POINT_COLS + EXPR, rexpr_idx);
            SET_VECTOR_ELT(answer, NRPoint::NUM_POINT_COLS + VAL, rexpr_vals);

            Rf_setAttrib(rexpr_idx, R_LevelsSymbol, rexprs);
            Rf_setAttrib(rexpr_idx, R_ClassSymbol, Rf_mkString("factor"));

            return answer;
        } else {
    		vector< vector<double> > values(num_exprs);

    		for (scanner.begin(_exprs, NRTrackExprScanner::REAL_T, _stime, _etime, _iterator_policy, _keepref, _filter); !scanner.isend(); scanner.next()) {
    			for (unsigned iexpr = 0; iexpr < num_exprs; ++iexpr)
    				values[iexpr].push_back(scanner.real(iexpr));

                out_points.push_back(scanner.point());
    			g_naryn->verify_max_data_size(out_points.size() * num_exprs, "Result");
    		}

            // assemble the answer
            vector<EMRPoint *> ppoints;
            SEXP answer = NRPoint::convert_points(out_points, NRPoint::NUM_POINT_COLS + num_exprs, false, do_sort, &ppoints);

            for (unsigned iexpr = 0; iexpr < num_exprs; ++iexpr) {
                 SEXP rexpr_vals;
                 rprotect(rexpr_vals = RSaneAllocVector(REALSXP, values[iexpr].size()));
                 for (vector<EMRPoint *>::const_iterator ippoint = ppoints.begin(); ippoint != ppoints.end(); ++ippoint)
                     REAL(rexpr_vals)[ippoint - ppoints.begin()] = values[iexpr][*ippoint - &out_points.front()];
                 SET_VECTOR_ELT(answer, NRPoint::NUM_POINT_COLS + iexpr, rexpr_vals);
            }

            SEXP col_names = Rf_getAttrib(answer, R_NamesSymbol);
            for (unsigned iexpr = 0; iexpr < num_exprs; ++iexpr) {
                if (Rf_isNull(_names))
                    SET_STRING_ELT(col_names, NRPoint::NUM_POINT_COLS + iexpr, Rf_mkChar(get_bound_colname(CHAR(STRING_ELT(_exprs, iexpr))).c_str()));
                else
                    SET_STRING_ELT(col_names, NRPoint::NUM_POINT_COLS + iexpr, STRING_ELT(_names, iexpr));
            }

            rreturn(answer);
        }
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	rreturn(R_NilValue);
}

}
