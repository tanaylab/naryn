#include <R.h>
#include <Rinternals.h>

#ifdef length
#undef length
#endif

#include "naryn.h"
#include "NRTrackExpressionVars.h"

using namespace std;

extern "C" {

SEXP emr_check_vtrack(SEXP _vtrack, SEXP _envir)
{
	try {
		Naryn naryn(_envir);

		// check the arguments
		if (!isString(_vtrack) || Rf_length(_vtrack) != 1)
			verror("The value of 'vtrack' parameter is not a string");

		const char *vtrack = CHAR(STRING_ELT(_vtrack, 0));
		vector<string> exprs;
		NRTrackExpressionVars parser;

		exprs.push_back(vtrack);
		parser.parse_exprs(exprs);
	} catch (TGLException &e) {
		rerror("%s", e.msg());
	}
	rreturn(R_NilValue);
}

}
