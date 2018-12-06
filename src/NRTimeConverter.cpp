#include <limits>

#include <R.h>
#include <Rinternals.h>

#ifdef length
#undef length
#endif

#include "naryn.h"
#include "EMRTimeConverter.h"
#include "TGLException.h"

using namespace Emr;
using namespace std;

extern "C" {

SEXP emr_time2hour(SEXP _t, SEXP _envir)
{
	try {
		Naryn naryn(_envir);
        
        if (!isInteger(_t) && !isReal(_t))
            verror("Invalid format of 'time' argument");

        SEXP answer = R_NilValue;
        rprotect(answer = RSaneAllocVector(REALSXP, Rf_length(_t)));

        for (int i = 0; i < Rf_length(_t); ++i) {
            double t = isInteger(_t) ? INTEGER(_t)[i] : REAL(_t)[i];

            if (std::isnan(t)) {
                REAL(answer)[i] = numeric_limits<double>::quiet_NaN();
                continue;
            }

            if (t < 0 || (int)t != t)
                verror("Invalid time value %g", t);
            REAL(answer)[i] = time2hour((unsigned)t);
        }

        return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	return R_NilValue;
}

SEXP emr_time2dayofmonth(SEXP _t, SEXP _envir)
{
	try {
		Naryn naryn(_envir);
        
        if (!isInteger(_t) && !isReal(_t))
            verror("Invalid format of 'time' argument");

        SEXP answer = R_NilValue;
        rprotect(answer = RSaneAllocVector(REALSXP, Rf_length(_t)));

        for (int i = 0; i < Rf_length(_t); ++i) {
            double t = isInteger(_t) ? INTEGER(_t)[i] : REAL(_t)[i];

            if (std::isnan(t)) {
                REAL(answer)[i] = numeric_limits<double>::quiet_NaN();
                continue;
            }

            if (t < 0 || (int)t != t)
                verror("Invalid time value %g", t);
            REAL(answer)[i] = time2dayofmonth((unsigned)t) + 1;
        }

        return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	return R_NilValue;
}

SEXP emr_time2month(SEXP _t, SEXP _envir)
{
	try {
		Naryn naryn(_envir);
        
        if (!isInteger(_t) && !isReal(_t))
            verror("Invalid format of 'time' argument");

        SEXP answer = R_NilValue;
        rprotect(answer = RSaneAllocVector(REALSXP, Rf_length(_t)));

        for (int i = 0; i < Rf_length(_t); ++i) {
            double t = isInteger(_t) ? INTEGER(_t)[i] : REAL(_t)[i];

            if (std::isnan(t)) {
                REAL(answer)[i] = numeric_limits<double>::quiet_NaN();
                continue;
            }

            if (t < 0 || (int)t != t)
                verror("Invalid time value %g", t);
            REAL(answer)[i] = time2month((unsigned)t) + 1;
        }

        return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	return R_NilValue;
}

SEXP emr_time2year(SEXP _t, SEXP _envir)
{
	try {
		Naryn naryn(_envir);
        
        if (!isInteger(_t) && !isReal(_t))
            verror("Invalid format of 'time' argument");

        SEXP answer = R_NilValue;
        rprotect(answer = RSaneAllocVector(REALSXP, Rf_length(_t)));

        for (int i = 0; i < Rf_length(_t); ++i) {
            double t = isInteger(_t) ? INTEGER(_t)[i] : REAL(_t)[i];

            if (std::isnan(t)) {
                REAL(answer)[i] = numeric_limits<double>::quiet_NaN();
                continue;
            }

            if (t < 0 || (int)t != t)
                verror("Invalid time value %g", t);
            REAL(answer)[i] = time2year((unsigned)t);
        }

        return answer;
	} catch (TGLException &e) {
        rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	return R_NilValue;
}


SEXP emr_date2time(SEXP _date, SEXP _envir)
{
	try {
		Naryn naryn(_envir);

        enum { HOUR, DAY, MONTH, YEAR, NUM_COLS };

        SEXP rcols[NUM_COLS];
        
        if (!isVector(_date) || Rf_length(_date) != NUM_COLS)
            verror("Invalid format of 'date' argument");

        for (int i = 0; i < NUM_COLS; ++i) {
            rcols[i] = VECTOR_ELT(_date, i);
            if (!isInteger(rcols[i]) && !isReal(rcols[i]) || i > 0 && Rf_length(rcols[i]) != Rf_length(rcols[i - 1]))
                verror("Invalid format of 'date' argument");
        }

        int num_rows = Rf_length(rcols[0]);

        SEXP answer;
        rprotect(answer = RSaneAllocVector(INTSXP, num_rows));

        for (int i = 0; i < num_rows; ++i) {
            double hour = isInteger(rcols[HOUR]) ? INTEGER(rcols[HOUR])[i] : REAL(rcols[HOUR])[i];
            double day = isInteger(rcols[DAY]) ? INTEGER(rcols[DAY])[i] : REAL(rcols[DAY])[i];
            double month = isInteger(rcols[MONTH]) ? INTEGER(rcols[MONTH])[i] : REAL(rcols[MONTH])[i];
            double year = isInteger(rcols[YEAR]) ? INTEGER(rcols[YEAR])[i] : REAL(rcols[YEAR])[i];

            if (hour < 0 || (int)hour != hour)
                verror("Invalid hour value %g", hour);

            if (day <= 0 || (int)day != day)
                verror("Invalid day value %g", day);

            if (month <= 0 || (int)month != month)
                verror("Invalid month value %g", month);

            if (year < 0 || (int)year != year)
                verror("Invalid year value %g", year);

            INTEGER(answer)[i] = date2time(hour, day - 1, month - 1, year);
        }

        return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	return R_NilValue;
}

}
