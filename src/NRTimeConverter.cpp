#include <cmath>
#include <limits>
#include <vector>

#include <R.h>
#include <Rinternals.h>

#ifdef length
#undef length
#endif

#include "naryn.h"
#include "NRTimeConverter.h"
#include "TGLException.h"

using namespace std;

static const unsigned START_YEAR = 1867;
static const unsigned START_MONTH = 2;        // zero based => March
static const unsigned START_DAYOFMONTH = 0;   // zero based => 1 March
static const unsigned YEARS_RANGE = 300;
static const unsigned DAYS_RANGE = 366 * YEARS_RANGE;

static const unsigned LEAP_YEAR_DAYS[12] =  { 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
static const unsigned NLEAP_YEAR_DAYS[12] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

static vector<unsigned> s_day2year;
static vector<unsigned> s_day2month;
static vector<unsigned> s_day2dayofmonth;
static vector<unsigned> s_date2day;

struct NRTimeConverterInit {
    NRTimeConverterInit();
};

static NRTimeConverterInit s_time_converter_init;

NRTimeConverterInit::NRTimeConverterInit()
{
    s_day2year.resize(DAYS_RANGE);
    s_day2month.resize(DAYS_RANGE);
    s_day2dayofmonth.resize(DAYS_RANGE);
    s_date2day.reserve(31 * 12 * YEARS_RANGE);

    unsigned day;
    unsigned year = START_YEAR;
    unsigned month = START_MONTH;
    unsigned dayofmonth = START_DAYOFMONTH;
    bool leap = is_leap_year(year);
    unsigned next_month_day = leap ? LEAP_YEAR_DAYS[month] - dayofmonth : NLEAP_YEAR_DAYS[month] - dayofmonth;;

    for (day = 0; day < DAYS_RANGE; ++day) {
        if (day == next_month_day) {
            if (month == 11) {
                month = 0;
                ++year;
                leap = is_leap_year(year);
            } else
                ++month;

            dayofmonth = 0;
            next_month_day += leap ? LEAP_YEAR_DAYS[month] : NLEAP_YEAR_DAYS[month];
        }
        s_day2year[day] = year;
        s_day2month[day] = month;
        s_day2dayofmonth[day] = dayofmonth;
        ++dayofmonth;
    }

    day = 0;
    for (year = START_YEAR; year < START_YEAR + YEARS_RANGE; ++year) {
        leap = is_leap_year(year);
        for (month = 0; month < 12; ++month) {
            for (dayofmonth = 0; dayofmonth < 31; ++dayofmonth) {
                if (leap && dayofmonth >= LEAP_YEAR_DAYS[month] ||
                    !leap && dayofmonth >= NLEAP_YEAR_DAYS[month] ||
                    year == START_YEAR && month < START_MONTH ||
                    year == START_YEAR && month == START_MONTH && dayofmonth < START_DAYOFMONTH)
                    s_date2day.push_back((unsigned)-1);
                else
                    s_date2day.push_back(day++);
            }
        }
    }
}

unsigned time2year(unsigned t)
{
    unsigned day = t / 24;
    if (day >= DAYS_RANGE)
        TGLError("Time is out of range");
    return s_day2year[day];
}

unsigned time2month(unsigned t)
{
    unsigned day = t / 24;
    if (day >= DAYS_RANGE)
        TGLError("Time is out of range");
    return s_day2month[day];
}

unsigned time2dayofmonth(unsigned t)
{
    unsigned day = t / 24;
    if (day >= DAYS_RANGE)
        TGLError("Time is out of range");
    return s_day2dayofmonth[day];
}

unsigned time2hour(unsigned t)
{
    unsigned day = t / 24;
    if (day >= DAYS_RANGE)
        TGLError("Time is out of range");
    return t % 24;
}

unsigned date2time(unsigned hour, unsigned dayofmonth, unsigned month, unsigned year)
{
    if (hour >= 24 || year < START_YEAR || year >= START_YEAR + YEARS_RANGE || month >= 12 || dayofmonth >= 31)
        TGLError("Time is out of range");

    unsigned idx = (year - START_YEAR) * 12 * 31 + month * 31 + dayofmonth;

    if (idx > s_date2day.size() || s_date2day[idx] == (unsigned)-1)
        TGLError("Time is out of range");

    return 24 * s_date2day[idx] + hour;
}

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
