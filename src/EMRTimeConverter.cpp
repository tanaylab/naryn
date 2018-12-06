#include <cmath>
#include <limits>
#include <vector>

#include "EMRTimeConverter.h"
#include "TGLException.h"

using namespace Emr;
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
                if ((leap && dayofmonth >= LEAP_YEAR_DAYS[month]) ||
                    (!leap && dayofmonth >= NLEAP_YEAR_DAYS[month]) ||
                    (year == START_YEAR && month < START_MONTH) ||
                    (year == START_YEAR && month == START_MONTH && dayofmonth < START_DAYOFMONTH))
                    s_date2day.push_back((unsigned)-1);
                else
                    s_date2day.push_back(day++);
            }
        }
    }
}

unsigned Emr::time2year(unsigned t)
{
    unsigned day = t / 24;
    if (day >= DAYS_RANGE)
        TGLError("Time is out of range");
    return s_day2year[day];
}

unsigned Emr::time2month(unsigned t)
{
    unsigned day = t / 24;
    if (day >= DAYS_RANGE)
        TGLError("Time is out of range");
    return s_day2month[day];
}

unsigned Emr::time2dayofmonth(unsigned t)
{
    unsigned day = t / 24;
    if (day >= DAYS_RANGE)
        TGLError("Time is out of range");
    return s_day2dayofmonth[day];
}

unsigned Emr::time2hour(unsigned t)
{
    unsigned day = t / 24;
    if (day >= DAYS_RANGE)
        TGLError("Time is out of range");
    return t % 24;
}

unsigned Emr::date2time(unsigned hour, unsigned dayofmonth, unsigned month, unsigned year)
{
    if (hour >= 24 || year < START_YEAR || year >= START_YEAR + YEARS_RANGE || month >= 12 || dayofmonth >= 31)
        TGLError("Time is out of range");

    unsigned idx = (year - START_YEAR) * 12 * 31 + month * 31 + dayofmonth;

    if (idx > s_date2day.size() || s_date2day[idx] == (unsigned)-1)
        TGLError("Time is out of range");

    return 24 * s_date2day[idx] + hour;
}

