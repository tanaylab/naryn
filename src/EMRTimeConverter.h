#ifndef EMRTIMECONVERTER_H_INCLUDED
#define EMRTIMECONVERTER_H_INCLUDED

namespace Emr {
    unsigned time2year(unsigned t);

    // month is zero based
    unsigned time2month(unsigned t);

    // day of month is zero based
    unsigned time2dayofmonth(unsigned t);

    // hour is zero based
    unsigned time2hour(unsigned t);

    // all parameters except for year are zero based! (hour: [0,23], month: [0,11], ...)
    unsigned date2time(unsigned hour, unsigned dayofmonth, unsigned month, unsigned year);

    inline bool is_leap_year(unsigned year) { return !(year % 400) || (!(year % 4) && (year % 100)); }
}

#endif

