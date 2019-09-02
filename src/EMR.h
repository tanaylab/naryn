#ifndef EMR_H_INCLUDED
#define EMR_H_INCLUDED

#include <sys/time.h>
#include <utility>

using namespace std::rel_ops;

#ifdef PYTHON

    #define unif_rand drand48

    void vmsg(const char *fmt, ...);

#else

    #include <R.h>
    #include <Rinternals.h>

    #ifdef length
    #undef length
    #endif

    #ifdef error
    #undef error
    #endif

    #define vmsg Rprintf

#endif

inline bool operator==(const struct timespec &t1, const struct timespec &t2) { return t1.tv_sec == t2.tv_sec && t1.tv_nsec == t2.tv_nsec; }
inline bool operator<(const struct timespec &t1, const struct timespec &t2) { return t1.tv_sec < t2.tv_sec || (t1.tv_sec == t2.tv_sec && t1.tv_nsec < t2.tv_nsec); }

#endif

