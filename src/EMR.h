#ifndef EMR_H_INCLUDED
#define EMR_H_INCLUDED

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

#endif

