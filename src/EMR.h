#ifndef EMR_H_INCLUDED
#define EMR_H_INCLUDED

#ifdef PYTHON

    #define Rprintf printf
    #define unif_rand drand48

#else

    #include <R.h>
    #include <Rinternals.h>

    #ifdef length
    #undef length
    #endif

    #ifdef error
    #undef error
    #endif

#endif

#endif

