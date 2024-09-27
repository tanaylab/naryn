#include "NRIteratorFilter.h"
#include "NRPoint.h"
#include "NRTrackExpressionVars.h"
#include "naryn.h"

using namespace std;

extern "C" {

SEXP emr_check_vtrack(SEXP _vtrackstr, SEXP _vtrack, SEXP _envir) {
    try {
        Naryn naryn(_envir);

        // check the arguments
        if (!Rf_isString(_vtrackstr) || Rf_length(_vtrackstr) != 1)
            verror("The value of 'vtrackstr' parameter is not a string");

        const char *vtrackstr = CHAR(STRING_ELT(_vtrackstr, 0));
        NRTrackExpressionVars::check_vtrack(vtrackstr, _vtrack);
    } catch (TGLException &e) {
        rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
    rreturn(R_NilValue);
}

SEXP emr_check_vtrack_attr_src(SEXP _src, SEXP _envir) {
    try {
        Naryn naryn(_envir);

        if (Rf_isString(_src)) {
            if (Rf_length(_src) != 1)
                verror("'src' must be a string or a data frame");

            const char *trackname = CHAR(STRING_ELT(_src, 0));

            if (!g_db->track(trackname))
                verror("Track %s does not exist", trackname);
        } else {
            EMRTrackData<float> data;

            if (!Rf_isVector(_src) || Rf_length(_src) != 2 ||
                !Rf_isLogical(VECTOR_ELT(_src, 1)))
                verror("Invalid format of 'src'");

            NRPoint::convert_rpoints_vals(VECTOR_ELT(_src, 0), data, "'src': ");
        }
    } catch (TGLException &e) {
        rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
    rreturn(R_NilValue);
}

SEXP emr_check_vtrack_attr_func(SEXP _func, SEXP _envir) {
    try {
        Naryn naryn(_envir);

        // check the arguments
        if (!Rf_isString(_func) || Rf_length(_func) != 1)
            verror("The value of 'func' must be a string");

        string func = CHAR(STRING_ELT(_func, 0));
        transform(func.begin(), func.end(), func.begin(), ::tolower);

        for (int ifunc = 0; ifunc < EMRTrack::NUM_FUNCS; ++ifunc) {
            if (!strcmp(func.c_str(), EMRTrack::FUNC_INFOS[ifunc].name))
                rreturn(R_NilValue);
        }
        verror("Invalid function \"%s\"", func.c_str());
    } catch (TGLException &e) {
        rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
    rreturn(R_NilValue);
}

SEXP emr_check_vtrack_attr_time_shift(SEXP _tshift, SEXP _envir) {
    try {
        Naryn naryn(_envir);

        // check the arguments
        if (!(Rf_isReal(_tshift) || Rf_isInteger(_tshift)) ||
            Rf_length(_tshift) < 1 || Rf_length(_tshift) > 2)
            verror("'time.shift' must be an integer or a pair of integers");

        int sshift, eshift;

        if (Rf_length(_tshift) == 1)
            sshift = eshift =
                Rf_isReal(_tshift) ? (int)REAL(_tshift)[0] : INTEGER(_tshift)[0];
        else {
            sshift =
                Rf_isReal(_tshift) ? (int)REAL(_tshift)[0] : INTEGER(_tshift)[0];
            eshift =
                Rf_isReal(_tshift) ? (int)REAL(_tshift)[1] : INTEGER(_tshift)[1];
        }

        if (sshift < -(int)EMRTimeStamp::MAX_HOUR ||
            sshift > (int)EMRTimeStamp::MAX_HOUR ||
            eshift < -(int)EMRTimeStamp::MAX_HOUR ||
            eshift > (int)EMRTimeStamp::MAX_HOUR)
            verror("'time.shift' is out of range");
    } catch (TGLException &e) {
        rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
    rreturn(R_NilValue);
}

SEXP emr_check_vtrack_attr_id_map(SEXP _id_map, SEXP _envir) {
    try {
        Naryn naryn(_envir);

        if (!Rf_isNull(_id_map)) {
            enum { ID1, ID2, TIME_SHIFT, NUM_COLS };
            static const char *COL_NAMES[NUM_COLS] = {"id1", "id2",
                                                      "time.shift"};
            NRTrackExpressionVars::IdMap id_map;

            if (TYPEOF(_id_map) == PROMSXP) {
                if (NARYN_PRENV(_id_map) == R_NilValue)
                    _id_map = NARYN_PRVALUE(_id_map);
                else
                    _id_map = eval_in_R(NARYN_PREXPR(_id_map), NARYN_PRENV(_id_map));
            }

            if (!Rf_isVector(_id_map) || Rf_xlength(_id_map) < NUM_COLS - 1)
                verror("Invalid format of 'id.map'");

            SEXP colnames = Rf_getAttrib(_id_map, R_NamesSymbol);

            if (!Rf_isString(colnames) || Rf_xlength(colnames) < NUM_COLS - 1)
                verror("Invalid format of 'id.map'");

            for (unsigned i = 0; i < NUM_COLS - 1; i++) {
                if (strcmp(CHAR(STRING_ELT(colnames, i)), COL_NAMES[i]))
                    verror("Invalid format of 'id.map'");
            }

            bool time_shift_used =
                Rf_xlength(_id_map) >= NUM_COLS && Rf_xlength(colnames) >= NUM_COLS &&
                !strcmp(CHAR(STRING_ELT(colnames, TIME_SHIFT)),
                        COL_NAMES[TIME_SHIFT]);

            SEXP rids1 = VECTOR_ELT(_id_map, ID1);
            SEXP rids2 = VECTOR_ELT(_id_map, ID2);
            SEXP rtime_shift =
                time_shift_used ? VECTOR_ELT(_id_map, TIME_SHIFT) : R_NilValue;
            unsigned num_ids = (unsigned)Rf_xlength(rids1);

            if ((!Rf_isReal(rids1) && !Rf_isInteger(rids1)) ||
                (!Rf_isReal(rids2) && !Rf_isInteger(rids2)) ||
                (Rf_xlength(rids1) != Rf_xlength(rids2)) ||
                (time_shift_used && (Rf_xlength(rids1) != Rf_xlength(rtime_shift)))) {
                verror("Invalid format of 'id.map'");
            }

            for (unsigned i = 0; i < num_ids; ++i) {
                double id1 = Rf_isReal(rids1) ? REAL(rids1)[i] : INTEGER(rids1)[i];
                double id2 = Rf_isReal(rids2) ? REAL(rids2)[i] : INTEGER(rids2)[i];
                int time_shift = 0;

                if (time_shift_used)
                    time_shift = Rf_isReal(rtime_shift) ? REAL(rtime_shift)[i]
                                                     : INTEGER(rtime_shift)[i];

                if (!g_db->id_exists((unsigned)id1) || id1 != (int)id1)
                    verror("Invalid source id (%g) within 'id.map'", id1);
                if (!g_db->id_exists((unsigned)id2) || id2 != (int)id2)
                    verror("Invalid target id (%g) within 'id.map'", id2);

                NRTrackExpressionVars::IdMap::const_iterator iid_map =
                    id_map.find((unsigned)id1);
                if (iid_map != id_map.end())
                    verror("Id (%d) is mapped more than once within 'id.map'",
                           (unsigned)id1);

                id_map[(unsigned)id1] = {(unsigned)id2, time_shift};
            }
        }
    } catch (TGLException &e) {
        rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
    rreturn(R_NilValue);
}

SEXP emr_check_vtrack_attr_filter(SEXP _filter, SEXP _envir) {
    try {
        Naryn naryn(_envir);

        if (!Rf_isNull(_filter)) {
            NRIteratorFilter filter;
            filter.init(_filter, 0, EMRTimeStamp::MAX_HOUR);
        }
    } catch (TGLException &e) {
        rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
    rreturn(R_NilValue);
}

}
