#include <fcntl.h>
#include <memory>
#include <sys/mman.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include <R.h>
#include <Rinternals.h>

#ifdef length
#undef length
#endif

#include "EMRDb.h"
#include "EMRTrack.h"
#include "naryn.h"
#include "NRPoint.h"

extern "C" {

SEXP emr_track_rm(SEXP _track, SEXP _envir)
{
	try {
		Naryn naryn(_envir);

		// check the arguments
		if (!isString(_track) || Rf_length(_track) != 1)
			verror("Track argument is not a string");

		const char *trackname = CHAR(STRING_ELT(_track, 0));
        vdebug("Removing track %s\n", trackname);
        const EMRDb::TrackInfo *track_info = g_db->track_info(trackname);

        if (!track_info)
            verror("Track %s does not exist", trackname);

        vdebug("Removing track file %s\n", track_info->filename.c_str());
        if (unlink(track_info->filename.c_str()))
            verror("Deleting file %s: %s", track_info->filename.c_str(), strerror(errno));

        g_db->unload_track(trackname);
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

SEXP emr_track_info(SEXP _track, SEXP _envir)
{
	try {
		Naryn naryn(_envir);

		// check the arguments
		if (!isString(_track) || Rf_length(_track) != 1)
			verror("Track argument is not a string");

        enum { PATH, TYPE, DATA_TYPE, CATEGORICAL, NUM_VALS, NUM_UNIQUE_VALS, MIN_VAL, MAX_VAL, MIN_ID, MAX_ID, MIN_TIME, MAX_TIME, NUM_COLS };

        const char *COL_NAMES[NUM_COLS] = { "path", "type", "data.type", "categorical", "num.vals", "num.unique.vals", "min.val", "max.val", "min.id", "max.id", "min.time", "max.time" };

		const char *trackname = CHAR(STRING_ELT(_track, 0));
        SEXP answer;
        SEXP names;
        EMRTrack *track = g_db->track(trackname);
        const EMRDb::TrackInfo *track_info = g_db->track_info(trackname);

        if (!track)
            verror("Track %s does not exist", trackname);

        rprotect(answer = RSaneAllocVector(VECSXP, NUM_COLS));
        rprotect(names = RSaneAllocVector(STRSXP, NUM_COLS));

        // path
        SET_VECTOR_ELT(answer, PATH, RSaneAllocVector(STRSXP, 1));
        SET_STRING_ELT(VECTOR_ELT(answer, PATH), 0, mkChar(track_info->filename.c_str()));

        // type
        SET_VECTOR_ELT(answer, TYPE, RSaneAllocVector(STRSXP, 1));
        SET_STRING_ELT(VECTOR_ELT(answer, TYPE), 0, mkChar(EMRTrack::TRACK_TYPE_NAMES[track->track_type()]));

        // data.type
        SET_VECTOR_ELT(answer, DATA_TYPE, RSaneAllocVector(STRSXP, 1));
        SET_STRING_ELT(VECTOR_ELT(answer, DATA_TYPE), 0, mkChar(EMRTrack::DATA_TYPE_NAMES[track->data_type()]));

        // categorical
        SET_VECTOR_ELT(answer, CATEGORICAL, RSaneAllocVector(LGLSXP, 1));
        LOGICAL(VECTOR_ELT(answer, CATEGORICAL))[0] = track->is_categorical();

        // num.vals
        SET_VECTOR_ELT(answer, NUM_VALS, RSaneAllocVector(INTSXP, 1));
        INTEGER(VECTOR_ELT(answer, NUM_VALS))[0] = track->size();
        
        // num.unique.vals
        SET_VECTOR_ELT(answer, NUM_UNIQUE_VALS, RSaneAllocVector(INTSXP, 1));
        INTEGER(VECTOR_ELT(answer, NUM_UNIQUE_VALS))[0] = track->unique_size();

        // min.val
        SET_VECTOR_ELT(answer, MIN_VAL, RSaneAllocVector(REALSXP, 1));
        REAL(VECTOR_ELT(answer, MIN_VAL))[0] = track->minval();

        // max.val
        SET_VECTOR_ELT(answer, MAX_VAL, RSaneAllocVector(REALSXP, 1));
        REAL(VECTOR_ELT(answer, MAX_VAL))[0] = track->maxval();

        // min.id
        SET_VECTOR_ELT(answer, MIN_ID, RSaneAllocVector(INTSXP, 1));
        INTEGER(VECTOR_ELT(answer, MIN_ID))[0] = track->minid();

        // max.id
        SET_VECTOR_ELT(answer, MAX_ID, RSaneAllocVector(INTSXP, 1));
        INTEGER(VECTOR_ELT(answer, MAX_ID))[0] = track->maxid();

        // min.time
        SET_VECTOR_ELT(answer, MIN_TIME, RSaneAllocVector(INTSXP, 1));
        INTEGER(VECTOR_ELT(answer, MIN_TIME))[0] = track->mintime();

        // max.time
        SET_VECTOR_ELT(answer, MAX_TIME, RSaneAllocVector(INTSXP, 1));
        INTEGER(VECTOR_ELT(answer, MAX_TIME))[0] = track->maxtime();

        for (int i = 0; i < NUM_COLS; ++i)
            SET_STRING_ELT(names, i, mkChar(COL_NAMES[i]));

        setAttrib(answer, R_NamesSymbol, names);

        return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

SEXP emr_track_ids(SEXP _track, SEXP _envir)
{
	try {
		Naryn naryn(_envir);

		// check the arguments
		if (!isString(_track) || Rf_length(_track) != 1)
			verror("Track argument is not a string");

		const char *trackname = CHAR(STRING_ELT(_track, 0));
        SEXP answer;
        vector<unsigned> ids;
        EMRTrack *track = g_db->track(trackname);

        if (!track)
            verror("Track %s does not exist", trackname);

        track->ids(ids);
        g_naryn->verify_max_data_size(ids.size(), "IDs");
        return NRPoint::convert_ids(ids, 1, false);
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

SEXP emr_track_unique(SEXP _track, SEXP _envir)
{
	try {
		Naryn naryn(_envir);

		// check the arguments
		if (!isString(_track) || Rf_length(_track) != 1)
			verror("Track argument is not a string");

		const char *trackname = CHAR(STRING_ELT(_track, 0));
        SEXP answer;
        EMRTrack *track = g_db->track(trackname);

        if (!track)
            verror("Track %s does not exist", trackname);

        g_naryn->verify_max_data_size(track->unique_size(), "Result");
        rprotect(answer = RSaneAllocVector(REALSXP, track->unique_size()));

        vector<double> unique_vals;
        track->unique_vals(unique_vals);

        for (size_t i = 0; i < unique_vals.size(); ++i)
            REAL(answer)[i] = unique_vals[i];

        return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	return R_NilValue;
}

SEXP emr_track_percentile(SEXP _track, SEXP _value, SEXP _lower, SEXP _envir)
{
	try {
		Naryn naryn(_envir);

		// check the arguments
		if (!isString(_track) || Rf_length(_track) != 1)
			verror("The value of 'track' parameter must be a string");

        if (!isReal(_value) && !isInteger(_value))
            verror("'val' argument must be numeric");

        if (!isLogical(_lower) || Rf_length(_lower) != 1)
            verror("'lower' argument must be a logical value");

		const char *trackname = CHAR(STRING_ELT(_track, 0));
        EMRTrack *track = g_db->track(trackname);

        if (!track)
            verror("Track %s does not exist", trackname);

        if (track->is_categorical()) 
            verror("Track %s is categorical: percentile queries are not supported", trackname);

        SEXP answer;
        int num_vals = Rf_length(_value);

        rprotect(answer = RSaneAllocVector(REALSXP, num_vals));

        if (asLogical(_lower)) {
            if (isReal(_value)) {
                for (int i = 0; i < num_vals; ++i)
                    REAL(answer)[i] = R_FINITE(REAL(_value)[i]) ? track->percentile_lower(REAL(_value)[i]) : numeric_limits<double>::quiet_NaN();
            } else {
                for (int i = 0; i < num_vals; ++i)
                    REAL(answer)[i] = INTEGER(_value)[i] == NA_INTEGER ? track->percentile_lower(INTEGER(_value)[i]) : numeric_limits<double>::quiet_NaN();
            }
        } else {
            if (isReal(_value)) {
                for (int i = 0; i < num_vals; ++i)
                    REAL(answer)[i] = R_FINITE(REAL(_value)[i]) ? track->percentile_upper(REAL(_value)[i]) : numeric_limits<double>::quiet_NaN();
            } else {
                for (int i = 0; i < num_vals; ++i)
                    REAL(answer)[i] = INTEGER(_value)[i] == NA_INTEGER ? track->percentile_upper(INTEGER(_value)[i]) : numeric_limits<double>::quiet_NaN();
            }
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
