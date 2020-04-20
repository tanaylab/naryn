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
#include "FileUtils.h"
#include "naryn.h"
#include "NRPoint.h"

extern "C" {

SEXP emr_track_mv(SEXP _srctrack, SEXP _tgttrack, SEXP _space, SEXP _envir)
{
	try {
		Naryn naryn(_envir);

		// check the arguments
		if (!isString(_srctrack) || Rf_length(_srctrack) != 1)
			verror("'src' argument is not a string");

		if (!isString(_tgttrack) || Rf_length(_tgttrack) != 1)
			verror("'tgt' argument is not a string");

        if (!isNull(_space) && (!isString(_space) || Rf_length(_space) != 1))
            verror("'space' must be a string");

		const char *src_trackname = CHAR(STRING_ELT(_srctrack, 0));
		const char *tgt_trackname = CHAR(STRING_ELT(_tgttrack, 0));
        const EMRDb::TrackInfo *src_track_info = g_db->track_info(src_trackname);
        string space;

        if (!src_track_info)
            verror("Track %s does not exist", src_trackname);

        EMRDb::check_track_name(tgt_trackname);

        if (isNull(_space))
            space = src_track_info->is_global ? "global" : "user";
        else {
            space = CHAR(asChar(_space));
            if (space != "global") {
                if (space == "user") {
                    if (g_db->urootdir().empty())
                        verror("User space root directory is not set");
                } else
                    verror("Invalid value of 'space' argument");
            }
        }

        if (strcmp(src_trackname, tgt_trackname)) {
            if (g_db->track_info(tgt_trackname))
                verror("Track %s already exists", tgt_trackname);
        } else if ((space == "user") ^ src_track_info->is_global)
            verror("Cannot move track '%s' into itself.", src_trackname);

        string tgt_fname = (space == "global" ? g_db->grootdir() : g_db->urootdir()) + string("/") + tgt_trackname + EMRDb::TRACK_FILE_EXT;
        vdebug("Moving track file %s to %s\n", src_track_info->filename.c_str(), tgt_fname.c_str());
        FileUtils::move_file(src_track_info->filename.c_str(), tgt_fname.c_str());

        g_db->unload_track(src_trackname);
        g_db->load_track(tgt_trackname, space == "global");
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	rreturn(R_NilValue);
}

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
        SEXP names, rpath, rtype, rdata_type, rcategorical, rnum_vals, rnum_unique_vals, rmin_val, rmax_val, rmin_id, rmax_id, rmin_time, rmax_time;
        EMRTrack *track = g_db->track(trackname);
        const EMRDb::TrackInfo *track_info = g_db->track_info(trackname);

        if (!track)
            verror("Track %s does not exist", trackname);

        rprotect(answer = RSaneAllocVector(VECSXP, NUM_COLS));
        rprotect(names = RSaneAllocVector(STRSXP, NUM_COLS));
        rprotect(rpath = RSaneAllocVector(STRSXP, 1));
        rprotect(rtype = RSaneAllocVector(STRSXP, 1));
        rprotect(rdata_type = RSaneAllocVector(STRSXP, 1));
        rprotect(rcategorical = RSaneAllocVector(LGLSXP, 1));
        rprotect(rnum_vals = RSaneAllocVector(INTSXP, 1));
        rprotect(rnum_unique_vals = RSaneAllocVector(INTSXP, 1));
        rprotect(rmin_val = RSaneAllocVector(REALSXP, 1));
        rprotect(rmax_val = RSaneAllocVector(REALSXP, 1));
        rprotect(rmin_id = RSaneAllocVector(INTSXP, 1));
        rprotect(rmax_id = RSaneAllocVector(INTSXP, 1));
        rprotect(rmin_time = RSaneAllocVector(INTSXP, 1));
        rprotect(rmax_time = RSaneAllocVector(INTSXP, 1));

        SET_STRING_ELT(rpath, 0, mkChar(track_info->filename.c_str()));
        SET_STRING_ELT(rtype, 0, mkChar(EMRTrack::TRACK_TYPE_NAMES[track->track_type()]));
        SET_STRING_ELT(rdata_type, 0, mkChar(EMRTrack::DATA_TYPE_NAMES[track->data_type()]));
        LOGICAL(rcategorical)[0] = track->is_categorical();
        INTEGER(rnum_vals)[0] = track->size();
        INTEGER(rnum_unique_vals)[0] = track->unique_size();
        REAL(rmin_val)[0] = track->size() ? track->minval() : numeric_limits<double>::quiet_NaN();
        REAL(rmax_val)[0] = track->size() ? track->maxval() : numeric_limits<double>::quiet_NaN();
        INTEGER(rmin_id)[0] = track->size() ? track->minid() : NA_INTEGER;
        INTEGER(rmax_id)[0] = track->size() ? track->maxid() : NA_INTEGER;
        INTEGER(rmin_time)[0] = track->size() ? track->mintime() : NA_INTEGER;
        INTEGER(rmax_time)[0] = track->size() ? track->maxtime() : NA_INTEGER;

        for (int i = 0; i < NUM_COLS; ++i)
            SET_STRING_ELT(names, i, mkChar(COL_NAMES[i]));

        SET_VECTOR_ELT(answer, CATEGORICAL, rcategorical);
        SET_VECTOR_ELT(answer, PATH, rpath);
        SET_VECTOR_ELT(answer, TYPE, rtype);
        SET_VECTOR_ELT(answer, DATA_TYPE, rdata_type);
        SET_VECTOR_ELT(answer, NUM_VALS, rnum_vals);
        SET_VECTOR_ELT(answer, NUM_UNIQUE_VALS, rnum_unique_vals);
        SET_VECTOR_ELT(answer, MIN_VAL, rmin_val);
        SET_VECTOR_ELT(answer, MAX_VAL, rmax_val);
        SET_VECTOR_ELT(answer, MIN_ID, rmin_id);
        SET_VECTOR_ELT(answer, MAX_ID, rmax_id);
        SET_VECTOR_ELT(answer, MIN_TIME, rmin_time);
        SET_VECTOR_ELT(answer, MAX_TIME, rmax_time);

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
