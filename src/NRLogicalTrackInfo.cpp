#include <cmath>

#include "naryn.h"
#include "NRTrackExpressionScanner.h"
#include "EMRPoint.h"

#include <R.h>
#include <Rinternals.h>

#ifdef length
#undef length
#endif

struct LogicalTrackInfo {
    set<int> unique_vals;
    double num_vals;
	double minval;
	double maxval;
    unsigned maxid;
    unsigned minid;
    unsigned maxtime;
    unsigned mintime;

    void update_id_time_info(EMRPoint v) {
        minid = min(minid, v.id);
        maxid = max(maxid, v.id);
        mintime = min(mintime, v.timestamp.hour());
        maxtime = max(maxtime, v.timestamp.hour());
    }

    void update_val_info(double v) {
        unique_vals.insert(v);
        num_vals++;
        minval = min(minval, v);
	    maxval = max(maxval, v);
    }

	LogicalTrackInfo() :
	 	minval(numeric_limits<double>::max()),
	 	maxval(-numeric_limits<double>::max()),
     	minid(numeric_limits<unsigned>::max()),
     	maxid(0),
     	mintime(numeric_limits<unsigned>::max()),
     	maxtime(0),
	 	num_vals(0) {}
};



extern "C" {

SEXP emr_logical_track_user_info(SEXP _track, SEXP _expr, SEXP _stime, SEXP _etime, SEXP _iterator_policy, SEXP _keepref, SEXP _filter, SEXP _gdir, SEXP _udir, SEXP _envir)
{
    EMRDb *new_g_db = NULL;
    EMRDb *old_g_db = NULL;
	try {
        Naryn naryn(_envir);
        
        enum { PATH, TYPE, DATA_TYPE, CATEGORICAL, NUM_VALS, NUM_UNIQUE_VALS, MIN_VAL, MAX_VAL, MIN_ID, MAX_ID, MIN_TIME, MAX_TIME, NUM_COLS };
        const char *COL_NAMES[NUM_COLS] = { "path", "type", "data.type", "categorical", "num.vals", "num.unique.vals", "min.val", "max.val", "min.id", "max.id", "min.time", "max.time" };

        // we create a clean EMRDb instance in order to ignore the current ids subset        
        new_g_db = new EMRDb;        
        const char *gdirname = CHAR(STRING_ELT(_gdir, 0));
        const char *udirname =
            isNull(_udir) ? NULL : CHAR(STRING_ELT(_udir, 0));
        new_g_db->init(gdirname, udirname, true, true, false);
        swap(g_db, new_g_db);

        LogicalTrackInfo summary;
        NRTrackExprScanner scanner;
        const char *logical_trackname = CHAR(STRING_ELT(_track, 0));
        const char *trackname = CHAR(STRING_ELT(_expr, 0));
        EMRTrack *track = g_db->track(trackname);
        const EMRDb::TrackInfo *track_info = g_db->track_info(trackname);

        for (scanner.begin(_expr, NRTrackExprScanner::REAL_T, _stime, _etime,
                           _iterator_policy, _keepref, _filter);
             !scanner.isend(); scanner.next()) {
            summary.update_id_time_info(scanner.point());
            summary.update_val_info(scanner.real());
        }
            
		SEXP answer;
        SEXP names, rtype, rdata_type, rcategorical, rnum_vals, rmin_id, rmax_id, rmin_time, rmax_time,  rmin_val, rmax_val, rnum_unique_vals, rpath;

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

        string path(
            new_g_db->logical_track_filename(string(logical_trackname)));
        SET_STRING_ELT(rpath, 0, mkChar(path.c_str()));
        SET_STRING_ELT(rtype, 0, mkChar(EMRTrack::TRACK_TYPE_NAMES[track->track_type()]));
        SET_STRING_ELT(rdata_type, 0, mkChar(EMRTrack::DATA_TYPE_NAMES[track->data_type()]));
        LOGICAL(rcategorical)[0] = track->is_categorical();
        INTEGER(rnum_vals)[0] = summary.num_vals;
        INTEGER(rnum_unique_vals)[0] = summary.unique_vals.size();
        REAL(rmin_val)[0] = summary.minval ? summary.minval : numeric_limits<double>::quiet_NaN();
        REAL(rmax_val)[0] = summary.maxval ? summary.maxval : numeric_limits<double>::quiet_NaN();
        INTEGER(rmin_id)[0] = summary.minid;
        INTEGER(rmax_id)[0] = summary.maxid;
        INTEGER(rmin_time)[0] = summary.mintime;
        INTEGER(rmax_time)[0] = summary.maxtime;

        for (int i = 0; i < NUM_COLS; i++)
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

        swap(g_db, new_g_db);
        delete new_g_db;
        rreturn(answer);

        } catch (TGLException &e) {
            swap(g_db, new_g_db);
            delete new_g_db;
		    rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        swap(g_db, new_g_db);
        delete new_g_db;
        rerror("Out of memory");
    }
	rreturn(R_NilValue);
}

}

