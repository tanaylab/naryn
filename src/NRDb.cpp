#include <dirent.h>
#include <limits.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "EMRDb.h"
#include "EMRProgressReporter.h"
#include "EMRTrack.h"
#include "naryn.h"
#include "NRPoint.h"

extern "C" {

SEXP emr_dbload(SEXP _gdir, SEXP _udir, SEXP _load_on_demand, SEXP envir)
{
	try {
		Naryn naryn(envir, false);

        if (g_db && !g_db->ids_subset().empty())
            Rf_warning("Current subset of ids will be reset");

		delete g_db;
		g_db = NULL;

		if (!isString(_gdir) || Rf_length(_gdir) != 1 || !isNull(_udir) && (!isString(_udir) || Rf_length(_udir) != 1))
			verror("Dir argument is not a string");

        if (!isLogical(_load_on_demand) || Rf_length(_load_on_demand) != 1)
            verror("load.on.demand argument must be a logical value");

		const char *gdirname = CHAR(STRING_ELT(_gdir, 0));
        const char *udirname = isNull(_udir) ? NULL : CHAR(STRING_ELT(_udir, 0));
		g_db = new EMRDb;
		g_db->load(gdirname, udirname, asLogical(_load_on_demand));
	} catch (TGLException &e) {
		delete g_db;
		g_db = NULL;
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	return R_NilValue;
}

SEXP emr_dbunload(SEXP envir)
{
	try {
		Naryn naryn(envir, false);

		delete g_db;
		g_db = NULL;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	return R_NilValue;
}

SEXP emr_db_subset(SEXP _src, SEXP _fraction, SEXP _complementary, SEXP _envir)
{
	try {
		Naryn naryn(_envir);

        if (isNull(_src))
            g_db->clear_ids_subset();
        else {
            if (!isReal(_fraction) || Rf_length(_fraction) != 1)
                verror("\"fraction\" argument must be a numeric value");

            if (!isLogical(_complementary) || Rf_length(_complementary) != 1 || LOGICAL(_complementary)[0] == NA_LOGICAL)
                verror("\"complementary\" argument must be a logical value");

            double fraction = asReal(_fraction);
            bool complementary = asLogical(_complementary);
            string src;
            vector<unsigned> ids;

            if (isString(_src) && Rf_length(_src) == 1) {
                src = CHAR(STRING_ELT(_src, 0));
                EMRTrack *track = g_db->track(src.c_str());

                if (!track)
                    verror("Track %s does not exist.", src.c_str());

                track->ids(ids);
            } else {
                src = "<Ids Table>";
                try {
                    NRPoint::convert_rids(_src, &ids);
                } catch (TGLException &e) {
                    if (e.type() == typeid(NRPoint) && e.code() != NRPoint::BAD_FORMAT) 
                        throw;
                    verror("Invalid value of \"src\" argument");
                }

                sort(ids.begin(), ids.end());
                vector<unsigned>::iterator last = unique(ids.begin(), ids.end());
                ids.erase(last, ids.end());
            }

            g_db->ids_subset(ids, src.c_str(), fraction, complementary);
        }
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	rreturn(R_NilValue);
}

SEXP emr_db_subset_ids(SEXP _envir)
{
	try {
		Naryn naryn(_envir);

        if (g_db->ids_subset().empty())
            return R_NilValue;

        vector<unsigned> ids;

        for (EMRDb::IdsSubset::const_iterator iid = g_db->ids_subset().begin(); iid != g_db->ids_subset().end(); ++iid)
            ids.push_back(*iid);
        sort(ids.begin(), ids.end());
        return NRPoint::convert_ids(ids);
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	rreturn(R_NilValue);
}

SEXP emr_db_subset_info(SEXP _envir)
{
	try {
		Naryn naryn(_envir);

        if (g_db->ids_subset().empty())
            return R_NilValue;

        enum { SRC, FRACTION, COMPLEMENTARY, NUM_COLS };

        const char *COL_NAMES[NUM_COLS] = { "src", "fraction", "complementary" };
        SEXP answer, names, src, fraction, complementary;

        rprotect(answer = RSaneAllocVector(VECSXP, NUM_COLS));
        rprotect(names = RSaneAllocVector(STRSXP, NUM_COLS));
        rprotect(src = mkString(g_db->ids_subset_src().c_str()));
        rprotect(fraction = RSaneAllocVector(REALSXP, 1));
        rprotect(complementary = RSaneAllocVector(LGLSXP, 1));

        REAL(fraction)[0] = g_db->ids_subset_fraction();
        LOGICAL(complementary)[0] = g_db->ids_subset_complementary();

        for (int i = 0; i < NUM_COLS; ++i)
            SET_STRING_ELT(names, i, mkChar(COL_NAMES[i]));

        SET_VECTOR_ELT(answer, SRC, src);
        SET_VECTOR_ELT(answer, FRACTION, fraction);
        SET_VECTOR_ELT(answer, COMPLEMENTARY, complementary);

        setAttrib(answer, R_NamesSymbol, names);
        rreturn(answer);
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	rreturn(R_NilValue);
}

SEXP emr_track_names(SEXP envir)
{
	try {
		Naryn naryn(envir);

		SEXP answer;

        rprotect(answer = RSaneAllocVector(STRSXP, g_db->track_names().size()));
        for (vector<string>::const_iterator itrack_name = g_db->track_names().begin(); itrack_name < g_db->track_names().end(); ++itrack_name)
            SET_STRING_ELT(answer, itrack_name - g_db->track_names().begin(), mkChar(itrack_name->c_str()));

		return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	return R_NilValue;
}

SEXP emr_global_track_names(SEXP envir)
{
	try {
		Naryn naryn(envir);

		SEXP answer;

        rprotect(answer = RSaneAllocVector(STRSXP, g_db->global_track_names().size()));
        for (vector<string>::const_iterator itrack_name = g_db->global_track_names().begin(); itrack_name < g_db->global_track_names().end(); ++itrack_name)
            SET_STRING_ELT(answer, itrack_name - g_db->global_track_names().begin(), mkChar(itrack_name->c_str()));

		return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	return R_NilValue;
}

SEXP emr_user_track_names(SEXP _from, SEXP envir)
{
	try {
		Naryn naryn(envir);

		SEXP answer;

        rprotect(answer = RSaneAllocVector(STRSXP, g_db->user_track_names().size()));
        for (vector<string>::const_iterator itrack_name = g_db->user_track_names().begin(); itrack_name < g_db->user_track_names().end(); ++itrack_name)
            SET_STRING_ELT(answer, itrack_name - g_db->user_track_names().begin(), mkChar(itrack_name->c_str()));

		return answer;
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	return R_NilValue;
}

}

