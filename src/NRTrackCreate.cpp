#include <R.h>
#include <Rinternals.h>

#ifdef length
#undef length
#endif
#ifdef error
#undef error
#endif

#include "EMRDb.h"
#include "EMRTrack.h"
#include "naryn.h"
#include "NRTrackExpressionScanner.h"

extern "C" {

SEXP emr_track_create(SEXP _track, SEXP _space, SEXP _categorical, SEXP _expr, SEXP _stime, SEXP _etime, SEXP _iterator_policy, SEXP _keepref, SEXP _filter, SEXP _envir)
{
	try {
        Naryn naryn(_envir);

        if (!isString(_track) || Rf_length(_track) != 1)
            verror("'track' parameter must be a string");

		if (!isString(_expr) || Rf_length(_expr) != 1)
			verror("'expr' parameter must be a string");

        if (!isLogical(_categorical) || Rf_length(_categorical) != 1)
            verror("'categorical' parameter must be logical");

        if (!isString(_space) || Rf_length(_space) != 1)
            verror("'space' parameter must be a string");

        string space = { CHAR(asChar(_space)) };

        if (space != "global") {
            if (space == "user") {
                if (g_db->urootdir().empty())
                    verror("User space root directory is not set");
            } else
                verror("Invalid value of 'space' parameter");
        }

        const char *trackname = CHAR(asChar(_track));

        EMRDb::check_track_name(trackname);

        string track_filename = (space == "global" ? g_db->grootdir() : g_db->urootdir()) + string("/") + trackname + EMRDb::TRACK_FILE_EXT;
        bool categorical = asLogical(_categorical);
		NRTrackExprScanner scanner;
        EMRTrackData<float> data;

		for (scanner.begin(_expr, NRTrackExprScanner::REAL_T, _stime, _etime, _iterator_policy, _keepref, _filter); !scanner.isend(); scanner.next()) {
            data.add_data(scanner.point().id, scanner.point().timestamp, scanner.real());
			g_naryn->verify_max_data_size(data.size(), "Result");
		}

        EMRTrack::serialize(track_filename.c_str(), categorical, data);
        g_db->load_track(trackname, space == "global");
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	rreturn(R_NilValue);
}

}
