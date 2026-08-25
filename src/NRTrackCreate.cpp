#ifndef R_NO_REMAP
#  define R_NO_REMAP
#endif
#include <R.h>
#include <Rinternals.h>

#ifdef length
#undef length
#endif
#ifdef error
#undef error
#endif

#include <unistd.h>

#include "EMRDb.h"
#include "EMRTrack.h"
#include "FileUtils.h"
#include "naryn.h"
#include "NRTrackExpressionScanner.h"

extern "C" {

SEXP emr_track_create(SEXP _track, SEXP _db_id, SEXP _categorical, SEXP _expr, SEXP _stime, SEXP _etime, SEXP _iterator_policy, SEXP _keepref, SEXP _filter, SEXP _override, SEXP _envir) {
	try {
        Naryn naryn(_envir);

        if (!Rf_isString(_track) || Rf_length(_track) != 1)
            verror("'track' parameter must be a string");

		if (!Rf_isString(_expr) || Rf_length(_expr) != 1)
			verror("'expr' parameter must be a string");

        if (!Rf_isLogical(_categorical) || Rf_length(_categorical) != 1)
            verror("'categorical' parameter must be logical");

        if (!Rf_isString(_db_id) || Rf_length(_db_id) != 1)
            verror("'db_id' (space) parameter must be a string");

        string db_id = { CHAR(Rf_asChar(_db_id)) };
        bool toverride = Rf_asLogical(_override);
        bool has_overlap = false;

        auto pos = std::find(g_db->rootdirs().begin(), g_db->rootdirs().end(), CHAR(Rf_asChar(_db_id)));

        if (pos == g_db->rootdirs().end()) {
            verror("The passed DB directory is not set");
        }

        string trackname = { CHAR(Rf_asChar(_track)) };

        // Rewriting a track in its own db needs override, same as shadowing one from another
        // db. The write below is staged and renamed into place, so an in-place rewrite never
        // leaves readers with a missing or half-written track - callers no longer have to
        // emr_track.rm() first and expose that window themselves.
        if (g_db->track(trackname) && (g_db->track_info(trackname)->db_id == db_id) && !toverride){
            verror("Track %s already exists, see override argument", trackname.c_str());
        }

        // User must explicitly pass an overriding argument
        if (g_db->track(trackname) && (g_db->track_info(trackname)->db_id != db_id) && !toverride){
            verror("Track %s already exists in db %s, see override argument", trackname.c_str(), g_db->track_info(trackname)->db_id.c_str());
        }
        
        //Override was passed and a track to override was found
        if (g_db->track(trackname) && (g_db->track_info(trackname)->db_id != db_id) && toverride){
            int curr_db_idx = g_db->get_db_idx(g_db->track_info(trackname)->db_id);
            int req_db_idx = g_db->get_db_idx(db_id);

            //Do not allowe writing a track with the same name to a db dir
            //With lower priority. This will only confuse the user.
            if (curr_db_idx > req_db_idx) {
                verror("Can not write track to %s the same track already exists in %s. This kind of write has no effect.", db_id.c_str(), g_db->track_info(trackname)->db_id.c_str());
            }

            has_overlap = true;
        }
        //can not override patients.dob
        if (g_db->track(trackname) && (strcmp(trackname.c_str(), g_db->dob_trackname()) == 0) && (g_db->get_db_idx(db_id) != 0)) {
            verror("Can not override %s track", g_db->dob_trackname());
        }
            
        EMRDb::check_track_name(trackname);

        string track_filename = db_id + string("/") + trackname + EMRDb::TRACK_FILE_EXT;
        bool categorical = Rf_asLogical(_categorical);
		NRTrackExprScanner scanner;
        EMRTrackData<float> data;

		for (scanner.begin(_expr, NRTrackExprScanner::REAL_T, _stime, _etime, _iterator_policy, _keepref, _filter); !scanner.isend(); scanner.next()) {
            data.add(scanner.point().id, scanner.point().timestamp, scanner.real());
			g_naryn->verify_max_data_size(data.data.size(), "Result");
		}

        // Rewriting in place really writes this file, unlike shadowing a track from another db,
        // so honour "read-only" the way emr_track.rm/mv/addto do. Nothing below would stop it:
        // read-only is a mode on the track file, and rename(2) does not consult the target's
        // mode, only the directory's.
        if (access(track_filename.c_str(), F_OK) == 0 && access(track_filename.c_str(), W_OK) != 0)
            verror("Cannot override track %s: it is read-only.", trackname.c_str());

        // Staged and renamed into place, so a concurrent reader sees either the complete previous
        // track or the complete new one - never a partial or absent file.
        FileUtils::atomic_write(track_filename.c_str(), [&](const char *path) {
            EMRTrack::serialize(path, categorical ? EMRTrack::IS_CATEGORICAL : 0, data);
        });

        if (has_overlap) {
            g_db->unload_track(trackname.c_str(), true, true);
        }

        g_db->load_track(trackname.c_str(), db_id);

	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	rreturn(R_NilValue);
}

}
