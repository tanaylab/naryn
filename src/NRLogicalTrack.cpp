#include <stdlib.h>
#include <unistd.h>

#include <algorithm>

#include "EMRDb.h"
#include "EMRLogicalTrack.h"
#include "EMRTrack.h"
#include "FileUtils.h"
#include "NRTrackExpressionVars.h"
#include "naryn.h"
#include "strutil.h"

extern "C" {

// create a logical track by adding it to the EMRDb object (g_db)
// the logical tracks would be saved as an unordered_map mapping name to
// an EMRLogicalTrack object (m_logical_tracks).
// In addition - we save the names of all logical tracks as a vector of
// strings at m_logical_track_names;
SEXP emr_create_logical(SEXP _track, SEXP _src, SEXP _values, SEXP _update, SEXP _envir) {
    try {
        Naryn naryn(_envir, asLogical(_update));


        if (!isString(_track) || Rf_length(_track) != 1)
            verror("'track' argument must be a string");


        if (!isLogical(_update)){
            verror("update argument must be a logical value");
        }

        string sourcename = {CHAR(asChar(_src))};
        const EMRDb::TrackInfo *source_track_info = g_db->track_info(sourcename);
        if (!source_track_info){
            verror("Source track %s not found", sourcename.c_str());
        }       
        
        // check if the source exists in the global db
        if (source_track_info->db_id != g_db->grootdir()) {
            if (std::find(source_track_info->dbs.begin(),
                          source_track_info->dbs.end(),
                          g_db->grootdir()) == source_track_info->dbs.end()) {
                verror("Source track %s is not in the global db",
                       sourcename.c_str());
            }
        }

        EMRTrack *source_track = g_db->track(sourcename);
        if (!source_track->is_categorical() && !isNull(_values)) {
            verror("Source track is not categorical and values were passed");
        }

        string trackname = {CHAR(asChar(_track))};
        EMRDb::check_track_name(trackname);

        if (g_db->logical_track(trackname)) {
            verror("Track %s already exists",
                   trackname.c_str());
        }

        if (g_db->track(trackname)) {
            verror("Track %s already exists", trackname.c_str());
        }

        if (isNull(_values) || g_db->track(trackname)){ // no values
            g_db->add_logical_track(trackname.c_str(), sourcename.c_str(), true, asLogical(_update));
        } else {
            int num_values = Rf_length(_values);
            vector<int> values(num_values);
            
            if (isReal(_values)){
                values.assign(REAL(_values), REAL(_values) + num_values);
            } else if (isInteger(_values)){
                values.assign(INTEGER(_values), INTEGER(_values) + num_values);
            } else {
                verror("invalid values parameter (it is not numeric)");
            }

            if (g_naryn->debug()) {
                vdebug(7, "values: ");
                for (auto i = values.begin(); i != values.end(); ++i)
                    vdebug(7, "%d ", *i);
            }

            g_db->add_logical_track(trackname.c_str(), sourcename.c_str(),
                                    values, true, asLogical(_update));
        }

    } catch (TGLException &e) {
        rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

    rreturn(R_NilValue);
}


SEXP update_logical_tracks_file(SEXP _envir) {
    try {
        Naryn naryn(_envir, false);
        g_db->update_logical_tracks_file();        
    } catch (TGLException &e) {
        rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

    rreturn(R_NilValue);
}

SEXP emr_remove_logical(SEXP _track, SEXP _update, SEXP _envir) {
    try {
        Naryn naryn(_envir, asLogical(_update));

        if (!isString(_track) || Rf_length(_track) != 1)
            verror("'track' argument must be a string");

        string trackname = {CHAR(asChar(_track))};

        if (!g_db->logical_track(trackname)) {
            if (g_db->track(trackname)) {
               verror("Track %s is a physical track",trackname.c_str());
            }
            verror("Track %s doesn't exist as a logical track",
                   trackname.c_str());
        }
        g_db->remove_logical_track(trackname.c_str(), asLogical(_update));
        vdebug(7, "Removed logical track: %s", trackname.c_str());

    } catch (TGLException &e) {
        rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

    rreturn(R_NilValue);
}

SEXP emr_is_logical(SEXP _track, SEXP _envir) {
    try {
        Naryn naryn(_envir);

        if (!isString(_track) || Rf_length(_track) != 1)
            verror("'track' argument must be a string");

        string trackname = {CHAR(asChar(_track))};
        if (g_db->logical_track(trackname)) {
            rreturn(ScalarLogical(true));
        }

        rreturn(ScalarLogical(false));

    } catch (TGLException &e) {
        rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

    rreturn(R_NilValue);
}

SEXP emr_logical_track_info(SEXP _track, SEXP _envir) {
    try {
        Naryn naryn(_envir);

        // check the arguments
        if (!isString(_track) || Rf_length(_track) != 1)
            verror("Track argument is not a string");

        enum { SOURCE, VALUES, NUM_COLS };

        const char *COL_NAMES[NUM_COLS] = {"source", "values"};

        const char *trackname = CHAR(STRING_ELT(_track, 0));
        SEXP answer;
        SEXP names, rsource, rvalues;

        const EMRLogicalTrack *track = g_db->logical_track(trackname);        

        if (!track) {
            if (g_db->track(trackname)) {
                verror("Track %s is a physical track", trackname);
            }

            verror("Logical track %s does not exist", trackname);
        }

        rprotect(answer = RSaneAllocVector(VECSXP, NUM_COLS));
        rprotect(names = RSaneAllocVector(STRSXP, NUM_COLS));

        rprotect(rsource = RSaneAllocVector(STRSXP, 1));
        SET_STRING_ELT(rsource, 0, mkChar(track->source.c_str()));

        unsigned num_values = track->values.size();        

        if (num_values > 0) {
            rprotect(rvalues = RSaneAllocVector(INTSXP, num_values));

            for (vector<int>::const_iterator iid = track->values.begin();
                 iid != track->values.end(); ++iid) {
                uint64_t index = iid - track->values.begin();
                INTEGER(rvalues)[index] = *iid;                
            }
        } else {
            rprotect(rvalues = RSaneAllocVector(NILSXP, 1));
        }

        for (int i = 0; i < NUM_COLS; ++i)
            SET_STRING_ELT(names, i, mkChar(COL_NAMES[i]));

        SET_VECTOR_ELT(answer, SOURCE, rsource);
        SET_VECTOR_ELT(answer, VALUES, rvalues);
        setAttrib(answer, R_NamesSymbol, names);

        return answer;
    } catch (TGLException &e) {
        rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
    return R_NilValue;
}

SEXP emr_expr_logical_tracks(SEXP _expr, SEXP _envir) {
    try {
        Naryn naryn(_envir);

        // check the arguments
        if (!isString(_expr) || Rf_length(_expr) != 1)
            verror("Expression argument is not a string");

        string expr(CHAR(STRING_ELT(_expr, 0)));
        vector<string> logical_tracks;

        vector<string> logical_track_names = g_db->logical_track_names();

        for (auto itrack =
                 logical_track_names.begin();
             itrack < logical_track_names.end(); ++itrack) {
            uint64_t pos = 0;

            while ((pos = expr.find(*itrack, pos)) != string::npos) {                
                if (NRTrackExpressionVars::is_var(expr, pos, pos + itrack->size())){                
                    logical_tracks.push_back(string(*itrack));
                    break;
                }
                pos += itrack->size();
            }
        }

        SEXP answer;

        rprotect(answer = RSaneAllocVector(STRSXP, logical_tracks.size()));
        for (auto itrack_name = logical_tracks.begin();
             itrack_name < logical_tracks.end(); ++itrack_name)
            SET_STRING_ELT(answer, itrack_name - logical_tracks.begin(),
                           mkChar(itrack_name->c_str()));

        return answer;
    } catch (TGLException &e) {
        rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
    return R_NilValue;
}

SEXP emr_expr_physical_tracks(SEXP _expr, SEXP _envir) {
    try {
        Naryn naryn(_envir);

        // check the arguments
        if (!isString(_expr) || Rf_length(_expr) != 1)
            verror("Expression argument is not a string");

        string expr(CHAR(STRING_ELT(_expr, 0)));
        vector<string> tracks;
     
        for(auto db_id = g_db->rootdirs().begin(); db_id != g_db->rootdirs().end(); db_id++){
            for (vector<string>::const_iterator itrack = g_db->track_names(*db_id).begin(); itrack < g_db->track_names(*db_id).end(); ++itrack) {

                uint64_t pos = 0;

                while ((pos = expr.find(*itrack, pos)) != string::npos) {
                  if (NRTrackExpressionVars::is_var(expr, pos, pos + itrack->size())){                 
                            tracks.push_back(string(*itrack));
                            break;
                        }
                        pos += itrack->size();
                }
            }        
        }             

        SEXP answer;

        rprotect(answer = RSaneAllocVector(STRSXP, tracks.size()));
        for (auto itrack_name = tracks.begin();
                itrack_name < tracks.end(); ++itrack_name)
            SET_STRING_ELT(answer, itrack_name - tracks.begin(),
                            mkChar(itrack_name->c_str()));

        return answer;
        }
        catch (TGLException &e) {
            rerror("%s", e.msg());
        }
        catch (const bad_alloc &e) {
            rerror("Out of memory");
        }
    return R_NilValue;
}

SEXP emr_expr_virtual_tracks(SEXP _expr, SEXP _envir) {
    try {
        Naryn naryn(_envir);

        // check the arguments
        if (!isString(_expr) || Rf_length(_expr) != 1)
            verror("Expression argument is not a string");

        string expr(CHAR(STRING_ELT(_expr, 0)));
        vector<string> tracks;
        SEXP emr_vtracks = R_NilValue;

        SEXPCleaner emr_vtracks_cleaner(emr_vtracks);
        vector<SEXP> rvtracknames;
        vector<SEXP> vtracks;

        // retrieve virtual track names (virtual tracks are at a variable called EMR_VTRACKS in the .naryn environment)        
        rprotect(emr_vtracks = findVar(install("EMR_VTRACKS"), findVar(install(".naryn"), g_naryn->env())));

        if (!isNull(emr_vtracks) && !isSymbol(emr_vtracks)) {
            if (!isVector(emr_vtracks)){
                verror(
                    "Invalid format of EMR_VTRACKS variable (1).\n"
                    "To continue working with virtual tracks please remove "
                    "this variable from the .naryn environment.");
            }

            vtracks.push_back(emr_vtracks);
            SEXP vtracknames = getAttrib(vtracks.back(), R_NamesSymbol);

            if (!isVector(vtracks.back()) ||
                (Rf_length(vtracks.back()) && !isString(vtracknames)) ||
                (Rf_length(vtracknames) != Rf_length(vtracks.back()))){
                verror(
                    "Invalid format of EMR_VTRACKS variable (2).\n"
                    "To continue working with virtual tracks please "
                    "remove this variable from the .naryn environment.");
            }

            rvtracknames.push_back(vtracknames);
        }

        // look for virtual tracks
        for (uint64_t i = 0; i < vtracks.size(); ++i) {
            if (isString(rvtracknames[i])) {
                for (int itrack = 0; itrack < Rf_length(rvtracknames[i]);
                     ++itrack) {
                    string track = CHAR(STRING_ELT(rvtracknames[i], itrack));
                    uint64_t pos = 0;

                    while ((pos = expr.find(track, pos)) != string::npos) {
                        if (NRTrackExpressionVars::is_var(expr, pos, pos + track.size())){
                            tracks.push_back(track);
                            break;
                        }
                        pos += track.size();
                    }
                }
            }
        }

        SEXP answer;

        rprotect(answer = RSaneAllocVector(STRSXP, tracks.size()));
        for (auto itrack_name = tracks.begin(); itrack_name < tracks.end();
             ++itrack_name)
            SET_STRING_ELT(answer, itrack_name - tracks.begin(),
                           mkChar(itrack_name->c_str()));

        return answer;
    } catch (TGLException &e) {
        rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
    return R_NilValue;
}


SEXP emr_ltrack_dependencies(SEXP _track, SEXP _envir) {
    try {
        Naryn naryn(_envir);
        vector<string> logical_dependent_tracks;

        if (!isString(_track) || Rf_length(_track) != 1)
            verror("'track' argument must be a string");

        string trackname = {CHAR(asChar(_track))};
        logical_dependent_tracks = g_db->dependent_logical_tracks(trackname);

        SEXP answer;

        rprotect(answer = RSaneAllocVector(STRSXP, logical_dependent_tracks.size()));
        for (auto itrack_name = logical_dependent_tracks.begin(); itrack_name < logical_dependent_tracks.end();
             ++itrack_name)
            SET_STRING_ELT(answer, itrack_name - logical_dependent_tracks.begin(),
                           mkChar(itrack_name->c_str()));

        return answer;

    } catch (TGLException &e) {
        rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

    rreturn(R_NilValue);
}


}

