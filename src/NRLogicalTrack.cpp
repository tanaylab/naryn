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
SEXP emr_create_logical(SEXP _track, SEXP _src, SEXP _values, SEXP _envir) {
    try {
        Naryn naryn(_envir);

        if (!isString(_track) || Rf_length(_track) != 1)
            verror("'track' argument must be a string");

        string sourcename = {CHAR(asChar(_src))};
        EMRTrack *source_track = g_db->track(sourcename);
        if (!source_track)
            verror("Source track %s not found", sourcename.c_str());

        if (!source_track->is_categorical()) {
            verror("Source track is not categorical");
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
            g_db->add_logical_track(trackname.c_str(), sourcename.c_str(),     true);
        } else {
            int num_values = Rf_length(_values);
            vector<int> values(num_values);
            values.assign(REAL(_values), REAL(_values) + num_values);

            vdebug("values: ");
            for (auto i = values.begin(); i != values.end(); ++i)
                vdebug("%d ", *i);

            g_db->add_logical_track(trackname.c_str(), sourcename.c_str(), values, true);
        }

    } catch (TGLException &e) {
        rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

    rreturn(R_NilValue);
}

SEXP emr_remove_logical(SEXP _track, SEXP _envir) {
    try {
        Naryn naryn(_envir);

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

        g_db->remove_logical_track(trackname.c_str(), true);
        vdebug("Removed logical track: %s", trackname.c_str());

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
                size_t index = iid - track->values.begin();
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
            size_t pos = 0;

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

        
        
        for (int is_global = 0; is_global < 2; ++is_global) {
            for (vector<string>::const_iterator itrack =
                        g_db->track_names(is_global).begin();
                    itrack < g_db->track_names(is_global).end(); ++itrack) {
                size_t pos = 0;

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

        // retrieve virtual track names (virtual track names are burried in a
        // list of lists)
        rprotect(emr_vtracks = findVar(install("EMR_VTRACKS"), g_naryn->env()));

        if (!isNull(emr_vtracks) && !isSymbol(emr_vtracks)) {
            SEXP roots = getAttrib(emr_vtracks, R_NamesSymbol);

            if (!isVector(emr_vtracks) ||
                Rf_length(emr_vtracks) && !isString(roots) ||
                Rf_length(roots) != Rf_length(emr_vtracks))
                verror(
                    "Invalid format of EMR_VTRACKS variable (1).\n"
                    "To continue working with virtual tracks please remove "
                    "this variable from the environment.");

            for (int i = 0; i < Rf_length(roots); ++i) {
                if (g_db->grootdir() == CHAR(STRING_ELT(roots, i)) ||
                    g_db->urootdir() == CHAR(STRING_ELT(roots, i))) {
                    vtracks.push_back(VECTOR_ELT(emr_vtracks, i));
                    SEXP vtracknames = getAttrib(vtracks.back(), R_NamesSymbol);

                    if (!isVector(vtracks.back()) ||
                        Rf_length(vtracks.back()) && !isString(vtracknames) ||
                        Rf_length(vtracknames) != Rf_length(vtracks.back()))
                        verror(
                            "Invalid format of EMR_VTRACKS variable (2).\n"
                            "To continue working with virtual tracks please "
                            "remove this variable from the environment.");

                    rvtracknames.push_back(vtracknames);
                }
            }
        }

        // look for virtual tracks
        for (size_t i = 0; i < vtracks.size(); ++i) {
            if (isString(rvtracknames[i])) {
                for (int itrack = 0; itrack < Rf_length(rvtracknames[i]);
                     ++itrack) {
                    string track = CHAR(STRING_ELT(rvtracknames[i], itrack));
                    size_t pos = 0;

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
}