#include <dirent.h>
#include <limits.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <numeric>

#include "EMRDb.h"
#include "EMRProgressReporter.h"
#include "EMRTrack.h"
#include "NRPoint.h"
#include "naryn.h"

extern "C" {

SEXP emr_dbinit(SEXP _dbdirs, SEXP _load_on_demand, SEXP _do_load, SEXP envir) {
    try {
        Naryn naryn(envir, false);

        if (!isLogical(_do_load) || Rf_length(_do_load) != 1)
            verror("'do_reload' argument must be a logical value");

        if (Rf_length(_dbdirs) != Rf_length(_load_on_demand)) {
            verror("'db_dirs' and 'load_on_demand' arguments must have matching length");
        }

        vector<string> dbdirs; 
        vector<bool> load_on_demand; 

        if (!isNull(_dbdirs)) {
            for (int i = 0; i < Rf_length(_dbdirs); i++){
                dbdirs.push_back(CHAR(STRING_ELT(_dbdirs, i)));
            }
        }

        if (!isNull(_load_on_demand)) {
            for (int i = 0; i < Rf_length(_load_on_demand); i++){
                load_on_demand.push_back(LOGICAL_ELT(_load_on_demand, i));
            }
        }

        if (!g_db) g_db = new EMRDb;

        g_db->init(dbdirs, load_on_demand, asLogical(_do_load));

    } catch (TGLException &e) {
        delete g_db;
        g_db = NULL;
        rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

    return R_NilValue;
}

SEXP emr_dbreload(SEXP envir) {
    try {
        Naryn naryn(envir);

        g_db->reload();
    } catch (TGLException &e) {
        delete g_db;
        g_db = NULL;
        rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

    return R_NilValue;
}

SEXP emr_dbunload(SEXP envir) {
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

SEXP emr_db_subset(SEXP _src, SEXP _fraction, SEXP _complementary,
                   SEXP _envir) {
    try {
        Naryn naryn(_envir);

        if (isNull(_src))
            g_db->clear_ids_subset(false);
        else {
            if (!isReal(_fraction) || Rf_length(_fraction) != 1)
                verror("\"fraction\" argument must be a numeric value");

            if (!isLogical(_complementary) || Rf_length(_complementary) != 1 ||
                LOGICAL(_complementary)[0] == NA_LOGICAL)
                verror("\"complementary\" argument must be a logical value");

            double fraction = asReal(_fraction);
            bool complementary = asLogical(_complementary);
            string src;
            vector<unsigned> ids;

            if (isString(_src) && Rf_length(_src) == 1) {
                src = CHAR(STRING_ELT(_src, 0));
                EMRTrack *track = g_db->track(src.c_str());
                const EMRLogicalTrack *logical_track =
                    g_db->logical_track(src.c_str());

                if (!track && !logical_track)
                    verror("Track %s does not exist", src.c_str());

                if (logical_track) {
                    track = g_db->track(logical_track->source.c_str());
                    unordered_set<double> vals(logical_track->values.begin(),
                                               logical_track->values.end());
                    track->ids(ids, vals);
                } else {
                    track->ids(ids);
                }
            } else {
                src = "<Ids Table>";
                try {
                    NRPoint::convert_rids(_src, &ids);
                } catch (TGLException &e) {
                    if (e.type() == typeid(NRPoint) &&
                        e.code() != NRPoint::BAD_FORMAT)
                        throw;
                    verror("Invalid value of \"src\" argument");
                }

                sort(ids.begin(), ids.end());
                vector<unsigned>::iterator last =
                    unique(ids.begin(), ids.end());
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

SEXP emr_db_subset_ids(SEXP _envir) {
    try {
        Naryn naryn(_envir);

        if (g_db->ids_subset().empty()) return R_NilValue;

        vector<unsigned> ids;

        for (EMRDb::IdsSubset::const_iterator iid = g_db->ids_subset().begin();
             iid != g_db->ids_subset().end(); ++iid)
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

SEXP emr_db_subset_info(SEXP _envir) {
    try {
        Naryn naryn(_envir);

        if (g_db->ids_subset().empty()) return R_NilValue;

        enum { SRC, FRACTION, COMPLEMENTARY, NUM_COLS };

        const char *COL_NAMES[NUM_COLS] = {"src", "fraction", "complementary"};
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

SEXP emr_track_names(SEXP envir) {
    try {
        Naryn naryn(envir);

        SEXP answer;

        vector<int> track_names_sizes;
        vector<string> rootdirs = g_db->rootdirs();

        for (int db_idx = 0; db_idx < (int)rootdirs.size(); db_idx++) {
            track_names_sizes.push_back(g_db->track_names(rootdirs[db_idx]).size());
        }

        int tracks_size = std::accumulate(track_names_sizes.begin(),
                                          track_names_sizes.end(), 
                                          decltype(track_names_sizes)::value_type(0));

        rprotect(answer = RSaneAllocVector(STRSXP, tracks_size));

        size_t idx = 0;

        for (int db_idx = 0; db_idx < (int)rootdirs.size(); db_idx++) {
            for ( auto track_name : g_db->track_names(rootdirs[db_idx]) )
                SET_STRING_ELT(answer, idx++, mkChar(track_name.c_str()));
        }

        return answer;
        
    } catch (TGLException &e) {
        rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

    return R_NilValue;
}

SEXP emr_track_db_names(SEXP _db_id, SEXP envir) {
      try {
        Naryn naryn(envir);

        SEXP answer;
        string db_id = CHAR(asChar(_db_id));

        rprotect(answer = RSaneAllocVector(STRSXP, g_db->track_names(db_id).size()));
        for (auto itrack_name = g_db->track_names(db_id).begin();
             itrack_name < g_db->track_names(db_id).end(); ++itrack_name)
            SET_STRING_ELT(answer,
                           itrack_name - g_db->track_names(db_id).begin(),
                           mkChar(itrack_name->c_str()));

        return answer;
    } catch (TGLException &e) {
        rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

    return R_NilValue;
}

SEXP emr_logical_track_names(SEXP envir) {
    try {
        Naryn naryn(envir);

        SEXP answer;
        vector<string> ltrack_names = g_db->logical_track_names();

        rprotect(answer = RSaneAllocVector(STRSXP, ltrack_names.size()));
        for (auto itrack_name = ltrack_names.begin();
             itrack_name < ltrack_names.end(); ++itrack_name)
            SET_STRING_ELT(answer, itrack_name - ltrack_names.begin(),
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
