#include <algorithm>

#include "naryn.h"
#include "NRProgressReporter.h"
#include "NRTrack.h"
#include "NRTrackExpressionScanner.h"

#include <R.h>
#include <Rinternals.h>

#ifdef length
#undef length
#endif

extern "C" {

SEXP emr_ids_dist(SEXP _ids, SEXP _tracks, SEXP _envir)
{
	try {
        Naryn naryn(_envir);

        if (!isString(_tracks) || Rf_length(_tracks) < 1)
            verror("'tracks' argument must be a vector of strings");

        vector<unsigned> ids;
        vector<NRTrack *> tracks;
        vector<unsigned> res;
        NRProgressReporter progress;

        if (isString(_ids) && Rf_length(_ids) == 1) { // it's a track name
            const char *trackname = CHAR(STRING_ELT(_ids, 0));
            NRTrack *track = g_db->track(trackname);
            if (!track)
                verror("Track %s does not exist", trackname);
            track->ids(ids);
        } else {  // IDs or Id-Time table
            NRPoint::convert_rids(_ids, &ids);
            sort(ids.begin(), ids.end());
        }

        ids.erase(unique(ids.begin(), ids.end()), ids.end());

        for (int i = 0; i < Rf_length(_tracks); ++i) {
            const char *trackname = CHAR(STRING_ELT(_tracks, i));
            NRTrack *track = g_db->track(trackname);
            if (!track)
                verror("Track %s does not exist", trackname);
            tracks.push_back(track);
        }

        progress.init(tracks.size(), 1);
        for (vector<NRTrack *>::const_iterator itrack = tracks.begin(); itrack != tracks.end(); ++itrack) {
            res.push_back((*itrack)->count_ids(ids));
            progress.report(1);
            check_interrupt();
        }
        progress.report_last();

        // pack the answer
        SEXP answer;

        rprotect(answer = allocVector(INTSXP, res.size()));

        for (vector<unsigned>::const_iterator ires = res.begin(); ires != res.end(); ++ires)
            INTEGER(answer)[ires - res.begin()] = *ires;

        setAttrib(answer, R_NamesSymbol, _tracks);

        rreturn(answer);
	} catch (TGLException &e) {
		rerror("%s", e.msg());
	}
	rreturn(R_NilValue);
}

SEXP emr_ids_dist_with_iterator(SEXP _ids, SEXP _tracks, SEXP _stime, SEXP _etime, SEXP _filter, SEXP _envir)
{
	try {
        Naryn naryn(_envir);

        if (!isString(_tracks) || Rf_length(_tracks) < 1)
            verror("'tracks' argument must be a vector of strings");

        vector<unsigned> ids;
        vector<NRTrack *> tracks;
        vector<unsigned> res;
        SEXP riterator;
        NRProgressReporter progress;

        if (isString(_ids) && Rf_length(_ids) == 1) { // it's a track name
            const char *trackname = CHAR(STRING_ELT(_ids, 0));
            NRTrack *track = g_db->track(trackname);
            if (!track)
                verror("Track %s does not exist", trackname);
            track->ids(ids);
        } else {  // IDs or Id-Time table
            NRPoint::convert_rids(_ids, &ids);
            sort(ids.begin(), ids.end());
        }

        ids.erase(unique(ids.begin(), ids.end()), ids.end());

        for (int i = 0; i < Rf_length(_tracks); ++i) {
            const char *trackname = CHAR(STRING_ELT(_tracks, i));
            NRTrack *track = g_db->track(trackname);
            if (!track)
                verror("Track %s does not exist", trackname);
            tracks.push_back(track);
        }

        rprotect(riterator = allocVector(STRSXP, 1));

        progress.init(tracks.size(), 1);
        for (auto itrack = tracks.begin(); itrack != tracks.end(); ++itrack) {
            unordered_set<unsigned> used_ids;
            NRTrackExprScanner scanner;

            SET_STRING_ELT(riterator, 0, STRING_ELT(_tracks, itrack - tracks.begin()));
            scanner.report_progress(false);
            for (scanner.begin(riterator, NRTrackExprScanner::REAL_T, _stime, _etime, riterator, ScalarLogical(true), _filter); !scanner.isend(); scanner.next())
                used_ids.insert(scanner.point().id);

            res.push_back(used_ids.size());
            progress.report(1);
            check_interrupt();
        }
        progress.report_last();

        // pack the answer
        SEXP answer;

        rprotect(answer = allocVector(INTSXP, res.size()));

        for (vector<unsigned>::const_iterator ires = res.begin(); ires != res.end(); ++ires)
            INTEGER(answer)[ires - res.begin()] = *ires;

        setAttrib(answer, R_NamesSymbol, _tracks);

        rreturn(answer);
	} catch (TGLException &e) {
		rerror("%s", e.msg());
	}
	rreturn(R_NilValue);
}

SEXP emr_ids_vals_dist(SEXP _ids, SEXP _tracks, SEXP _stime, SEXP _etime, SEXP _filter, SEXP _envir)
{
    typedef unordered_map<int64_t, unsigned> Val2Count;

    struct ValCount {
        double val;
        size_t count;

        ValCount(double _val, size_t _count) : val(_val), count(_count) {}
        bool operator<(const ValCount &o) const { return val < o.val; }
    };

	try {
        Naryn naryn(_envir);

        if (!isString(_tracks) || Rf_length(_tracks) < 1)
            verror("'tracks' argument must be a vector of strings");

        unordered_set<unsigned> ids;
        vector<unsigned> ids_vec;
        vector<NRTrack *> tracks;
        vector<Val2Count> res;
        NRProgressReporter progress;
        SEXP riterator;
        size_t tot_num_vals = 0;

        if (isString(_ids) && Rf_length(_ids) == 1) { // it's a track name
            const char *trackname = CHAR(STRING_ELT(_ids, 0));
            NRTrack *track = g_db->track(trackname);

            if (!track)
                verror("Track %s does not exist", trackname);

            track->ids(ids_vec);
        } else  // IDs or Id-Time table
            NRPoint::convert_rids(_ids, &ids_vec);

        for (vector<unsigned>::const_iterator iid = ids_vec.begin(); iid != ids_vec.end(); ++iid)
            ids.insert(*iid);

        for (int i = 0; i < Rf_length(_tracks); ++i) {
            const char *trackname = CHAR(STRING_ELT(_tracks, i));
            NRTrack *track = g_db->track(trackname);

            if (!track)
                verror("Track %s does not exist", trackname);

            if (!track->is_categorial())
                verror("Track %s is not categorial", trackname);

            tracks.push_back(track);
        }

        res.resize(Rf_length(_tracks));

        rprotect(riterator = allocVector(STRSXP, 1));

        progress.init(tracks.size(), 1);
        for (vector<NRTrack *>::const_iterator itrack = tracks.begin(); itrack != tracks.end(); ++itrack) {
            NRTrackExprScanner scanner;
            Val2Count &val2count = res[itrack - tracks.begin()];
            vector<double> unique_vals;
            unordered_set<pair<size_t, size_t>> idval;

            (*itrack)->unique_vals(unique_vals);
            tot_num_vals += unique_vals.size();

            for (vector<double>::const_iterator ival = unique_vals.begin(); ival != unique_vals.end(); ++ival)
                val2count[*(int64_t *)&*ival] = 0;

            SET_STRING_ELT(riterator, 0, STRING_ELT(_tracks, itrack - tracks.begin()));

            scanner.report_progress(false);
            for (scanner.begin(riterator, NRTrackExprScanner::REAL_T, _stime, _etime, riterator, ScalarLogical(true), _filter); !scanner.isend(); scanner.next()) {
                double val = scanner.real();
                if (val != -1 && ids.find(scanner.point().id) != ids.end() &&
                    idval.find(pair<size_t, size_t>(*(size_t *)&val, (size_t)scanner.point().id)) == idval.end())
                {
                    ++val2count[*(int64_t *)&val];
                    idval.insert(pair<size_t, size_t>(*(size_t *)&val, (size_t)scanner.point().id));
                }
            }
            progress.report(1);
            check_interrupt();
        }
        progress.report_last();

        // pack the answer
        enum { TRACK, VAL, COUNT, NUM_COLS };

        const char *COL_NAMES[NUM_COLS] = { "track", "val", "count" };

        SEXP answer;
        SEXP rtracks, rtracks_idx, rvals, rcounts;
        SEXP row_names;
        SEXP col_names;

        rprotect(answer = allocVector(VECSXP, NUM_COLS));

        SET_VECTOR_ELT(answer, TRACK, (rtracks_idx = allocVector(INTSXP, tot_num_vals)));
        SET_VECTOR_ELT(answer, VAL, (rvals = allocVector(REALSXP, tot_num_vals)));
        SET_VECTOR_ELT(answer, COUNT, (rcounts = allocVector(INTSXP, tot_num_vals)));

        setAttrib(rtracks_idx, R_LevelsSymbol, (rtracks = allocVector(STRSXP, tracks.size())));
        setAttrib(rtracks_idx, R_ClassSymbol, mkString("factor"));

        setAttrib(answer, R_NamesSymbol, (col_names = allocVector(STRSXP, NUM_COLS)));
        setAttrib(answer, R_ClassSymbol, mkString("data.frame"));
        setAttrib(answer, R_RowNamesSymbol, (row_names = allocVector(INTSXP, tot_num_vals)));

        for (vector<NRTrack *>::const_iterator itrack = tracks.begin(); itrack != tracks.end(); ++itrack)
            SET_STRING_ELT(rtracks, itrack - tracks.begin(), mkChar((*itrack)->name()));

        for (int i = 0; i < NUM_COLS; i++)
            SET_STRING_ELT(col_names, i, mkChar(COL_NAMES[i]));

        size_t idx = 0;
        vector<ValCount> valcounts;

        for (vector<Val2Count>::const_iterator ires = res.begin(); ires != res.end(); ++ires) {
            const Val2Count &val2count = *ires;

            valcounts.clear();
            for (Val2Count::const_iterator ival2count = val2count.begin(); ival2count != val2count.end(); ++ival2count)
                valcounts.push_back(ValCount(*(double *)&ival2count->first, ival2count->second));

            sort(valcounts.begin(), valcounts.end());

            for (vector<ValCount>::const_iterator ivalcount = valcounts.begin(); ivalcount != valcounts.end(); ++ivalcount) {
                INTEGER(rtracks_idx)[idx] = ires - res.begin() + 1;
                REAL(rvals)[idx] = ivalcount->val;
                INTEGER(rcounts)[idx] = ivalcount->count;
                INTEGER(row_names)[idx] = idx + 1;
                ++idx;
            }
        }

        rreturn(answer);
	} catch (TGLException &e) {
		rerror("%s", e.msg());
	}
	rreturn(R_NilValue);
}

}

