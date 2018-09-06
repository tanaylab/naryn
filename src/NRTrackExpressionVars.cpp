#include <algorithm>
#include <unistd.h>

#include "naryn.h"
#include "NRDb.h"
#include "NRIteratorFilter.h"
#include "NRTrackExpressionScanner.h"
#include "NRTrackExpressionVars.h"

#include <R.h>
#include <Rinternals.h>

#ifdef length
#undef length
#endif

NRTrackExpressionVars::NRTrackExpressionVars()
{
	m_imanagers.reserve(10000);
}

void NRTrackExpressionVars::parse_exprs(const vector<string> &track_exprs, bool only_check, unsigned stime, unsigned etime)
{
	SEXP emr_vtracks = R_NilValue;
    vector<SEXP> rvtracknames;
	vector<SEXP> vtracks;

	// retrieve virtual track names (virtual track names are burried in a list of lists)
	rprotect(emr_vtracks = findVar(install("EMR_VTRACKS"), g_naryn->env()));

	if (!isNull(emr_vtracks) && !isSymbol(emr_vtracks)) {
		SEXP roots = getAttrib(emr_vtracks, R_NamesSymbol);

		if (!isVector(emr_vtracks) || Rf_length(emr_vtracks) && !isString(roots) || Rf_length(roots) != Rf_length(emr_vtracks))
			verror("Invalid format of EMR_VTRACKS variable (1).\n"
				   "To continue working with virtual tracks please remove this variable from the environment.");

		for (int i = 0; i < Rf_length(roots); ++i) {
			if (g_db->grootdir() == CHAR(STRING_ELT(roots, i)) || g_db->urootdir() == CHAR(STRING_ELT(roots, i))) {
				vtracks.push_back(VECTOR_ELT(emr_vtracks, i));
				SEXP vtracknames = getAttrib(vtracks.back(), R_NamesSymbol);

				if (!isVector(vtracks.back()) || Rf_length(vtracks.back()) && !isString(vtracknames) || Rf_length(vtracknames) != Rf_length(vtracks.back()))
					verror("Invalid format of EMR_VTRACKS variable (2).\n"
				           "To continue working with virtual tracks please remove this variable from the environment.");

				rvtracknames.push_back(vtracknames);
			}
		}
	}

	for (vector<string>::const_iterator iexpr = track_exprs.begin(); iexpr != track_exprs.end(); ++iexpr) {
		// look for track names
		for (vector<string>::const_iterator itrack = g_db->track_names().begin(); itrack < g_db->track_names().end(); ++itrack) {
			size_t pos = 0;

			while ((pos = iexpr->find(*itrack, pos)) != string::npos) {
				if (is_var(*iexpr, pos, pos + itrack->size())) {
					add_track_var(*itrack);
					break;
				}
				pos += itrack->size();
			}
		}

		// look for virtual tracks
        for (size_t i = 0; i < vtracks.size(); ++i) {
    		if (isString(rvtracknames[i])) {
    			for (int itrack = 0; itrack < Rf_length(rvtracknames[i]); ++itrack) {
    				string track = CHAR(STRING_ELT(rvtracknames[i], itrack));
    				size_t pos = 0;

    				while ((pos = iexpr->find(track, pos)) != string::npos) {
    					if (is_var(*iexpr, pos, pos + track.size())) {
    						add_vtrack_var(track, VECTOR_ELT(vtracks[i], itrack), only_check, stime, etime);
    						break;
    					}
    					pos += track.size();
    				}
    			}
    		}
        }
	}
}

NRTrackExpressionVars::IteratorManager *NRTrackExpressionVars::add_imanager(const IteratorManager &imanager, NRTrack *track, NRTrack::Func func, unordered_set<double> &&vals)
{
    IteratorManagers::iterator iimanager;

    for (iimanager = m_imanagers.begin(); iimanager < m_imanagers.end(); ++iimanager) {
        if (*iimanager == imanager && iimanager->data_fetcher.func() == func && iimanager->data_fetcher.vals2compare() == vals)
            break;
    }

	if (iimanager == m_imanagers.end()) {
		if (m_imanagers.size() == m_imanagers.capacity())
			verror("Reached the limit of maximal number of simultaneously used virtual tracks");

		m_imanagers.push_back(imanager);
		m_imanagers.back().data_fetcher.init(track, imanager.filter != R_NilValue, move(vals));
        m_imanagers.back().data_fetcher.register_function(func);
		return &m_imanagers.back();
	}

	return &*iimanager;
}

void NRTrackExpressionVars::add_vtrack_var(const string &vtrack, SEXP rvtrack, bool only_check, unsigned stime, unsigned etime)
{
	for (TrackVars::const_iterator ivar = m_track_vars.begin(); ivar != m_track_vars.end(); ++ivar) {
		if (ivar->var_name == vtrack)
			return;
	}

	IteratorManager imanager;

	SEXP rsrc = get_rvector_col(rvtrack, "src", vtrack.c_str(), true);

	if (!isString(rsrc) || Rf_length(rsrc) != 1)
		verror("Invalid format of a virtual track %s", vtrack.c_str());

	imanager.name = CHAR(STRING_ELT(rsrc, 0));
    NRTrack *track = g_db->track(imanager.name.c_str());

    if (!track)
        verror("Invalid source %s used in a virtual track %s", imanager.name.c_str(), vtrack.c_str());

	SEXP rtshift = get_rvector_col(rvtrack, "time_shift", vtrack.c_str(), false);

	if (isNull(rtshift))
		imanager.sshift = imanager.eshift = 0;
	else {
		if (!(isReal(rtshift) || isInteger(rtshift)) || Rf_length(rtshift) < 1 || Rf_length(rtshift) > 2)
			verror("Virtual track %s: 'time.shift' must be an integer or a pair of integers", vtrack.c_str());

		if (Rf_length(rtshift) == 1)
			imanager.sshift = imanager.eshift = isReal(rtshift) ? (int)REAL(rtshift)[0] : INTEGER(rtshift)[0];
		else {
			imanager.sshift = isReal(rtshift) ? (int)REAL(rtshift)[0] : INTEGER(rtshift)[0];
			imanager.eshift = isReal(rtshift) ? (int)REAL(rtshift)[1] : INTEGER(rtshift)[1];
			if (imanager.sshift > imanager.eshift)
				swap(imanager.sshift, imanager.eshift);
		}

        if (imanager.sshift < -(int)NRTimeStamp::MAX_HOUR || imanager.sshift > (int)NRTimeStamp::MAX_HOUR ||
            imanager.eshift < -(int)NRTimeStamp::MAX_HOUR || imanager.eshift > (int)NRTimeStamp::MAX_HOUR)
            verror("Virtual track %s: 'time.shift' is out of range", vtrack.c_str());
	}

    SEXP rkeepref = get_rvector_col(rvtrack, "keepref", vtrack.c_str(), true);
    if (!isLogical(rkeepref) || Rf_length(rkeepref) != 1 || asLogical(rkeepref) == NA_LOGICAL)
        verror("Virtual track %s: keepref must be a logical value", vtrack.c_str());
    imanager.keepref = asLogical(rkeepref);

    if (imanager.keepref && (imanager.sshift || imanager.eshift))
        verror("Time shift is not allowed when keepref is 'TRUE'");

	m_track_vars.push_back(TrackVar());
	TrackVar &var = m_track_vars.back();
	var.var_name = vtrack;

	SEXP rfunc = get_rvector_col(rvtrack, "func", vtrack.c_str(), false);
	SEXP rparams = get_rvector_col(rvtrack, "params", vtrack.c_str(), false);
	string func;

	if (isNull(rfunc)) {
        if (track->is_categorial())
            func = NRTrack::FUNC_INFOS[NRTrack::VALUE].name;
        else
            func = NRTrack::FUNC_INFOS[NRTrack::AVG].name;
    } else {
		if (!isString(rfunc))
			verror("Function argument must be a string");

		func = CHAR(STRING_ELT(rfunc, 0));
		transform(func.begin(), func.end(), func.begin(), ::tolower);
	}

    unordered_set<double> vals;
	int ifunc;

	for (ifunc = 0; ifunc < NRTrack::NUM_FUNCS; ++ifunc) {
		if (!strcmp(func.c_str(), NRTrack::FUNC_INFOS[ifunc].name)) {
            if (imanager.keepref && !NRTrack::FUNC_INFOS[ifunc].keepref)
                verror("Function %s is not supported when keepref is 'TRUE'", NRTrack::FUNC_INFOS[ifunc].name);

            if (track->is_categorial() && !NRTrack::FUNC_INFOS[ifunc].categorial)
                verror("Function %s is not supported with categorial tracks", NRTrack::FUNC_INFOS[ifunc].name);

            if (track->is_quantitative() && !NRTrack::FUNC_INFOS[ifunc].quantitative)
                verror("Function %s is not supported with quantitative tracks", NRTrack::FUNC_INFOS[ifunc].name);

            if (ifunc == NRTrack::QUANTILE) {
				if (isNull(rparams))
					verror("Virtual track %s: function %s requires an additional parameter - percentile", vtrack.c_str(), func.c_str());
				if (!isReal(rparams) || Rf_length(rparams) != 1)
					verror("Virtual track %s: invalid parameters used for function %s", vtrack.c_str(), func.c_str());
				var.percentile = REAL(rparams)[0];
				if (var.percentile < 0 || var.percentile > 1)
					verror("Virtual track %s: parameter (percentile) used for function %s is out of range", vtrack.c_str(), func.c_str());
			} else {
                var.percentile = numeric_limits<double>::quiet_NaN();

                if (ifunc == NRTrack::EXISTS && isNull(rparams))
                    verror("Virtual track %s: function %s requires an additional parameter", vtrack.c_str(), func.c_str());

                if (track->is_categorial()) {
                    if (!isNull(rparams)) {
                        if (!isReal(rparams) && !isInteger(rparams))
                            verror("Virtual track %s: invalid parameters used for function %s", vtrack.c_str(), func.c_str());
                        for (int i = 0; i < Rf_length(rparams); ++i)
                            // The track might contain its data as float and not double. In this case a track value might not be equal to its double representation,
                            // like (float)0.3 != (double)0.3. So let's "downgrade" all our values to the least precise type.
                            vals.insert(isReal(rparams) ? (float)REAL(rparams)[i] : (float)INTEGER(rparams)[i]);
                    }
                } else if (!isNull(rparams)) {
                    if (NRTrack::FUNC_INFOS[ifunc].categorial)
                        verror("Virtual track %s: function %s does not accept any parameters when applied to quantative tracks", vtrack.c_str(), func.c_str());
                    verror("Virtual track %s: function %s does not accept any parameters", vtrack.c_str(), func.c_str());
                }
			}
			break;
		}
	}

	if (ifunc >= NRTrack::NUM_FUNCS)
		verror("Virtual track %s: invalid function %s used for a virtual track", vtrack.c_str(), func.c_str());

    SEXP rid_map = get_rvector_col(rvtrack, "id_map", vtrack.c_str(), false);

    imanager.id_map.clear();
    if (!isNull(rid_map)) {
        enum { ID1, ID2, TIME_SHIFT, NUM_COLS };
        static const char *COL_NAMES[NUM_COLS] = { "id1", "id2", "time.shift" };

        if (TYPEOF(rid_map) == PROMSXP) {
            if (PRENV(rid_map) == R_NilValue)
                rid_map = PRVALUE(rid_map);
            else
                rid_map = eval_in_R(PRCODE(rid_map), PRENV(rid_map));
        }

        if (!isVector(rid_map) || xlength(rid_map) < NUM_COLS - 1)
            verror("Virtual track %s: invalid format of 'id.map'", vtrack.c_str());

        SEXP colnames = getAttrib(rid_map, R_NamesSymbol);

        if (!isString(colnames) || xlength(colnames) < NUM_COLS - 1)
            verror("Virtual track %s: invalid format of 'id.map'", vtrack.c_str());

        for (unsigned i = 0; i < NUM_COLS - 1; i++) {
            if (strcmp(CHAR(STRING_ELT(colnames, i)), COL_NAMES[i]))
                verror("Virtual track %s: invalid format of 'id.map'", vtrack.c_str());
        }

        bool time_shift_used = xlength(rid_map) >= NUM_COLS && xlength(colnames) >= NUM_COLS &&
            !strcmp(CHAR(STRING_ELT(colnames, TIME_SHIFT)), COL_NAMES[TIME_SHIFT]);

        if (time_shift_used && imanager.keepref)
            verror("Time shift in 'id.map' is not supported when 'keepref' is 'TRUE'");

        SEXP rids1 = VECTOR_ELT(rid_map, ID1);
        SEXP rids2 = VECTOR_ELT(rid_map, ID2);
        SEXP rtime_shift = time_shift_used ? VECTOR_ELT(rid_map, TIME_SHIFT) : R_NilValue;
        unsigned num_ids = (unsigned)xlength(rids1);

        if (!isReal(rids1) && !isInteger(rids1) || !isReal(rids2) && !isInteger(rids2) || xlength(rids1) != xlength(rids2) ||
            time_shift_used && xlength(rids1) != xlength(rtime_shift))
            verror("Virtual track %s: invalid format of 'id.map'", vtrack.c_str());

        for (unsigned i = 0; i < num_ids; ++i) {
            double id1 = isReal(rids1) ? REAL(rids1)[i] : INTEGER(rids1)[i];
            double id2 = isReal(rids2) ? REAL(rids2)[i] : INTEGER(rids2)[i];
            int time_shift = 0;

            if (time_shift_used)
                time_shift = isReal(rtime_shift) ? REAL(rtime_shift)[i] : INTEGER(rtime_shift)[i];

            if (id1 < g_db->minid() || id1 > g_db->maxid() || id1 != (int)id1)
                verror("Virtual track %s: invalid id (%g) within 'id.map'", vtrack.c_str(), id1);
            if (id2 < g_db->minid() || id2 > g_db->maxid() || id2 != (int)id2)
                verror("Virtual track %s: invalid id (%g) within 'id.map'", vtrack.c_str(), id2);

            IdMap::const_iterator iid_map = imanager.id_map.find((unsigned)id1);
            if (iid_map != imanager.id_map.end())
                verror("Virtual track %s: id (%d) is mapped more than once within 'id.map'", vtrack.c_str(), (unsigned)id1);

            imanager.id_map[(unsigned)id1] = {(unsigned)id2, time_shift};
        }
    }

    SEXP rfilter = get_rvector_col(rvtrack, "filter", vtrack.c_str(), false);

    if (!isNull(rfilter)) {
        if (only_check) {
            NRIteratorFilter filter;
            filter.init(rfilter, g_db->mintime(), g_db->maxtime());
        } else {
            // Create an intermediate track by applying the filter to the original track
            track = g_db->track(imanager.name.c_str());

            NRTrackData<float> track_data_float;
            NRTrackData<double> track_data_double;
            vector<string> track_expr;

            track_expr.push_back(imanager.name);

            NRTrackExprScanner scanner;

            scanner.report_progress(false);

            for (scanner.begin(track_expr, NRTrackExprScanner::REAL_T,
                               max((int)stime + imanager.sshift, (int)g_db->mintime()),
                               min((int)etime + imanager.eshift, (int)g_db->maxtime()),
                               R_NilValue, true, rfilter); !scanner.isend(); scanner.next())
            {
                if (track->data_type() == NRTrack::FLOAT)
                    track_data_float.add_data(scanner.point().id, scanner.point().timestamp, scanner.real());
                else
                    track_data_double.add_data(scanner.point().id, scanner.point().timestamp, scanner.real());
            }

            if (track->data_type() == NRTrack::FLOAT)
                track = NRTrack::construct((vtrack + ".filtered").c_str(), track, track->flags(), track_data_float);
            else
                track = NRTrack::construct((vtrack + ".filtered").c_str(), track, track->flags(), track_data_double);

            imanager.filter = rfilter;
        }
    }

    var.imanager = add_imanager(imanager, track, (NRTrack::Func)ifunc, move(vals));
}

NRTrackExpressionVars::TrackVar &NRTrackExpressionVars::add_track_var(const string &track_name)
{
	for (TrackVars::iterator ivar = m_track_vars.begin(); ivar != m_track_vars.end(); ++ivar) {
		if (ivar->var_name == track_name)
			return *ivar;
	}

	IteratorManager imanager;

	imanager.name = track_name;
    imanager.keepref = true;
	m_track_vars.push_back(TrackVar());

    NRTrack *track = g_db->track(track_name);
	TrackVar &var = m_track_vars.back();
	var.var_name = track_name;
	var.percentile = numeric_limits<double>::quiet_NaN();
	var.imanager = add_imanager(imanager, track, track->is_categorial() ? NRTrack::VALUE : NRTrack::AVG, unordered_set<double>());
	return var;
}

void NRTrackExpressionVars::define_r_vars(unsigned size)
{
	for (TrackVars::iterator ivar = m_track_vars.begin(); ivar != m_track_vars.end(); ivar++) {
		rprotect(ivar->rvar = allocVector(REALSXP, size));
		defineVar(install(ivar->var_name.c_str()), ivar->rvar, g_naryn->env());
		ivar->var = REAL(ivar->rvar);
        for (int i = 0; i < size; ++i)
            ivar->var[i] = numeric_limits<double>::quiet_NaN();
	}
}

