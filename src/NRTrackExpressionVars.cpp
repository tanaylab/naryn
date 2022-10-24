#include <algorithm>
#include <unistd.h>

#include "EMRDb.h"
#include "naryn.h"
#include "NRIteratorFilter.h"
#include "NRPoint.h"
#include "NRTrackExpressionScanner.h"
#include "NRTrackExpressionVars.h"

NRTrackExpressionVars::NRTrackExpressionVars()
{
    m_imanagers.reserve(10000);
}

void NRTrackExpressionVars::parse_expr(const string &expr, unsigned stime, unsigned etime){
    vector<string> vars;
    get_expression_vars(expr, vars);

    for (string var : vars) {
        // look for track names
        if (g_db->track_exists(var)) {
            add_track_var(var);
        }

        // look for logical tracks and add a virtual track if needed
        if (g_db->logical_track_exists(var)) {
            const EMRLogicalTrack *logical_track =
                g_db->logical_track(var.c_str());
            add_vtrack_var(
                var,
                logical_track->vtrack(),
                false, stime, etime);
        }

        // look for virtual tracks using emr_vtrack.exists R function
        SEXP e;
        PROTECT(e = lang2(install("emr_vtrack.exists"),
                          mkString(var.c_str())));
        bool vtrack_exists = asLogical(R_tryEval(e, g_naryn->env(), NULL));
        UNPROTECT(1);

        if (vtrack_exists) {            
            // get the virtual track from R and add it
            PROTECT(e = lang3(install(".emr_vtrack.get"),
                                mkString(var.c_str()),
                                ScalarLogical(0)));
            SEXP vtrack = R_tryEval(e, g_naryn->env(), NULL);
            UNPROTECT(1);
            add_vtrack_var(var, vtrack, false, stime, etime);
        }
    }
}

void NRTrackExpressionVars::parse_exprs(const vector<string> &track_exprs, unsigned stime, unsigned etime) {
    for (vector<string>::const_iterator iexpr = track_exprs.begin(); iexpr != track_exprs.end(); ++iexpr) {
        parse_expr(*iexpr, stime, etime);     
    }
}

void NRTrackExpressionVars::check_vtrack(const string &track, SEXP rvtrack) {
    NRTrackExpressionVars parser;
    parser.add_vtrack_var(track, rvtrack, true, 0, 0);
}

NRTrackExpressionVars::IteratorManager *NRTrackExpressionVars::add_imanager(const IteratorManager &imanager, EMRTrack *track, EMRTrack::Func func, unordered_set<double> &&vals, bool track_ownership){
    IteratorManagers::iterator iimanager;

    for (iimanager = m_imanagers.begin(); iimanager < m_imanagers.end(); ++iimanager) {
        if (*iimanager == imanager && iimanager->data_fetcher.func() == func && iimanager->data_fetcher.vals2compare() == vals)
            break;
    }

    if (iimanager == m_imanagers.end()) {
        if (m_imanagers.size() == m_imanagers.capacity())
            verror("Reached the limit of maximal number of simultaneously used virtual tracks");

        m_imanagers.push_back(imanager);
        m_imanagers.back().data_fetcher.init(track, track_ownership, std::move(vals));
        m_imanagers.back().data_fetcher.register_function(func);
        return &m_imanagers.back();
    }

    return &*iimanager;
}

void NRTrackExpressionVars::add_vtrack_var(const string &vtrack, SEXP rvtrack, bool only_check, unsigned stime, unsigned etime){
    for (TrackVars::const_iterator ivar = m_track_vars.begin(); ivar != m_track_vars.end(); ++ivar) {
        if (ivar->var_name == vtrack)
            return;
    }

    IteratorManager imanager;
    bool track_ownership = false;
    EMRTrack *track = NULL;
    bool is_categorical = false;
    EMRTrackData<float> data;

    SEXP rsrc = get_rvector_col(rvtrack, "src", vtrack.c_str(), true);

    if (isString(rsrc)) {
        if (Rf_length(rsrc) != 1)
            verror("Invalid format of a virtual track %s", vtrack.c_str());

        imanager.name = CHAR(STRING_ELT(rsrc, 0));
        track = g_db->track(imanager.name.c_str());

        if (!track)
            verror("Invalid source %s used in a virtual track %s", imanager.name.c_str(), vtrack.c_str());

        is_categorical = track->is_categorical();
    } else {    // 'src' must be then list(data.frame, T/F)
        char buf[1000];

        if (!isVector(rsrc) || Rf_length(rsrc) != 2 || !isLogical(VECTOR_ELT(rsrc, 1)))
            verror("Invalid source used in a virtual track %s", vtrack.c_str());

        is_categorical = asLogical(VECTOR_ELT(rsrc, 1));
        NRPoint::convert_rpoints_vals(VECTOR_ELT(rsrc, 0), data, "Virtual's track 'src' attribute: ");

        snprintf(buf, sizeof(buf), ".emr_%s.src.df.%lu", vtrack.c_str(), m_imanagers.size());
        imanager.name = buf;
    }

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

        if (imanager.sshift < -(int)EMRTimeStamp::MAX_HOUR || imanager.sshift > (int)EMRTimeStamp::MAX_HOUR ||
            imanager.eshift < -(int)EMRTimeStamp::MAX_HOUR || imanager.eshift > (int)EMRTimeStamp::MAX_HOUR)
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
        if (is_categorical)
            func = EMRTrack::FUNC_INFOS[EMRTrack::VALUE].name;
        else
            func = EMRTrack::FUNC_INFOS[EMRTrack::AVG].name;
    } else {
        if (!isString(rfunc))
            verror("Function argument must be a string");

        func = CHAR(STRING_ELT(rfunc, 0));
        transform(func.begin(), func.end(), func.begin(), ::tolower);
    }

    unordered_set<double> vals;
    int ifunc;

    for (ifunc = 0; ifunc < EMRTrack::NUM_FUNCS; ++ifunc) {
        if (!strcmp(func.c_str(), EMRTrack::FUNC_INFOS[ifunc].name)) {
            if (imanager.keepref && !EMRTrack::FUNC_INFOS[ifunc].keepref)
                verror("Function %s is not supported when keepref is 'TRUE'", EMRTrack::FUNC_INFOS[ifunc].name);

            if (is_categorical && !EMRTrack::FUNC_INFOS[ifunc].categorical)
                verror("Function %s is not supported with categorical data", EMRTrack::FUNC_INFOS[ifunc].name);

            if (!is_categorical && !EMRTrack::FUNC_INFOS[ifunc].quantitative)
                verror("Function %s is not supported with quantitative data", EMRTrack::FUNC_INFOS[ifunc].name);

            if (ifunc == EMRTrack::QUANTILE) {
                if (isNull(rparams))
                    verror("Virtual track %s: function %s requires an additional parameter - percentile", vtrack.c_str(), func.c_str());
                if (!isReal(rparams) || Rf_length(rparams) != 1)
                    verror("Virtual track %s: invalid parameters used for function %s", vtrack.c_str(), func.c_str());
                var.percentile = REAL(rparams)[0];
                if (var.percentile < 0 || var.percentile > 1)
                    verror("Virtual track %s: parameter (percentile) used for function %s is out of range", vtrack.c_str(), func.c_str());
            } else {
                var.percentile = numeric_limits<double>::quiet_NaN();

                if (ifunc == EMRTrack::EXISTS && isNull(rparams))
                    verror("Virtual track %s: function %s requires an additional parameter", vtrack.c_str(), func.c_str());

                if (is_categorical) {
                    if (!isNull(rparams)) {       
                        // params are a single NA value   
                        if (isLogical(rparams) && 
                            Rf_length(rparams) == 1 && 
                            LOGICAL(rparams)[0] == NA_LOGICAL){
                            vals.insert(NAN);                            
                        } else if (!isReal(rparams) && !isInteger(rparams)) {
                            verror("Virtual track %s: invalid parameters used for function %s", vtrack.c_str(), func.c_str());
                        } else {
                            for (int i = 0; i < Rf_length(rparams); ++i){ 
                                // The track might contain its data as float and not double. In this case a track value might not be equal to its double representation,
                                // like (float)0.3 != (double)0.3. So let's "downgrade" all our values to the least precise type.
                                vals.insert(isReal(rparams)
                                                ? (float)REAL(rparams)[i]
                                                : (float)INTEGER(rparams)[i]);
                                
                            }
                        }
                    }
                } else if (!isNull(rparams)) {
                    if (EMRTrack::FUNC_INFOS[ifunc].categorical)
                        verror("Virtual track %s: function %s does not accept any parameters when applied to quantative data", vtrack.c_str(), func.c_str());
                    verror("Virtual track %s: function %s does not accept any parameters", vtrack.c_str(), func.c_str());
                }
            }
            break;
        }
    }

    if (ifunc >= EMRTrack::NUM_FUNCS)
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

        if ((!isReal(rids1) && !isInteger(rids1)) || 
            (!isReal(rids2) && !isInteger(rids2)) || 
            (xlength(rids1) != xlength(rids2)) ||
            (time_shift_used && xlength(rids1) != xlength(rtime_shift))){
            verror("Virtual track %s: invalid format of 'id.map'", vtrack.c_str());
        }

        for (unsigned i = 0; i < num_ids; ++i) {
            double id1 = isReal(rids1) ? REAL(rids1)[i] : INTEGER(rids1)[i];
            double id2 = isReal(rids2) ? REAL(rids2)[i] : INTEGER(rids2)[i];
            int time_shift = 0;

            if (time_shift_used)
                time_shift = isReal(rtime_shift) ? REAL(rtime_shift)[i] : INTEGER(rtime_shift)[i];

            if (!g_db->id_exists((unsigned)id1) || id1 != (int)id1)
                verror("Virtual track %s: invalid source id (%g) within 'id.map'", vtrack.c_str(), id1);
            if (!g_db->id_exists((unsigned)id2) || id2 != (int)id2)
                verror("Virtual track %s: invalid target id (%g) within 'id.map'", vtrack.c_str(), id2);

            IdMap::const_iterator iid_map = imanager.id_map.find((unsigned)id1);
            if (iid_map != imanager.id_map.end())
                verror("Virtual track %s: id (%d) is mapped more than once within 'id.map'", vtrack.c_str(), (unsigned)id1);

            imanager.id_map[(unsigned)id1] = {(unsigned)id2, time_shift};
        }
    }

    SEXP rfilter = get_rvector_col(rvtrack, "filter", vtrack.c_str(), false);

    try {
        // time to build the intermediate track if src==data.frame
        if (!only_check && !isString(rsrc)) {
            track = EMRTrack::construct(imanager.name.c_str(), (EMRTrack::Func)ifunc, is_categorical ? EMRTrack::IS_CATEGORICAL : 0, data);
            track_ownership = true;
        }

        if (!isNull(rfilter)) {
            if (!isString(rsrc))
                verror("Virtual track %s: filter cannot be used when 'src' is a data frame", vtrack.c_str());

            if (only_check) {
                NRIteratorFilter filter;
                filter.init(rfilter, 0, EMRTimeStamp::MAX_HOUR);
            } else {
                // Create an intermediate track by applying the filter to the original track
                // (not to be confused with an intermediate track that needs to be built when src==data.frame)
                track = g_db->track(imanager.name.c_str());

                EMRTrackData<float> track_data_float;
                EMRTrackData<double> track_data_double;
                vector<string> track_expr;

                track_expr.push_back(imanager.name);

                NRTrackExprScanner scanner;

                scanner.report_progress(false);

                for (scanner.begin(track_expr, NRTrackExprScanner::REAL_T,
                                   max((int)stime + imanager.sshift, 0),
                                   min((int)etime + imanager.eshift, (int)EMRTimeStamp::MAX_HOUR),
                                   false, R_NilValue, true, rfilter); !scanner.isend(); scanner.next())
                {
                    if (track->data_type() == EMRTrack::FLOAT)
                        track_data_float.add(scanner.point().id, scanner.point().timestamp, scanner.real());
                    else
                        track_data_double.add(scanner.point().id, scanner.point().timestamp, scanner.real());
                }

                if (track->data_type() == EMRTrack::FLOAT)
                    track = EMRTrack::construct((vtrack + ".filtered").c_str(), track, track->flags(), track_data_float);
                else
                    track = EMRTrack::construct((vtrack + ".filtered").c_str(), track, track->flags(), track_data_double);

                track_ownership = true;
                imanager.filter = rfilter;
            }
        }
    } catch (...) {
        if (track_ownership && track)
            delete track;
        throw;
    }

    SEXP rlogical = get_rvector_col(rvtrack, "logical", vtrack.c_str(), false);
    if (!isNull(rlogical)) {
        SEXP rlsource = get_rvector_col(rlogical, "src", vtrack.c_str(), false);
        var.logical_track_source = CHAR(STRING_ELT(rlsource, 0));        
    }

    if (!only_check)
        var.imanager = add_imanager(imanager, track, (EMRTrack::Func)ifunc, std::move(vals), track_ownership);
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

    EMRTrack *track = g_db->track(track_name);
    TrackVar &var = m_track_vars.back();
    var.var_name = track_name;
    var.percentile = numeric_limits<double>::quiet_NaN();
    var.imanager = add_imanager(imanager, track, track->is_categorical() ? EMRTrack::VALUE : EMRTrack::AVG, unordered_set<double>(), false);
    return var;
}

void NRTrackExpressionVars::define_r_vars(unsigned size)
{
    for (TrackVars::iterator ivar = m_track_vars.begin(); ivar != m_track_vars.end(); ivar++) {
        rprotect(ivar->rvar = RSaneAllocVector(REALSXP, size));
        defineVar(install(ivar->var_name.c_str()), ivar->rvar, g_naryn->env());
        ivar->var = REAL(ivar->rvar);
        for (int i = 0; i < (int)size; ++i)
            ivar->var[i] = numeric_limits<double>::quiet_NaN();
    }
}

