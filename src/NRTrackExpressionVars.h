#ifndef NRTRACKEXPRESSIONVARS_H_INCLUDED
#define NRTRACKEXPRESSIONVARS_H_INCLUDED

#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <string>
#include <string.h>

#include <R.h>
#include <Rinternals.h>

#ifdef length
#undef length
#endif

#include "EMRInterval.h"
#include "EMRPoint.h"
#include "EMRTrack.h"
#include "naryn.h"

using namespace std;

class NRTrackExpressionVars {
public:
    typedef unordered_map<unsigned, pair<unsigned, int>> IdMap;

    struct IteratorManager {
        string                 name;
        EMRTrack::DataFetcher  data_fetcher;
        int                    sshift;       // time shift in hours
        int                    eshift;
        bool                   keepref;
        EMRInterval            interv;
        IdMap                  id_map;
        SEXP                   filter;

        IteratorManager() : sshift(0), eshift(0), keepref(false), filter(R_NilValue) {}
        bool operator==(const IteratorManager &o) const;

        void transform(const EMRPoint &point, EMRTimeStamp::Refcount refcount);
    };

    struct TrackVar {
        string             var_name;
        SEXP               rvar{R_NilValue};
        double            *var;
        double             percentile;
        IteratorManager   *imanager;    
        string             logical_track_source; // name of a logical track the vtrack was created from (empty by default)
    };

    NRTrackExpressionVars();
    ~NRTrackExpressionVars();

	unsigned get_num_track_vars() const { return m_track_vars.size(); }

	const string &get_track_name(unsigned ivar) const { return m_track_vars[ivar].imanager->name; }
    const string &get_var_name(unsigned ivar) const { return m_track_vars[ivar].var_name; }
    const string &get_logical_track_source(unsigned ivar) const { return m_track_vars[ivar].logical_track_source; }
    EMRTrack *get_track(unsigned ivar) const { return m_track_vars[ivar].imanager->data_fetcher.track(); }

	void parse_exprs(const vector<string> &track_exprs, unsigned stime, unsigned etime);
	void define_r_vars(unsigned size);
    const TrackVar *var(const char *var_name) const;

	void set_vars(const EMRPoint &point, unsigned idx);

    static void check_vtrack(const string &track, SEXP rvtrack);

    static bool is_var(const string &str, size_t start, size_t end) {
        return (!start || !is_R_var_char(str[start - 1])) &&
               (end == str.size() || !is_R_var_char(str[end]));
    }

private:
	typedef vector<IteratorManager> IteratorManagers;

	typedef vector<TrackVar> TrackVars;

	TrackVars              m_track_vars;
	IteratorManagers       m_imanagers;
	int                    m_abs_hour;

	IteratorManager     *add_imanager(const IteratorManager &imanager, EMRTrack *track, EMRTrack::Func func, unordered_set<double> &&vals, bool track_ownership);
	TrackVar            &add_track_var(const string &track);
	void                 add_vtrack_var(const string &track, SEXP rvtrack, bool only_check, unsigned stime, unsigned etime);	
};


// -------------------------------------------------- IMPLEMENTATION -------------------------------------------------------

inline NRTrackExpressionVars::~NRTrackExpressionVars()
{
    for (TrackVars::iterator ivar = m_track_vars.begin(); ivar != m_track_vars.end(); ivar++)
        runprotect(ivar->rvar);
}

inline bool NRTrackExpressionVars::IteratorManager::operator==(const IteratorManager &o) const
{
    return id_map.empty() && o.id_map.empty() && filter == R_NilValue && o.filter == R_NilValue && name == o.name && sshift == o.sshift && eshift == o.eshift && keepref == o.keepref;
}

inline void NRTrackExpressionVars::IteratorManager::transform(const EMRPoint &point, EMRTimeStamp::Refcount refcount)
{
    if (id_map.empty())
        interv.init(point.id, max((int)point.timestamp.hour() + sshift, 0), point.timestamp.hour() + eshift, refcount);
    else {
        IdMap::const_iterator iid = id_map.find(point.id);
        if (iid == id_map.end())
            interv.init(-1, max((int)point.timestamp.hour() + sshift, 0), point.timestamp.hour() + eshift, refcount);
        else
            interv.init(iid->second.first, max((int)point.timestamp.hour() + sshift + iid->second.second, 0), point.timestamp.hour() + eshift + iid->second.second, refcount);
    }
}


inline void NRTrackExpressionVars::set_vars(const EMRPoint &point, unsigned idx)
{
	for (IteratorManagers::iterator iimanager = m_imanagers.begin(); iimanager != m_imanagers.end(); ++iimanager) {
        iimanager->transform(point, iimanager->keepref ? point.timestamp.refcount() : EMRTimeStamp::NA_REFCOUNT);
        if (iimanager->interv.stime <= iimanager->interv.etime)
            iimanager->data_fetcher.set_vals(iimanager->interv);
    }

    for (TrackVars::iterator ivar = m_track_vars.begin(); ivar != m_track_vars.end(); ++ivar) {
        EMRTrack::DataFetcher &data_fetcher = ivar->imanager->data_fetcher;

        if (ivar->imanager->interv.stime > ivar->imanager->interv.etime)
            ivar->var[idx] = numeric_limits<double>::quiet_NaN();
        else if (data_fetcher.func() == EMRTrack::QUANTILE)
            ivar->var[idx] = data_fetcher.quantile(ivar->percentile);
        else
            ivar->var[idx] = data_fetcher.val();
    }
}

inline const NRTrackExpressionVars::TrackVar *NRTrackExpressionVars::var(const char *var_name) const
{
    for (TrackVars::const_iterator ivar = m_track_vars.begin(); ivar != m_track_vars.end(); ivar++) {
        if (ivar->var_name == var_name)
            return &*ivar;
    }
    return NULL;
}

#endif

