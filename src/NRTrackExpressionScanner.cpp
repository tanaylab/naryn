#include <errno.h>
#include <sys/timeb.h>

#include <set>
#include <unordered_set>

#include "port.h"

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Parse.h>

#ifdef length
#undef length
#endif

#include "HashFunc.h"
#include "NRBeatIterator.h"
#include "NRBeatExtIterator.h"
#include "NRIdTimeIntervalsIterator.h"
#include "NRIdsIterator.h"
#include "NRPointsIterator.h"
#include "NRTimesIterator.h"
#include "NRTrack.h"
#include "NRTrackExpressionScanner.h"
#include "NRTrackExpressionVars.h"
#include "NRTrackIterator.h"

const int NRTrackExprScanner::INIT_REPORT_STEP = 10000;
const int NRTrackExprScanner::REPORT_INTERVAL = 3000;
const int NRTrackExprScanner::MIN_REPORT_INTERVAL = 1000;
const int NRTrackExprScanner::MIN_MULTITASKING_TIME = 2000;
const int NRTrackExprScanner::MAX_MULTITASKING_TIME = 20000;
const int NRTrackExprScanner::CHECK_MULTITASKING_TIME = 300;

static uint64_t get_cur_clock()
{
	struct timeb tp;
	ftime(&tp);
	return tp.time * 1000 + tp.millitm;
}

NRTrackExprScanner::NRTrackExprScanner() :
    m_mtask_buf(NULL),
	m_num_track_vars(0),
	m_isend(true),
	m_expr_vars()
{
	m_do_report_progress = true;
    m_ritr_times = R_NilValue;
    m_multitasking = false;
}

NRTrackExprScanner::~NRTrackExprScanner()
{
    delete []m_mtask_buf;
}

void NRTrackExprScanner::convert_rtrack_exprs(SEXP rtrack_exprs, vector<string> &track_exprs)
{
	track_exprs.clear();

	if (!isString(rtrack_exprs) || Rf_length(rtrack_exprs) < 1)
		verror("Tracks expressions argument must be a vector of strings");

	unsigned num_track_exprs = (unsigned)Rf_length(rtrack_exprs);
	track_exprs.resize(num_track_exprs);

	for (unsigned iexpr = 0; iexpr < num_track_exprs; ++iexpr)
		track_exprs[iexpr] = CHAR(STRING_ELT(rtrack_exprs, iexpr));
}

void NRTrackExprScanner::convert_rscope(SEXP rstime, SEXP retime, unsigned *pstime, unsigned *petime)
{
	if (!isReal(rstime) && !isInteger(rstime) || !isReal(retime) && !isInteger(retime) || Rf_length(rstime) != 1 || Rf_length(retime) != 1)
		verror("Invalid time scope");

	double stime = asReal(rstime);
	double etime = asReal(retime);

	if (stime != (int)stime || stime < 0)
		verror("Time scope start time must be a positive integer");

	if (etime != (int)etime || etime < 0)
		verror("Time scope end time must be a positive integer");

	if (stime > etime)
		verror("Time scope start time exceeds end time");

	*pstime = (unsigned)stime;
	*petime = (unsigned)etime;
}

bool NRTrackExprScanner::convert_rkeepref(SEXP rkeepref)
{
    if (!isLogical(rkeepref) || Rf_length(rkeepref) != 1 || asLogical(rkeepref) == NA_LOGICAL)
        verror("Invalid format of iter.keepref parameter");

    return asLogical(rkeepref);
}

void NRTrackExprScanner::define_r_vars(unsigned eval_buf_limit)
{
	m_eval_buf_limit = eval_buf_limit;
	m_expr_vars.define_r_vars(m_eval_buf_limit);
	m_expr_itr_points.resize(m_eval_buf_limit);

    runprotect(m_ritr_times);
    rprotect(m_ritr_times = allocVector(REALSXP, m_eval_buf_limit));
    m_itr_times = REAL(m_ritr_times);
    for (unsigned i = 0; i < eval_buf_limit; ++i)
        m_itr_times[i] = 0;
    defineVar(install("EMR_TIME"), m_ritr_times, findVar(install(".GlobalEnv"), g_naryn->env()));

    for (unsigned iexpr = 0; iexpr < m_track_exprs.size(); ++iexpr) {
        const NRTrackExpressionVars::TrackVar *var = m_expr_vars.var(m_track_exprs[iexpr].c_str());

        if (var) { // track expression is a virtual track
            if (m_valtype == LOGICAL_T)
                verror("Expression \"%s\" does not produce a logical result", m_track_exprs[iexpr].c_str());

            m_eval_doubles[iexpr] = REAL(var->rvar);
        }
    }
}

void NRTrackExprScanner::check(SEXP track_exprs, SEXP rstime, SEXP retime, SEXP iterator_policy, SEXP rkeepref, SEXP filter)
{
	vector<string> track_expr_strs;
	unsigned stime, etime;
	convert_rtrack_exprs(track_exprs, track_expr_strs);
	convert_rscope(rstime, retime, &stime, &etime);
	check(track_expr_strs, stime, etime, iterator_policy, convert_rkeepref(rkeepref), filter);
}

void NRTrackExprScanner::check(const vector<string> &track_exprs, unsigned stime, unsigned etime, SEXP iterator_policy, bool keepref, SEXP filter)
{
    runprotect(m_eval_bufs);
    runprotect(m_eval_exprs);

    m_track_exprs.reserve(track_exprs.size());
    for (vector<string>::const_iterator iexpr = track_exprs.begin(); iexpr != track_exprs.end(); ++iexpr) {
        // trim spaces from the track expression
        string::const_iterator istr_start, istr_end;

        for (istr_start = iexpr->begin(); istr_start < iexpr->end(); ++istr_start) {
            if (!isspace(*istr_start))
                break;
        }

        for (istr_end = iexpr->end() - 1; istr_end >= iexpr->begin(); --istr_end) {
            if (!isspace(*istr_end))
                break;
        }

        m_track_exprs.push_back(iexpr->substr(istr_start - iexpr->begin(), istr_end - istr_start + 1));
    }

	m_eval_exprs.resize(m_track_exprs.size(), R_NilValue);
	m_eval_bufs.resize(m_track_exprs.size(), R_NilValue);
	m_eval_doubles.resize(m_track_exprs.size(), NULL);
	m_eval_ints.resize(m_track_exprs.size(), NULL);

	m_expr_vars.parse_exprs(m_track_exprs, false, stime, etime);

	// initiate the expression iterator
	create_expr_iterator(&m_itr, iterator_policy, keepref, m_expr_vars, m_track_exprs, stime, etime, filter);

	for (unsigned iexpr = 0; iexpr < m_track_exprs.size(); ++iexpr) {
        if (!m_expr_vars.var(m_track_exprs[iexpr].c_str())) {   // track expression is not a virtual track
    		SEXP expr;
    		rprotect(expr = allocVector(STRSXP, 1));
    		SET_STRING_ELT(expr, 0, mkChar(m_track_exprs[iexpr].c_str()));

    		// parse R expression
    		ParseStatus status;
    		SEXP parsed_expr;
    		rprotect(parsed_expr = R_ParseVector(expr, -1, &status, R_NilValue));
    		if (status != PARSE_OK)
    			verror("R parsing of expression \"%s\" failed", m_track_exprs[iexpr].c_str());
    		m_eval_exprs[iexpr] = VECTOR_ELT(parsed_expr, 0);
    		runprotect(expr);
        }
	}
}

bool NRTrackExprScanner::begin(SEXP track_exprs, ValType valtype, SEXP rstime, SEXP retime, SEXP iterator_policy, SEXP rkeepref, SEXP filter)
{
	vector<string> track_expr_strs;
	unsigned stime, etime;

	convert_rtrack_exprs(track_exprs, track_expr_strs);
	convert_rscope(rstime, retime, &stime, &etime);
	return begin(track_expr_strs, valtype, stime, etime, iterator_policy, convert_rkeepref(rkeepref), filter);
}

bool NRTrackExprScanner::begin(const vector<string> &track_exprs, ValType valtype, unsigned stime, unsigned etime, SEXP iterator_policy, bool keepref, SEXP filter)
{
    vdebug("Parsing track expressions\n");
	check(track_exprs, stime, etime, iterator_policy, keepref, filter);

    m_valtype = valtype;
    m_multitasking = false;

	// check whether m_eval_buf_limit == 1000 will correctly work for the expression
    vdebug("Defining R variables\n");
	SEXP eval_buf_size = GetOption(install("emr_eval.buf.size"), R_NilValue);
	if (!isReal(eval_buf_size) || REAL(eval_buf_size)[0] < 1)
		define_r_vars(1000);
	else
		define_r_vars((unsigned)REAL(eval_buf_size)[0]);

    vdebug("Determining evaluation buffer size\n");
	for (unsigned iexpr = 0; iexpr < m_track_exprs.size(); ++iexpr) {
        if (m_eval_exprs[iexpr] != R_NilValue) {
            SEXP res = eval_in_R(m_eval_exprs[iexpr], g_naryn->env());

            if (Rf_length(res) != (int)m_eval_buf_limit) {
                runprotect(res);
                define_r_vars(1);
                break;
            }
            runprotect(res);
        }
	}

    if (isNull(filter) && (typeid(m_itr.itr()) == typeid(NRBeatIterator) || typeid(m_itr.itr()) == typeid(NRBeatExtIterator)) &&
        g_naryn->beat_itr_warning_size() != (uint64_t)-1 && m_itr.itr().size() > g_naryn->beat_itr_warning_size())
    {
        if (typeid(m_itr.itr()) == typeid(NRBeatIterator))
            vwarning("The Beat Iterator is going to produce %llu points.\n"
                     "To improve performance please consider using a filter.\n", m_itr.itr().size());
        else
            vwarning("The Extended Beat Iterator might produce up to %llu points.\n"
                     "To improve performance please consider using a filter.\n", m_itr.itr().size());
    }

	m_num_evals = 0;
	m_last_progress_reported = -1;
	m_report_step = INIT_REPORT_STEP;
	m_last_report_clock = get_cur_clock();
    if (g_naryn->multitasking_avail()) 
        g_naryn->set_alarm(CHECK_MULTITASKING_TIME);

	m_isend = false;
	m_eval_buf_idx = m_eval_buf_limit;
	m_eval_buf_size = 0;

	return next();
}

void NRTrackExprScanner::report_progress()
{
    m_num_evals += m_eval_buf_size;
    if (m_num_evals > (size_t)m_report_step && m_do_report_progress) {
        uint64_t curclock = get_cur_clock();
        double delta = curclock - m_last_report_clock;

        if (delta)
            m_report_step = (int)(m_report_step * (REPORT_INTERVAL / delta) + .5);
        else
            m_report_step *= 10;

        if (delta > MIN_REPORT_INTERVAL) {
            if (m_last_progress_reported < 0 && m_eval_buf_limit == 1 && !m_multitasking)
                Rprintf("Warning: track expression(s) cannot be evaluated as a vector. Run-times might be slow.\n");

            int progress = 0;

            if (m_itr.itr().size()) {
                if (m_multitasking)
                    progress = Naryn::itr_idx_sum() * 100. / (m_itr.itr().size() * Naryn::num_kids());
                else
                    progress = (int)(m_itr.itr().idx() * 100. / m_itr.itr().size());
            }

            progress = max(progress, m_last_progress_reported);  // just to be on the safe side
            if (progress != 100) {
                if (progress != m_last_progress_reported)
                    Rprintf("%d%%...", progress);
                else
                    Rprintf(".");
                m_last_progress_reported = progress;
            }
            m_num_evals = 0;
            m_last_report_clock = curclock;
        }
    }
}

void NRTrackExprScanner::start_multitasking()
{
    vdebug("Checking possible switching to multitasking\n");
    double progress = m_itr.itr().size() ? m_itr.itr().idx() / (double)m_itr.itr().size() : 0.;
    size_t estimated_runtime = progress ? CHECK_MULTITASKING_TIME + CHECK_MULTITASKING_TIME / progress : MIN_MULTITASKING_TIME;

    g_naryn->reset_alarm();

    if (m_itr.isend())
        return;

    // switch to multitasking
    vdebug("Estimated run-time without multitasking: %g sec\n", estimated_runtime / 1000.);
    if (estimated_runtime < (size_t)MIN_MULTITASKING_TIME)
        return;

    int num_cores = max(1, (int)sysconf(_SC_NPROCESSORS_ONLN));
    vdebug("Detected %d cores\n", num_cores);

    unsigned cur_id = m_itr.itr().point().id;
    vector<unsigned> ids_left;

    for (unsigned id = cur_id; id <= g_db->maxid(); ++id) {
        if (g_db->is_in_subset(id)) 
            ids_left.push_back(id);
    }
    vdebug("%ld ids left (cur id: %d, max id: %d)\n", ids_left.size(), cur_id, g_db->maxid());

    // depending on the estimated run-time set the number of kids on a value between min_processes and max_processes
    int min_processes = min(g_naryn->min_processes() - 1, num_cores - 1);
    int max_processes = min(g_naryn->max_processes() - 1, num_cores - 1);
    int num_kids = min_processes + (double)(max_processes - min_processes) * (estimated_runtime - MIN_MULTITASKING_TIME) / (double)(MAX_MULTITASKING_TIME - MIN_MULTITASKING_TIME);

    num_kids = min(num_kids, max_processes);
    num_kids = min(num_kids, (int)ids_left.size());

    if (num_kids <= 1)
        return;

    vdebug("Opening %d child processes\n", num_kids);

    vector<unsigned> ids_subset;

    g_naryn->prepare4multitasking();

    m_mtask_record_size = NRPoint::packed_size();
    switch (m_valtype) {
    case REAL_T:
        m_mtask_record_size += m_eval_exprs.size() * sizeof(double);
        break;
    case LOGICAL_T:
        m_mtask_record_size += m_eval_exprs.size() * sizeof(char);
        break;
    default:
        verror("Invalid value type");
    }

    for (int ikid = 0; ikid < num_kids; ++ikid) {
        size_t ids_subset_size = ids_left.size() / (num_kids - ikid);

        vdebug("Calculating ids subset, size: %ld\n", ids_subset_size);
        ids_subset.clear();
        ids_subset.reserve(ids_subset_size);
        for (size_t i = 0; i < ids_subset_size; ++i) {
            size_t idx = (size_t)(unif_rand() * ids_left.size());
            ids_subset.push_back(ids_left[idx]);
            swap(ids_left[idx], ids_left[ids_left.size() - 1]);
            ids_left.pop_back();
        }
        vdebug("Subset is ready\n");

        if (!ids_subset.empty() && !g_naryn->launch_process()) { // kid process
            kid_main_loop(ids_subset);
            exit(0);
        }
        vdebug("Launched child %d/%d\n", ikid + 1, num_kids);
    }

    m_multitasking = true;
    m_eval_buf_size = m_eval_buf_idx = 0;
    m_eval_buf_limit = 1024;

    m_expr_itr_points.resize(m_eval_buf_limit);
    m_eval_doubles_mtask.resize(m_eval_exprs.size());
    m_eval_ints_mtask.resize(m_eval_exprs.size());
    for (size_t i = 0; i < m_eval_exprs.size(); ++i) {
        m_eval_doubles_mtask[i].resize(m_eval_buf_limit);
        m_eval_ints_mtask[i].resize(m_eval_buf_limit);
        m_eval_doubles[i] = &m_eval_doubles_mtask[i].front();
        m_eval_ints[i] = &m_eval_ints_mtask[i].front();
    }

    m_mtask_buf_size = m_eval_buf_limit * m_mtask_record_size;

    delete []m_mtask_buf;
    m_mtask_buf = new char[m_mtask_buf_size];
    vdebug("end of start_multitasking\n");
}

void NRTrackExprScanner::kid_main_loop(vector<unsigned> &ids_subset)
{
    g_db->ids_subset(ids_subset, "", 1, false);

    m_mtask_buf_size = m_mtask_record_size * m_eval_buf_limit;
    delete []m_mtask_buf;
    m_mtask_buf = new char[m_mtask_buf_size];

    while (1) {
        // run the iterator for the next m_eval_buf_limit steps
        m_eval_buf_size = 0;

        while (m_eval_buf_size < m_eval_buf_limit) {
            const NRPoint &point = m_itr.itr().point();

            // Even though we set a new ids subset, our iterator might still point to an id that was used by the parent before the multitasking was launched.
            // This id might be not part of the new subset, yet the iterator might continue working with it.
            if (g_db->is_in_subset(point.id)) {
                m_expr_itr_points[m_eval_buf_size] = point;
                m_itr_times[m_eval_buf_size] = (double)point.timestamp.hour();
                m_expr_vars.set_vars(point, m_eval_buf_size);
                ++m_eval_buf_size;
            }

            if (!m_itr.next()) {
                for (unsigned i = m_eval_buf_size; i < m_eval_buf_limit; ++i)
                    m_itr_times[i] = 0;
                break;
            }
        }

        // evaluate the expressions in R
        for (unsigned iexpr = 0; iexpr < m_eval_exprs.size(); ++iexpr) {
            if (m_eval_exprs[iexpr] != R_NilValue) {
                runprotect(m_eval_bufs[iexpr]);
                m_eval_bufs[iexpr] = eval_in_R(m_eval_exprs[iexpr], g_naryn->env());
                if (Rf_length(m_eval_bufs[iexpr]) != (int)m_eval_buf_limit)
                    verror("Evaluation of expression \"%s\" produces a vector of size %d while expecting size %d",
                            m_track_exprs[iexpr].c_str(), Rf_length(m_eval_bufs[iexpr]), m_eval_buf_limit);
                if (isReal(m_eval_bufs[iexpr])) {
                    if (m_valtype != REAL_T)
                        verror("Expression \"%s\" does not produce a numeric result.", m_track_exprs[iexpr].c_str());
                    m_eval_doubles[iexpr] = REAL(m_eval_bufs[iexpr]);
                } else if (isLogical(m_eval_bufs[iexpr])) {
                    if (m_valtype != LOGICAL_T)
                        verror("Expression \"%s\" does not produce a logical result.", m_track_exprs[iexpr].c_str());
                    m_eval_ints[iexpr] = LOGICAL(m_eval_bufs[iexpr]);
                } else
                    verror("Evaluation of expression \"%s\" produces a vector of unsupported type %s",
                            m_track_exprs[iexpr].c_str(), type2char(TYPEOF(m_eval_bufs[iexpr])));
            }
        }

        // pack the values
        char *p = m_mtask_buf;

        for (int ieval = 0; ieval < m_eval_buf_size; ++ieval) {
            m_expr_itr_points[ieval].pack(p);
            p += NRPoint::packed_size();
            
            switch (m_valtype) {
            case REAL_T:
                for (unsigned iexpr = 0; iexpr < m_eval_exprs.size(); ++iexpr) {
                    *(double *)p = m_eval_doubles[iexpr][ieval];
                    p += sizeof(double);
                }
                break;
            case LOGICAL_T:
                for (unsigned iexpr = 0; iexpr < m_eval_exprs.size(); ++iexpr) {
                    *p = (char)m_eval_ints[iexpr][ieval];
                    ++p;
                }
                break;
            }
        }

        g_naryn->write_multitask_fifo(m_mtask_buf, p - m_mtask_buf);
        Naryn::itr_idx(m_itr.itr().idx());

        if (m_itr.isend()) {
            Naryn::itr_idx(m_itr.itr().size());
            break;
        }
    }
}

void NRTrackExprScanner::create_expr_iterator(SEXP rtrack_exprs, SEXP rstime, SEXP retime, SEXP iterator_policy, SEXP rkeepref,
                                              SEXP filter, bool call_begin)
{
    m_track_exprs.resize(Rf_length(rtrack_exprs));
    for (int i = 0; i < Rf_length(rtrack_exprs); ++i)
        m_track_exprs[i] = CHAR(STRING_ELT(rtrack_exprs, i));

    unsigned stime, etime;
    convert_rscope(rstime, retime, &stime, &etime);

    m_expr_vars.parse_exprs(m_track_exprs, false, stime, etime);

    // initiate the expression iterator
    create_expr_iterator(&m_itr, iterator_policy, convert_rkeepref(rkeepref), m_expr_vars, m_track_exprs, stime, etime, filter, call_begin);
}

void NRTrackExprScanner::create_expr_iterator(IteratorWithFilter *itr, SEXP riterator, bool keepref, const NRTrackExpressionVars &vars,
                                              const vector<string> &track_exprs, unsigned stime, unsigned etime, SEXP filter, bool call_begin)
{
    NRTrackExpressionIterator *expr_itr = NULL;

    if ((isReal(riterator) || isInteger(riterator)) && Rf_length(riterator) == 1)            // iterator == period
        expr_itr = new NRBeatIterator(asInteger(riterator), keepref, stime, etime);
    else if (isString(riterator) && Rf_length(riterator) == 1 && g_db->track(CHAR(asChar(riterator))))
        expr_itr = new NRTrackIterator(g_db->track(CHAR(asChar(riterator))), keepref, stime, etime);
    else if (isNull(riterator)) {
        string track_name;

        for (unsigned ivar = 0; ivar < vars.get_num_track_vars(); ++ivar) {
			if (track_name.empty())
                track_name = vars.get_track_name(ivar);
            else if (track_name != vars.get_track_name(ivar)) {
                if (m_track_exprs.size() == 1)
                    verror("Unable to implicitly set iterator policy: track expression contains several different tracks");
                else
                    verror("Unable to implicitly set iterator policy: track expressions contain several different tracks");
            }
        }

        if (track_name.empty()) {
            if (m_track_exprs.size() == 1)
                verror("Unable to implicitly set iterator policy: track expression does not contain any tracks");
            else
                verror("Unable to implicitly set iterator policy: track expressions do not contain any tracks");
        }

        expr_itr = new NRTrackIterator(g_db->track(track_name.c_str()), keepref, stime, etime);
    } else {
        bool success = false;
        NRPoints points;

        try {
            NRPoint::convert_rpoints(riterator, &points);
            success = true;
        } catch (TGLException &e) {
            if (e.type() == typeid(NRPoint) && e.code() != NRPoint::BAD_FORMAT) 
                verror("Iterator: %s", e.msg());
        }

        if (success) {
            try {
                expr_itr = new NRPointsIterator(points, keepref, stime, etime);
            } catch (TGLException &e) {
                verror("Iterator: %s", e.msg());
            }
        }

        if (!success) {
            NRIdTimeIntervals intervs;

            try {
                NRIdTimeIntervals::convert_rid_time_intervals(riterator, &intervs);
                success = true;
            } catch (TGLException &e) {
                if (e.type() == typeid(NRIdTimeIntervals) && e.code() != NRIdTimeIntervals::BAD_FORMAT) 
                    verror("Iterator: %s", e.msg());
            }

            if (success) {
                try {
                    expr_itr = new NRIdTimeIntervalsIterator(intervs, keepref, stime, etime);
                } catch (TGLException &e) {
                    verror("Iterator: %s", e.msg());
                }
            }
        }

        if (!success) {
            vector<unsigned> ids;

            try {
                NRPoint::convert_rids(riterator, &ids);
                success = true;
            } catch (TGLException &e) {
                if (e.type() == typeid(NRPoint) && e.code() != NRPoint::BAD_FORMAT) 
                    verror("Iterator: %s", e.msg());
            }

            if (success) {
                try {
                    expr_itr = new NRIdsIterator(ids, keepref, stime, etime);
                } catch (TGLException &e) {
                    verror("Iterator: %s", e.msg());
                }
            }
        }

        if (!success) {
            NRTimeIntervals intervs;

            try {
                NRTimeIntervals::convert_rtime_intervals(riterator, &intervs);
                success = true;
            } catch (TGLException &e) {
                if (e.type() == typeid(NRTimeIntervals) && e.code() != NRTimeIntervals::BAD_FORMAT) 
                    verror("Iterator: %s", e.msg());
            }

            if (success) {
                try {
                    expr_itr = new NRTimesIterator(intervs, keepref, stime, etime);
                } catch (TGLException &e) {
                    verror("Iterator: %s", e.msg());
                }
            }
        }

        if (!success && isVector(riterator) && Rf_length(riterator) == 2) {
            SEXP rbeat = VECTOR_ELT(riterator, 0);
            SEXP rinit = VECTOR_ELT(riterator, 1);

            if ((isReal(rbeat) || isInteger(rbeat)) && Rf_length(rbeat) == 1) {
                if (isString(rinit) && Rf_length(rinit) == 1 && g_db->track(CHAR(asChar(rinit)))) {
                    NRTrackIterator *itr = new NRTrackIterator(g_db->track(CHAR(asChar(rinit))), keepref, g_db->mintime(), etime);
                    try {
                        expr_itr = new NRBeatExtIterator(asInteger(rbeat), itr, keepref, stime, etime);
                    } catch (...) {
                        delete itr;
                        throw;
                    }
                    success = true;
                } else {
                    try {
                        NRPoint::convert_rpoints(rinit, &points);
                        success = true;
                    } catch (TGLException &e) {
                        if (e.type() == typeid(NRPoint) && e.code() != NRPoint::BAD_FORMAT) 
                            verror("Iterator: %s", e.msg());
                    }

                    if (success) {
                        NRPointsIterator *itr = new NRPointsIterator(points, keepref, g_db->mintime(), etime);
                        try {
                            expr_itr = new NRBeatExtIterator(asInteger(rbeat), itr, keepref, stime, etime);
                        } catch (...) {
                            delete itr;
                            throw;
                        }
                        success = true;
                    }
                }
            }
        }

        if (!success)
            verror("Invalid iterator parameter");
    }

    itr->init(expr_itr, filter, stime, etime);
    if (call_begin)
        itr->begin();
}

