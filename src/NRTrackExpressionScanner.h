#ifndef NRTRACKEXPRESSIONSCANNER_H_INCLUDED
#define NRTRACKEXPRESSIONSCANNER_H_INCLUDED

#include <ctype.h>

#include <vector>
#include <string>

#include "EMRTrack.h"
#include "EMRTrackExpressionIterator.h"
#include "naryn.h"
#include "NRIteratorFilter.h"
#include "NRTrackExpressionVars.h"

using namespace std;

class NRTrackExprScanner {
public:
    enum ValType { REAL_T, LOGICAL_T };

	NRTrackExprScanner();
    ~NRTrackExprScanner();

	const vector<string> &get_track_exprs() const { return m_track_exprs; }

    const NRTrackExpressionVars &get_expr_vars() const { return m_expr_vars; }

	void check(const vector<string> &track_exprs, unsigned stime, unsigned etime, bool is_implicit_scope, SEXP iterator_policy, bool keepref, SEXP filter);
	void check(SEXP track_exprs, SEXP stime, SEXP etime, SEXP iterator_policy, SEXP keepref, SEXP filter);
	bool begin(const vector<string> &track_exprs, ValType valtype, unsigned stime, unsigned etime, bool is_implicit_scope, SEXP iterator_policy, bool keepref, SEXP filter);
	bool begin(SEXP track_exprs, ValType valtype, SEXP stime, SEXP etime, SEXP iterator_policy, SEXP keepref, SEXP filter);
	bool next();
	bool isend() { return m_isend; }

	void report_progress(bool do_report_progress) { m_do_report_progress = do_report_progress; }

	// returns the result of last evaluation of track expression as REAL
	double real(int track_expr_idx = 0) const { return m_eval_doubles[track_expr_idx][m_eval_buf_idx]; }

	// returns the result of last evaluation of track expression as LOGICAL
	// 1 = true, 0 = false, -1 = NA
	int    logical(int track_expr_idx = 0) const { return m_eval_ints[track_expr_idx][m_eval_buf_idx]; }

	const EMRPoint &point() const { return m_expr_itr_points[m_eval_buf_idx]; }

    bool are_points_sorted() const { return !m_multitasking; }

	// Gets the tracks names that appear in the track expression, creates a TrackExpressionIterator after begin is being called.
	void create_expr_iterator(SEXP rtrack_exprs, SEXP stime, SEXP etime, SEXP iterator_policy, SEXP keepref, SEXP filter, bool call_begin = true);

    static void convert_rscope(SEXP rstime, SEXP retime, unsigned *pstime, unsigned *petime, bool *is_implicit_scope);
    static bool convert_rkeepref(SEXP rkeepref);

private:
    class IteratorWithFilter {
    public:
        IteratorWithFilter() { m_itr = NULL; m_isend = true; }
        ~IteratorWithFilter() { delete m_itr; }

        void init(EMRTrackExpressionIterator *itr, SEXP filter, unsigned stime, unsigned etime);

        const EMRTrackExpressionIterator &itr() const { return *m_itr; }
        const NRIteratorFilter           &filter() const { return m_filter; }

        // returns false if end is reached
        bool begin();

        // returns false if end is reached
        bool next();

        bool isend() const { return m_isend; }

    private:
        EMRTrackExpressionIterator *m_itr;
        NRIteratorFilter            m_filter;
        bool                        m_isend;

        bool apply_filter();
    };

	vector<string>   m_track_exprs;
	unsigned         m_num_track_vars;
    ValType          m_valtype;
    bool             m_multitasking;
    char            *m_mtask_buf;
    size_t           m_mtask_record_size;
    size_t           m_mtask_buf_size;

	vector<SEXP>     m_eval_exprs;
	vector<SEXP>     m_eval_bufs;
	vector<double *> m_eval_doubles;
	vector<int *>    m_eval_ints;
    vector< vector<double> >  m_eval_doubles_mtask;
    vector< vector<int> >     m_eval_ints_mtask;
    SEXP             m_ritr_times{R_NilValue};
    double          *m_itr_times;
	unsigned         m_eval_buf_idx;
	unsigned         m_eval_buf_limit;
	unsigned         m_eval_buf_size;

	int              m_last_progress_reported;
	size_t           m_num_evals;
	int              m_report_step;
	uint64_t         m_last_report_clock;
	EMRPoints        m_expr_itr_points;
	bool             m_isend;
	bool             m_do_report_progress;

	IteratorWithFilter         m_itr;
	NRTrackExpressionVars      m_expr_vars;

	static const int INIT_REPORT_STEP;
	static const int REPORT_INTERVAL;         // report interval in milliseconds
	static const int MIN_REPORT_INTERVAL;     // report interval in milliseconds
    static const int MIN_MULTITASKING_TIME;   // minimal run time required to switch to multitasking
    static const int MAX_MULTITASKING_TIME;   // run-time threshold that triggers maximal number of processes to be used while multitasking
    static const int CHECK_MULTITASKING_TIME; // time after which run-times are checked and decision to switch to multitasking is taken

	void convert_rtrack_exprs(SEXP rtrack_exprs, vector<string> &track_exprs);
	void define_r_vars(unsigned eval_buf_limit);

    bool next_multitasking();
	bool eval_next();
	void report_progress();

    void start_multitasking();
    void kid_main_loop(vector<unsigned> &ids_subset);

	void create_expr_iterator(IteratorWithFilter *itr, SEXP riterator, bool keepref, const NRTrackExpressionVars &vars, const vector<string> &track_exprs,
                              unsigned stime, unsigned etime, bool is_implicit_scope, SEXP filter, bool call_begin = true);
};


//------------------------------ IMPLEMENTATION ----------------------------------------

inline void NRTrackExprScanner::IteratorWithFilter::init(EMRTrackExpressionIterator *itr, SEXP filter, unsigned stime, unsigned etime)
{
    m_isend = true;
    delete m_itr;
    m_itr = itr;
    if (!isNull(filter))
        m_filter.init(filter, stime, etime);
}

inline bool NRTrackExprScanner::IteratorWithFilter::begin()
{
    m_isend = false;
    m_itr->begin();
    return apply_filter();
}

inline bool NRTrackExprScanner::IteratorWithFilter::next()
{
    if (m_isend)
        return false;
    m_itr->next();
    return apply_filter();
}

inline bool NRTrackExprScanner::IteratorWithFilter::apply_filter()
{
    while (!m_itr->isend()) {
        if (m_filter.is_passed(m_itr->point()))
            return true;

        if (m_filter.jumpto().id == (unsigned)-1) {
            m_isend = true;
            return false;
        }

        if (m_filter.jumpto().id == m_itr->point().id && m_filter.jumpto().timestamp.hour() == m_itr->point().timestamp.hour())
            m_itr->next();
        else
            m_itr->next(m_filter.jumpto());
    }

    m_isend = true;
    return false;
}

inline bool NRTrackExprScanner::next()
{
    if (m_multitasking) {
        if (next_multitasking())
            return true;
    } else if (eval_next())
        return true;

    // did we start reporting progress?
    if (m_last_progress_reported >= 0) {
        if (m_last_progress_reported != 100)
            Rprintf("100%%\n");
        else
            Rprintf("\n");
    }

    if (!m_multitasking)
        runprotect(m_eval_bufs);
    return false;
}

inline bool NRTrackExprScanner::eval_next()
{
    m_eval_buf_idx++;

    // do we need to read more track samples to the buffer?
    if (m_eval_buf_idx >= m_eval_buf_limit) {
        // check the progress and decide whether multitasking is required
        if (Naryn::alarm_fired() && !m_multitasking) {
            start_multitasking();
            if (m_multitasking)
                return next_multitasking();
        }

        m_eval_buf_idx = 0;
        for (m_eval_buf_size = 0; m_eval_buf_size < m_eval_buf_limit; ++m_eval_buf_size) {
            if (m_itr.isend()) {
                for (unsigned i = m_eval_buf_size; i < m_eval_buf_limit; ++i)
                    m_itr_times[i] = 0;
                break;
            }

            const EMRPoint &point = m_itr.itr().point();
            m_expr_itr_points[m_eval_buf_size] = point;
            m_itr_times[m_eval_buf_size] = (double)point.timestamp.hour();
            m_expr_vars.set_vars(point, m_eval_buf_size);
            m_itr.next();
        }

        check_interrupt();

        for (unsigned iexpr = 0; iexpr < m_eval_exprs.size(); ++iexpr) {
            if (m_eval_exprs[iexpr] != R_NilValue) {
                runprotect(m_eval_bufs[iexpr]);
                m_eval_bufs[iexpr] = eval_in_R(m_eval_exprs[iexpr], g_naryn->env());
                if (Rf_length(m_eval_bufs[iexpr]) != (int)m_eval_buf_limit)
                    verror("Evaluation of expression \"%s\" produces a vector of size %d while expecting size %d",
                            m_track_exprs[iexpr].c_str(), Rf_length(m_eval_bufs[iexpr]), m_eval_buf_limit);
                if (isReal(m_eval_bufs[iexpr])) {
                    if (m_valtype != REAL_T) {
                        defineVar(install("EMR_ERROR_EXPR"), m_eval_bufs[iexpr], findVar(install(".GlobalEnv"), g_naryn->env()));
                        verror("Expression \"%s\" does not produce a numeric result.\n"
                                "The result of the last expression evaluation was saved in EMR_ERROR_EXPR variable.", m_track_exprs[iexpr].c_str());
                    }
                    m_eval_doubles[iexpr] = REAL(m_eval_bufs[iexpr]);
                } else if (isLogical(m_eval_bufs[iexpr])) {
                    if (m_valtype != LOGICAL_T) {
                        defineVar(install("EMR_ERROR_EXPR"), m_eval_bufs[iexpr], findVar(install(".GlobalEnv"), g_naryn->env()));
                        verror("Expression \"%s\" does not produce a logical result.\n"
                                "The result of the last expression evaluation was saved in EMR_ERROR_EXPR variable.", m_track_exprs[iexpr].c_str());
                    }
                    m_eval_ints[iexpr] = LOGICAL(m_eval_bufs[iexpr]);
                } else
                    verror("Evaluation of expression \"%s\" produces a vector of unsupported type %s",
                            m_track_exprs[iexpr].c_str(), type2char(TYPEOF(m_eval_bufs[iexpr])));
            }
        }

        report_progress();
    }

    if (m_eval_buf_idx >= m_eval_buf_size) {
        m_eval_buf_idx = m_eval_buf_limit;
        m_isend = true;
    }

    return !m_isend;
}

inline bool NRTrackExprScanner::next_multitasking()
{
    m_eval_buf_idx++;

    // do we need to read more track samples to the buffer?
    if (m_eval_buf_idx >= m_eval_buf_size) {
        m_eval_buf_idx = 0;

        int bytes_read = g_naryn->read_multitask_fifo(m_mtask_buf, m_mtask_buf_size);

        if (bytes_read) {
            char *p = m_mtask_buf;

            if (bytes_read % m_mtask_record_size != 0)
                verror("Invalid FIFO format");

            m_eval_buf_size = bytes_read / m_mtask_record_size;

            for (unsigned i = 0; i < m_eval_buf_size; ++i) {
                m_expr_itr_points[i].unpack(p);
                p += EMRPoint::packed_size();

                switch (m_valtype) {
                case REAL_T:
                    {
                        for (unsigned iexpr = 0; iexpr < m_eval_exprs.size(); ++iexpr) {
                            m_eval_doubles[iexpr][i] = *(double *)p;
                            p += sizeof(double);
                        }
                    }
                    break;
                case LOGICAL_T:
                    {
                        for (unsigned iexpr = 0; iexpr < m_eval_exprs.size(); ++iexpr) {
                            m_eval_ints[iexpr][i] = *p;
                            ++p;
                        }
                    }
                    break;
                }
            }
            report_progress();
        } else {
            m_eval_buf_idx = m_eval_buf_limit;
            m_isend = true;
            return false;
        }
    }

    return true;
}

#endif

