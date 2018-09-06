#include <R.h>
#include <Rinternals.h>

#ifdef length
#undef length
#endif
#ifdef error
#undef error
#endif

#include "naryn.h"
#include "NRDb.h"
#include "NRIdTimeIntervalsIterator.h"
#include "NRIdsIterator.h"
#include "NRIteratorFilter.h"
#include "NRPointsIterator.h"
#include "NRTimesIterator.h"
#include "NRTrackIterator.h"

void NRIteratorFilter::init(SEXP filter, unsigned stime, unsigned etime)
{
    SEXP emr_filters = R_NilValue;
    vector<SEXP> rfilter_names;
    vector<SEXP> filters;

    // retrieve filter names (named filters are burried in a list of lists)
    rprotect(emr_filters = findVar(install("EMR_FILTERS"), g_naryn->env()));

    if (!isNull(emr_filters) && !isSymbol(emr_filters)) {
        SEXP roots = getAttrib(emr_filters, R_NamesSymbol);

        if (!isVector(emr_filters) || Rf_length(emr_filters) && !isString(roots) || Rf_length(roots) != Rf_length(emr_filters))
            verror("Invalid format of EMR_FILTERS variable (1).\n"
                   "To continue working with filters please remove this variable from the environment.");

        for (int i = 0; i < Rf_length(roots); ++i) {
            if (g_db->grootdir() == CHAR(STRING_ELT(roots, i)) || g_db->urootdir() == CHAR(STRING_ELT(roots, i))) {
                filters.push_back(VECTOR_ELT(emr_filters, i));
                SEXP filter_names = getAttrib(filters.back(), R_NamesSymbol);

                if (!isVector(filters.back()) || Rf_length(filters.back()) && !isString(filter_names) || Rf_length(filter_names) != Rf_length(filters.back()))
                    verror("Invalid format of EMR_FILTERS variable (2).\n"
                           "To continue working with filters please remove this variable from the environment.");

                rfilter_names.push_back(filter_names);
            }
        }
    }

    if (isLanguage(filter)) {
        try {
            build_subtree(filters, rfilter_names, filter, &m_tree, false, stime, etime, 0);
            if (!m_tree->is_leaf()) {
                vector<NRIteratorFilterItem *> op_nodes;
                vector<NRIteratorFilterItem *> end_nodes;
                optimize_subtree(m_tree, NRIteratorFilterItem::NONE, op_nodes, end_nodes, 0);
            }
        } catch(...) {
            delete m_tree;
            m_tree = NULL;
            throw;
        }
    } else {
        if (!isString(filter) && !isSymbol(filter) || Rf_length(filter) != 1)
            verror("Invalid filter (1)");

        m_tree = create_filter_item(filters, rfilter_names, CHAR(asChar(filter)), false, stime, etime);
    }
}

void NRIteratorFilter::build_subtree(vector<SEXP> &filters, vector<SEXP> &rfilter_names, SEXP filter, NRIteratorFilterItem **tree,
                                     bool operator_not, unsigned stime, unsigned etime, int depth)
{
    int idx = 0;

    while (1) {
        SEXP data = CAR(filter);

        if (isLanguage(data)) {
            if (idx > 2)
                verror("Syntax error in filter (2)");
            build_subtree(filters, rfilter_names, data, *tree ? &(*tree)->m_child[idx - 1] : tree, operator_not, stime, etime, depth + 1);
        } else {
            const char *str = CHAR(asChar(data));

            if (!idx && !strcmp(str, "(")) {
                filter = CDR(filter);
                if (isNull(filter))
                    verror("Syntax error in filter (3)");
                continue;
            }

            if (!idx) {
                if (!strcmp(str, "&") || !strcmp(str, "&&")) {
//for (int j = 0; j < depth; ++j)
//printf("  ");
//printf("& (not: %d)\n", operator_not);
                    *tree = new NRIteratorFilterItem();
                    (*tree)->m_op = operator_not ? NRIteratorFilterItem::OR : NRIteratorFilterItem::AND;
                } else if (!strcmp(str, "|") || !strcmp(str, "||")) {
//for (int j = 0; j < depth; ++j)
//printf("  ");
//printf("& (not: %d)\n", operator_not);
                    *tree = new NRIteratorFilterItem();
                    (*tree)->m_op = operator_not ? NRIteratorFilterItem::AND : NRIteratorFilterItem::OR;
                } else if (!strcmp(str, "!")) {
//for (int j = 0; j < depth; ++j)
//printf("  ");
//printf("! (not: %d)\n", operator_not);
                    operator_not = !operator_not;
                    filter = CDR(filter);
                    if (isNull(filter))
                        verror("Syntax error in filter (4)");
                    continue;
                } else if (!strlen(str))
                    verror("Syntax error in filter (5)");
                else if (!isalpha(str[0]) && str[0] != '.')
                    verror("Unsupported operator '%s' used in filter (6)", str);
                else {
                    // track or list surrounded by brackets
//for (int j = 0; j < depth; ++j)
//printf("  ");
//printf("track %s surrounded by brackets (not: %d)\n", str, operator_not);

                    if (!isNull(CDR(filter)))
                        verror("Syntax error in filter (6)");

                    *tree = create_filter_item(filters, rfilter_names, str, operator_not, stime, etime);
                }
            } else if (idx > 2)
                verror("Syntax error in filter (7)");
            else
//{
//for (int j = 0; j < depth; ++j)
//printf("  ");
//printf("track %s without brackets (not: %d)\n", str, operator_not);
                // track or list
                (*tree)->m_child[idx - 1] = create_filter_item(filters, rfilter_names, str, operator_not, stime, etime);
//            }

        }

        filter = CDR(filter);
        if (isNull(filter))
            break;

        ++idx;
    }
}

NRIteratorFilterItem *NRIteratorFilter::create_filter_item(vector<SEXP> &filters, vector<SEXP> &rfilter_names, const char *str,
                                                           bool operator_not, unsigned stime, unsigned etime)
{
    for (size_t i = 0; i < filters.size(); ++i) {
        for (int ifilter = 0; ifilter < Rf_length(rfilter_names[i]); ++ifilter) {
            const char *filter_name = CHAR(STRING_ELT(rfilter_names[i], ifilter));

            if (!strcmp(str, filter_name))
                return create_filter_item(VECTOR_ELT(filters[i], ifilter), str, operator_not, stime, etime);
        }
    }

    NRIteratorFilterItem *filter = NULL;

    try {
        filter = new NRIteratorFilterItem();
        filter->m_is_not = operator_not;
        filter->m_stime = (int)stime;
        filter->m_etime = (int)etime;
        filter->m_keepref = false;

        NRTrack *track = g_db->track(str);

        if (track) {
            filter->m_itr = new NRTrackIterator(track, filter->m_keepref, stime, etime);
            return filter;
        }

        SEXP rval = findVar(install(str), g_naryn->env());
        bool success = false;

        NRPoints points;
        try {
            NRPoint::convert_rpoints(rval, &points);
            success = true;
        } catch (TGLException &e) {
            if (e.type() == typeid(NRPoint) && e.code() != NRPoint::BAD_FORMAT) 
                verror("Filter item %s: %s", str, e.msg());
        }

        if (success) {
            try {
                filter->m_itr = new NRPointsIterator(points, filter->m_keepref, stime, etime);
                return filter;
            } catch (TGLException &e) {
                verror("Filter item %s: %s", str, e.msg());
            }
        }

        NRIdTimeIntervals id_time_intervs;
        try {
            NRIdTimeIntervals::convert_rid_time_intervals(rval, &id_time_intervs);
            success = true;
        } catch (TGLException &e) {
            if (e.type() == typeid(NRIdTimeIntervals) && e.code() != NRIdTimeIntervals::BAD_FORMAT) 
                verror("Filter item %s: %s", str, e.msg());
        }

        if (success) {
            try {
                filter->m_itr = new NRIdTimeIntervalsIterator(id_time_intervs, filter->m_keepref, stime, etime);
                return filter;
            } catch (TGLException &e) {
                verror("Filter item %s: %s", str, e.msg());
            }
        }

        vector<unsigned> ids;
        try {
            NRPoint::convert_rids(rval, &ids);
            success = true;
        } catch (TGLException &e) {
            if (e.type() == typeid(NRPoint) && e.code() != NRPoint::BAD_FORMAT) 
                verror("Filter item %s: %s", str, e.msg());
        }

        if (success) {
            try {
                filter->m_itr = new NRIdsIterator(ids, filter->m_keepref, stime, etime);
                return filter;
            } catch (TGLException &e) {
                verror("Filter item %s: %s", str, e.msg());
            }
        }

        NRTimeIntervals time_intervs;
        try {
            NRTimeIntervals::convert_rtime_intervals(rval, &time_intervs);
            success = true;
        } catch (TGLException &e) {
            if (e.type() == typeid(NRTimeIntervals) && e.code() != NRTimeIntervals::BAD_FORMAT) 
                verror("Filter item %s: %s", str, e.msg());
        }

        if (success) {
            try {
                filter->m_itr = new NRTimesIterator(time_intervs, filter->m_keepref, stime, etime);
                return filter;
            } catch (TGLException &e) {
                verror("Filter item %s: %s", str, e.msg());
            }
        }

        if (strlen(str)) {
            if (isalpha(str[0]) || str[0] == '.')
                verror("Filter: %s neither a track nor a named filter (9)", str);
            verror("Unsupported operator '%s' used in the filter (10)", str);
        }
        verror("Syntax error in filter (11)");
    }
    catch (...) {
        delete filter;
        throw;
    }

    return filter;
}

NRIteratorFilterItem *NRIteratorFilter::create_filter_item(SEXP rfilter, const char *name, bool operator_not, unsigned stime, unsigned etime)
{
    NRIteratorFilterItem *filter = NULL;

    try {
        filter = new NRIteratorFilterItem();
        filter->m_is_not = operator_not;
        filter->m_stime = (int)stime;
        filter->m_etime = (int)etime;

        SEXP rtshift = get_rvector_col(rfilter, "time_shift", name, false);

        if (isNull(rtshift))
            filter->m_sshift = filter->m_eshift = 0;
        else {
            if (!(isReal(rtshift) || isInteger(rtshift)) || Rf_length(rtshift) < 1 || Rf_length(rtshift) > 2)
                verror("Filter %s: 'time.shift' must be an integer or a pair of integers", name);

            if (Rf_length(rtshift) == 1)
                filter->m_sshift = filter->m_eshift = isReal(rtshift) ? (int)REAL(rtshift)[0] : INTEGER(rtshift)[0];
            else {
                filter->m_sshift = isReal(rtshift) ? (int)REAL(rtshift)[0] : INTEGER(rtshift)[0];
                filter->m_eshift = isReal(rtshift) ? (int)REAL(rtshift)[1] : INTEGER(rtshift)[1];
                if (filter->m_sshift > filter->m_eshift)
                    swap(filter->m_sshift, filter->m_eshift);
            }

            if (filter->m_sshift < -(int)NRTimeStamp::MAX_HOUR || filter->m_sshift > (int)NRTimeStamp::MAX_HOUR ||
                filter->m_eshift < -(int)NRTimeStamp::MAX_HOUR || filter->m_eshift > (int)NRTimeStamp::MAX_HOUR)
                verror("Filter %s: 'time.shift' is out of range", name);
        }

        int _stime = filter->m_stime + filter->m_sshift;
        int _etime = filter->m_etime + filter->m_eshift;
        _stime = max(_stime, 0);
        _stime = min(_stime, (int)NRTimeStamp::MAX_HOUR);
        _etime = max(_etime, 0);
        _etime = min(_etime, (int)NRTimeStamp::MAX_HOUR);

        SEXP rkeepref = get_rvector_col(rfilter, "keepref", name, true);

        if (!isLogical(rkeepref) || Rf_length(rkeepref) != 1 || asLogical(rkeepref) == NA_LOGICAL)
            verror("Filter %s: 'keepref' must be a logical value", name);
        filter->m_keepref = asLogical(rkeepref);

        if (filter->m_keepref && (filter->m_sshift || filter->m_eshift))
            verror("Filter %s: 'time.shift' is not allowed when keepref is 'TRUE'", name);

        SEXP rval = get_rvector_col(rfilter, "val", name, false);
        SEXP rexpiration = get_rvector_col(rfilter, "expiration", name, false);
        SEXP rsrc = get_rvector_col(rfilter, "src", name, true);

        if (isString(rsrc)) { // track name
            if (Rf_length(rsrc) != 1)
                verror("Filter %s: invalid 'src'", name);

            const char *track_name = CHAR(STRING_ELT(rsrc, 0));
            NRTrack *track = g_db->track(track_name);

            if (!track)
                verror("Filter %s: track %s does not exist", name, track_name);

            if (!isNull(rval) && !track->is_categorial())
                verror("Filter %s: 'val' parameter can be used only with categorial tracks", name);

            if (!isNull(rval) && (!isReal(rval) && !isInteger(rval)))
                verror("Filter %s: 'val' must be a numeric vector", name);

            unordered_set<double> vals;
            for (int i = 0; i < Rf_length(rval); ++i)
                // The track might contain its data as float and not double. In this case a track value might not be equal to its double representation,
                // like (float)0.3 != (double)0.3. So let's "downgrade" all our values to the least precise type.
                vals.insert(isReal(rval) ? (float)REAL(rval)[i] : (float)INTEGER(rval)[i]);

            double expiration;
            if (isNull(rexpiration))
                expiration = 0;
            else if (!isReal(rexpiration) && !isInteger(rexpiration) || Rf_length(rexpiration) != 1)
                verror("Filter %s: 'expiration' must be a positive integer", name);
            else if (filter->m_keepref)
                verror("Filter %s: 'expiration' cannot be used when keepref is 'TRUE'", name);
            else if (!track->is_categorial())
                verror("Filter %s: 'expiration' can be used only with categorial tracks", name);
            else {
                expiration = asReal(rexpiration);
                if (expiration < 1 || expiration != (int)expiration)
                    verror("Filter %s: 'expiration' must be a positive integer", name);
                if (expiration > NRTimeStamp::MAX_HOUR)
                    verror("Filter %s: 'expiration' is out of range", name);
            }

            filter->m_itr = new NRTrackIterator(track, filter->m_keepref, _stime, _etime, move(vals), expiration);
        } else {   // id-time list
            NRPoints points;
            try {
                NRPoint::convert_rpoints(rsrc, &points);

                if (!isNull(rval))
                    verror("'val' parameter can be used only with categorial tracks");

                if (!isNull(rexpiration))
                    verror("'expiration' parameter can be used only with tracks");

                filter->m_itr = new NRPointsIterator(points, filter->m_keepref, _stime, _etime);
            } catch (TGLException &e) {
                if (e.type() == typeid(NRPoint) && e.code() != NRPoint::BAD_FORMAT) 
                    verror("Filter %s: 'src' is neigther a track nor an id-time data frame", name);

                verror("Filter item %s: %s", name, e.msg());
            }
        }
    }
    catch (...) {
        delete filter;
        throw;
    }

    return filter;
}

int NRIteratorFilter::optimize_subtree(NRIteratorFilterItem *tree, NRIteratorFilterItem::Op op,
                                       vector<NRIteratorFilterItem *> &op_nodes, vector<NRIteratorFilterItem *> &end_nodes, int depth)
{
    if (tree->is_leaf())
        end_nodes.push_back(tree);
    else {
        if (tree->m_op == op) {
            op_nodes.push_back(tree);
            int depth1 = optimize_subtree(tree->m_child[0], op, op_nodes, end_nodes, depth + 1);
            int depth2 = optimize_subtree(tree->m_child[1], op, op_nodes, end_nodes, depth + 1);
            return max(depth1, depth2);
        } else {
            end_nodes.push_back(tree);

            vector<NRIteratorFilterItem *> _op_nodes;
            vector<NRIteratorFilterItem *> _end_nodes;

            int depth1 = optimize_subtree(tree->m_child[0], tree->m_op, _op_nodes, _end_nodes, 1);
            int depth2 = optimize_subtree(tree->m_child[1], tree->m_op, _op_nodes, _end_nodes, 1);
            int _depth = max(depth1, depth2);

            if (_end_nodes.size() > 3) {  // under 3 end nodes there is no room for optimization
                int optimal_depth = 0;

                for (size_t num = _end_nodes.size() - 1; num; num = num >> 1)
                    ++optimal_depth;

                if (optimal_depth < _depth)
                    build_balanced_tree(tree, tree->m_op, _end_nodes.begin(), _end_nodes.end());

                for (vector<NRIteratorFilterItem *>::iterator inode = _op_nodes.begin(); inode != _op_nodes.end(); ++inode) {
                    (*inode)->m_child[0] = (*inode)->m_child[1] = NULL;    // prevent recursive deletion of the node
                    delete *inode;
                }
            }
        }
    }
    return depth;
}

void NRIteratorFilter::build_balanced_tree(NRIteratorFilterItem *tree, NRIteratorFilterItem::Op op,
                                           vector<NRIteratorFilterItem *>::const_iterator isnode, vector<NRIteratorFilterItem *>::const_iterator ienode)
{
    tree->m_op = op;

    if (ienode - isnode == 2) {
        tree->m_child[0] = *isnode;
        tree->m_child[1] = *(isnode + 1);
    } else if (ienode - isnode == 3) {
        tree->m_child[0] = *isnode;
        tree->m_child[1] = new NRIteratorFilterItem();
        build_balanced_tree(tree->m_child[1], op, isnode + 1, ienode);
    } else {
        vector<NRIteratorFilterItem *>::const_iterator imid_node = isnode + (ienode - isnode) / 2;
        
        tree->m_child[0] = new NRIteratorFilterItem();
        tree->m_child[1] = new NRIteratorFilterItem();
        build_balanced_tree(tree->m_child[0], op, isnode, imid_node);
        build_balanced_tree(tree->m_child[1], op, imid_node, ienode);
    }
}

void NRIteratorFilter::check_named_filter(SEXP rfilter, const char *name)
{
    NRIteratorFilterItem *filter = create_filter_item(rfilter, name, false, 0, g_db->maxtime());
    delete filter;
}

void NRIteratorFilter::debug_print()
{
    if (m_tree)
        debug_print(m_tree, 0);
}

void NRIteratorFilter::debug_print(NRIteratorFilterItem *tree, int depth)
{
    tree->debug_print(depth);
    if (tree->m_child[0])
        debug_print(tree->m_child[0], depth + 1);
    if (tree->m_child[1])
        debug_print(tree->m_child[1], depth + 1);
}

extern "C" {

SEXP emr_check_named_filter(SEXP _filter, SEXP _name, SEXP _envir)
{
	try {
		Naryn naryn(_envir);

        if (!isString(_name) || Rf_length(_name) != 1)
            verror("Name of the filter is not a string");

		NRIteratorFilter::check_named_filter(_filter, CHAR(STRING_ELT(_name, 0)));
	} catch (TGLException &e) {
		rerror("%s", e.msg());
	}
	return R_NilValue;
}

SEXP emr_check_filter_attr_time_shift(SEXP _tshift, SEXP _envir)
{
	try {
		Naryn naryn(_envir);

		// check the arguments
        if (!(isReal(_tshift) || isInteger(_tshift)) || Rf_length(_tshift) < 1 || Rf_length(_tshift) > 2)
            verror("'time.shift' must be an integer or a pair of integers");

        int sshift, eshift;

        if (Rf_length(_tshift) == 1)
            sshift = eshift = isReal(_tshift) ? (int)REAL(_tshift)[0] : INTEGER(_tshift)[0];
        else {
            sshift = isReal(_tshift) ? (int)REAL(_tshift)[0] : INTEGER(_tshift)[0];
            eshift = isReal(_tshift) ? (int)REAL(_tshift)[1] : INTEGER(_tshift)[1];
        }

        if (sshift < -(int)NRTimeStamp::MAX_HOUR || sshift > (int)NRTimeStamp::MAX_HOUR ||
            eshift < -(int)NRTimeStamp::MAX_HOUR || eshift > (int)NRTimeStamp::MAX_HOUR)
            verror("'time.shift' is out of range");
	} catch (TGLException &e) {
		rerror("%s", e.msg());
	}
	rreturn(R_NilValue);
}

SEXP emr_check_filter_attr_expiration(SEXP _expiration, SEXP _envir)
{
	try {
		Naryn naryn(_envir);

		// check the arguments
        double expiration;
        if (isNull(_expiration))
            expiration = 0;
        else if (!isReal(_expiration) && !isInteger(_expiration) || Rf_length(_expiration) != 1)
            verror("'expiration' must be a positive integer");
        else {
            expiration = asReal(_expiration);
            if (expiration < 1 || expiration != (int)expiration)
                verror("'expiration' must be a positive integer");
            if (expiration > NRTimeStamp::MAX_HOUR)
                verror("'expiration' is out of range");
        }
	} catch (TGLException &e) {
		rerror("%s", e.msg());
	}
	rreturn(R_NilValue);
}

}
