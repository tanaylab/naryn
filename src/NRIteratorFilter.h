#ifndef NRITERATORFILTER_H_INCLUDED
#define NRITERATORFILTER_H_INCLUDED

#include "EMRIteratorFilterItem.h"
#include "EMRTrack.h"

class NRIteratorFilter {
public:
    NRIteratorFilter() { m_tree = NULL; }
    ~NRIteratorFilter() { delete m_tree; }

    void init(SEXP filter, unsigned stime, unsigned etime); // filter is expected to be of LANGUAGE type

    bool is_passed(const EMRPoint &point) const { return !m_tree || m_tree->is_passed(point); }

    const EMRPoint &jumpto() const { return m_tree->m_jumpto; }

    static void check_named_filter(SEXP rfilter, const char *name);

    void debug_print();

protected:
    EMRIteratorFilterItem *m_tree;

    void build_subtree(vector<SEXP> &filters, vector<SEXP> &rfilter_names, SEXP filter, EMRIteratorFilterItem **tree,
                       bool operator_not, unsigned stime, unsigned etime, int depth);

    static int check_expiration(SEXP rexpiration, bool keepref, bool categorical,const char* name);
    static enum EMRTrack::Iterator::OPS check_op(const char *op,const char* name);

    // create filter item from a string
    static EMRIteratorFilterItem *create_filter_item(vector<SEXP> &filters, vector<SEXP> &rfilter_names, const char *str, bool operator_not, unsigned stime, unsigned etime);

    // create filter item from a named filter
    static EMRIteratorFilterItem *create_filter_item(SEXP rnamed_filter, const char *name, bool operator_not, unsigned stime, unsigned etime);

    // return depth of subtree sharing the operator
    int optimize_subtree(EMRIteratorFilterItem *tree, EMRIteratorFilterItem::Op op, vector<EMRIteratorFilterItem *> &op_nodes, vector<EMRIteratorFilterItem *> &end_nodes, int depth);

    void build_balanced_tree(EMRIteratorFilterItem *tree, EMRIteratorFilterItem::Op op,
                             vector<EMRIteratorFilterItem *>::const_iterator isnode, vector<EMRIteratorFilterItem *>::const_iterator ienode);

    void debug_print(EMRIteratorFilterItem *tree, int depth);
};

#endif

