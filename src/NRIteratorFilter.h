#ifndef NRITERATORFILTER_H_INCLUDED
#define NRITERATORFILTER_H_INCLUDED

#include "NRIteratorFilterItem.h"

class NRIteratorFilter {
public:
    NRIteratorFilter() { m_tree = NULL; }
    ~NRIteratorFilter() { delete m_tree; }

    void init(SEXP filter, unsigned stime, unsigned etime); // filter is expected to be of LANGUAGE type

    bool is_passed(const NRPoint &point) const { return !m_tree || m_tree->is_passed(point); }

    const NRPoint &jumpto() const { return m_tree->m_jumpto; }

    static void check_named_filter(SEXP rfilter, const char *name);

    void debug_print();

protected:
    NRIteratorFilterItem *m_tree;

    void build_subtree(vector<SEXP> &filters, vector<SEXP> &rfilter_names, SEXP filter, NRIteratorFilterItem **tree,
                       bool operator_not, unsigned stime, unsigned etime, int depth);

    // create filter item from a string
    static NRIteratorFilterItem *create_filter_item(vector<SEXP> &filters, vector<SEXP> &rfilter_names, const char *str, bool operator_not, unsigned stime, unsigned etime);

    // create filter item from a named filter
    static NRIteratorFilterItem *create_filter_item(SEXP rnamed_filter, const char *name, bool operator_not, unsigned stime, unsigned etime);

    // return depth of subtree sharing the operator
    int optimize_subtree(NRIteratorFilterItem *tree, NRIteratorFilterItem::Op op, vector<NRIteratorFilterItem *> &op_nodes, vector<NRIteratorFilterItem *> &end_nodes, int depth);

    void build_balanced_tree(NRIteratorFilterItem *tree, NRIteratorFilterItem::Op op,
                             vector<NRIteratorFilterItem *>::const_iterator isnode, vector<NRIteratorFilterItem *>::const_iterator ienode);

    void debug_print(NRIteratorFilterItem *tree, int depth);
};

#endif

