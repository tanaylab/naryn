#ifndef NRITERATORFILTERITEM_H_INCLUDED
#define NRITERATORFILTERITEM_H_INCLUDED

#include <stdio.h>

#include "EMRPoint.h"
#include "NRTrackExpressionIterator.h"

class NRIteratorFilterItem {
public:
    NRIteratorFilterItem();
    virtual ~NRIteratorFilterItem();

    bool is_leaf() const { return !m_child[0]; }

    // returns true if the point passes the filter; nearest_point is set if the point is filtered out
    bool is_passed(const EMRPoint &point) { return is_leaf() ? is_passed_leaf(point) : is_passed_node(point); }

    const EMRPoint &jumpto() const { return m_jumpto; }

protected:
    friend class NRIteratorFilter;

    enum Op { NONE, OR, AND, NUM_OPS };

    static const char *OP_NAMES[NUM_OPS];

    // used when node
    Op       m_op;
    NRIteratorFilterItem *m_child[2];

    // used when leaf
    bool     m_is_not;
    int      m_sshift;
    int      m_eshift;
    int      m_stime;
    int      m_etime;
    bool     m_keepref;
    bool     m_itr_started;
    NRTrackExpressionIterator *m_itr;

    EMRPoint  m_jumpto;
    EMRPoint  m_true_upto;

    bool is_passed_node(const EMRPoint &point);
    bool is_passed_leaf(const EMRPoint &point);
    bool filtered_next();
    bool filtered_end();

    virtual void debug_print(int depth);
};


//------------------------------ IMPLEMENTATION ----------------------------------------

inline NRIteratorFilterItem::NRIteratorFilterItem()
{
    m_op = NONE;
    m_child[0] = m_child[1] = NULL;
    m_is_not = false;
    m_sshift = m_eshift = 0;
    m_keepref = false;
    m_itr_started = false;
    m_itr = NULL;
}

inline NRIteratorFilterItem::~NRIteratorFilterItem()
{
    delete m_child[0];
    delete m_child[1];
    delete m_itr;
}

inline bool NRIteratorFilterItem::filtered_next()
{
    int hour = max((int)m_itr->point().timestamp.hour() - m_eshift, m_stime);
    m_jumpto.init(m_itr->point().id, hour, (EMRTimeStamp::Refcount)-1);
    return false;
}

inline bool NRIteratorFilterItem::filtered_end()
{
    m_jumpto = EMRPoint();
    return false;
}

#endif
