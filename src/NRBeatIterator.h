#ifndef NRBEATITERATOR_H_INCLUDED
#define NRBEATITERATOR_H_INCLUDED

#include "naryn.h"
#include "NRTrackExpressionIterator.h"

class NRBeatIterator : public NRTrackExpressionIterator {
public:
	NRBeatIterator() {}
    NRBeatIterator(unsigned period, bool keepref, unsigned stime, unsigned etime) { init(period, keepref, stime, etime); }
	virtual ~NRBeatIterator() {}

    void init(unsigned period, bool keepref, unsigned stime, unsigned etime);

	virtual bool begin();
	virtual bool next();
    virtual bool next(const EMRPoint &jumpto);

	virtual uint64_t size() const { return m_num_steps; }
    virtual uint64_t idx() const;


protected:
    unsigned m_period;
    unsigned m_stime;
    unsigned m_etime;
    uint64_t m_num_steps;
    uint64_t m_num_steps4id;
};


//------------------------------ IMPLEMENTATION ----------------------------------------

inline void NRBeatIterator::init(unsigned period, bool keepref, unsigned stime, unsigned etime)
{
    m_keepref = keepref;
    m_period = period;
    m_stime = stime;
    m_etime = etime;
    m_num_steps4id = (uint64_t)ceil((etime - stime + 1) / (double)period);
    if (m_keepref)
        m_num_steps4id *= EMRTimeStamp::MAX_REFCOUNT + 1;
    m_num_steps = m_num_steps4id * (uint64_t)(g_db->maxid() - g_db->minid() + 1);
}

inline bool NRBeatIterator::begin()
{
	m_isend = false;
	m_point.init(g_db->minid(), m_stime, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
    while (m_point.id <= g_db->maxid()) {
        if (g_db->is_in_subset(m_point.id))
            return true;
        ++m_point.id;
    }
    m_isend = true;
    return false;
}

inline bool NRBeatIterator::next()
{
    if (m_keepref && m_point.timestamp.refcount() < EMRTimeStamp::MAX_REFCOUNT) {
        m_point.timestamp.init(m_point.timestamp.hour(), m_point.timestamp.refcount() + 1);
        return true;
    }

    EMRTimeStamp::Hour hour = m_point.timestamp.hour() + m_period;
    if (hour <= m_etime) {
        m_point.timestamp.init(hour, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
        return true;
    }

    while (++m_point.id <= g_db->maxid()) {
        if (g_db->is_in_subset(m_point.id)) {
            m_point.timestamp.init(m_stime, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
            return true;
        }
    }

    m_isend = true;
    return false;
}

inline bool NRBeatIterator::next(const EMRPoint &jumpto)
{
    unsigned id = jumpto.id;

    if (g_db->is_in_subset(id)) {
        EMRTimeStamp::Hour hour = m_period * (unsigned)ceil((jumpto.timestamp.hour() - m_stime) / (double)m_period) + m_stime;

        if (hour <= m_etime) {
            m_point.init(jumpto.id, hour, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
            return true;
        }
    }

    while (++id <= g_db->maxid()) {
        if (g_db->is_in_subset(id)) {
            m_point.init(id, m_stime, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
            return true;
        }
    }

    m_isend = true;
    return false;
}

inline uint64_t NRBeatIterator::idx() const
{
    return m_keepref ?
        m_num_steps4id * (uint64_t)(m_point.id - g_db->minid()) + (EMRTimeStamp::MAX_REFCOUNT + 1) * (uint64_t)(m_point.timestamp.hour() - m_stime) / m_period + m_point.timestamp.refcount() :
        m_num_steps4id * (uint64_t)(m_point.id - g_db->minid()) + (uint64_t)(m_point.timestamp.hour() - m_stime) / m_period;
}

#endif

