#ifndef EMRBEATITERATOR_H_INCLUDED
#define EMRBEATITERATOR_H_INCLUDED

#include <cmath>

#include "EMRTrackExpressionIterator.h"

class EMRBeatIterator : public EMRTrackExpressionIterator {
public:
	EMRBeatIterator() {}
    EMRBeatIterator(unsigned period, bool keepref, unsigned stime, unsigned etime) { init(period, keepref, stime, etime); }
	virtual ~EMRBeatIterator() {}

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
    uint64_t   m_id_idx{0};
    uint64_t m_num_steps;
    uint64_t m_num_steps4id;
};


//------------------------------ IMPLEMENTATION ----------------------------------------

inline void EMRBeatIterator::init(unsigned period, bool keepref, unsigned stime, unsigned etime)
{
    m_keepref = keepref;
    m_period = period;
    m_stime = stime;
    m_etime = etime;
    m_id_idx = 0;
    m_num_steps4id = (uint64_t)std::ceil((etime - stime + 1) / (double)period);
    if (m_keepref)
        m_num_steps4id *= EMRTimeStamp::MAX_REFCOUNT + 1;
    m_num_steps = m_num_steps4id * g_db->num_ids();
}

inline bool EMRBeatIterator::begin()
{
    uint64_t num_ids = g_db->num_ids();
    for (m_id_idx = 0; m_id_idx < num_ids; ++m_id_idx) {
        m_point.id = g_db->id(m_id_idx);
        if (g_db->is_in_subset(m_point.id)) {
            m_isend = false;
            m_point.timestamp.init(m_stime, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
            return true;
        }
    }

    m_isend = true;
    return false;
}

inline bool EMRBeatIterator::next()
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

    while (++m_id_idx < g_db->num_ids()) {
        m_point.id = g_db->id(m_id_idx);
        if (g_db->is_in_subset(m_point.id)) {
            m_point.timestamp.init(m_stime, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
            return true;
        }
    }

    m_isend = true;
    return false;
}

inline bool EMRBeatIterator::next(const EMRPoint &jumpto)
{
    m_id_idx = g_db->id2idx(jumpto.id);

    if (g_db->is_in_subset(jumpto.id)) {
        EMRTimeStamp::Hour hour = m_period * (unsigned)std::ceil((jumpto.timestamp.hour() - m_stime) / (double)m_period) + m_stime;

        if (hour <= m_etime) {
            m_point.init(jumpto.id, hour, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
            return true;
        }
    }

    while (++m_id_idx < g_db->num_ids()) {
        m_point.id = g_db->id(m_id_idx);
        if (g_db->is_in_subset(m_point.id)) {
            m_point.timestamp.init(m_stime, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
            return true;
        }
    }

    m_isend = true;
    return false;
}

inline uint64_t EMRBeatIterator::idx() const
{
    return m_keepref ?
        m_num_steps4id * m_id_idx + (EMRTimeStamp::MAX_REFCOUNT + 1) * (uint64_t)(m_point.timestamp.hour() - m_stime) / m_period + m_point.timestamp.refcount() :
        m_num_steps4id * m_id_idx + (uint64_t)(m_point.timestamp.hour() - m_stime) / m_period;
}

#endif

