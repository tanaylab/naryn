#ifndef NRBEATEXTITERATOR_H_INCLUDED
#define NRBEATEXTITERATOR_H_INCLUDED

#include "naryn.h"
#include "NRDb.h"
#include "NRTrackExpressionIterator.h"

class NRBeatExtIterator : public NRTrackExpressionIterator {
public:
	NRBeatExtIterator() : m_itr(NULL) {}
    NRBeatExtIterator(unsigned period, NRTrackExpressionIterator *itr, bool keepref, unsigned stime, unsigned etime);
	virtual ~NRBeatExtIterator() { delete m_itr; }

    void init(unsigned period, NRTrackExpressionIterator *itr, bool keepref, unsigned stime, unsigned etime);

	virtual bool begin();
	virtual bool next();
    virtual bool next(const NRPoint &jumpto);

	virtual uint64_t size() const { return m_num_steps; }
    virtual uint64_t idx() const;

protected:
    unsigned                   m_period;
    NRTrackExpressionIterator *m_itr;
    unsigned                   m_stime;
    unsigned                   m_etime;
    uint64_t                   m_num_steps;
    uint64_t                   m_num_steps4id;
};


//------------------------------ IMPLEMENTATION ----------------------------------------

inline NRBeatExtIterator::NRBeatExtIterator(unsigned period, NRTrackExpressionIterator *itr, bool keepref, unsigned stime, unsigned etime) :
    m_itr(NULL)
{
    init(period, itr, keepref, stime, etime);
}

inline void NRBeatExtIterator::init(unsigned period, NRTrackExpressionIterator *itr, bool keepref, unsigned stime, unsigned etime)
{
    delete m_itr;
    m_itr = itr;
    m_keepref = keepref;
    m_period = period;
    m_stime = stime;
    m_etime = etime;
    m_num_steps4id = (uint64_t)ceil((etime - stime + 1) / (double)period);
    if (m_keepref)
        m_num_steps4id *= NRTimeStamp::MAX_REFCOUNT + 1;
    m_num_steps = m_num_steps4id * (uint64_t)(g_db->maxid() - g_db->minid() + 1);
}

inline bool NRBeatExtIterator::begin()
{
    if (m_itr->begin()) {
        m_isend = false;

        unsigned itr_id = m_itr->point().id;
        NRTimeStamp::Hour itr_hour = m_itr->point().timestamp.hour();

        if (itr_hour >= m_stime) {
            m_point.init(itr_id, itr_hour, m_keepref ? 0 : NRTimeStamp::NA_REFCOUNT);
            return true;
        } else {
            while (1) {
                NRTimeStamp::Hour hour = m_period * (unsigned)ceil((m_stime - itr_hour) / (double)m_period) + itr_hour;

                if (hour <= m_etime) {
                    m_point.init(itr_id, hour, m_keepref ? 0 : NRTimeStamp::NA_REFCOUNT);
                    return true;
                }

                if (!m_itr->next())
                    break;

                if (m_itr->point().id == itr_id)
                    verror("Id %d appears multiple times in the initiation table of the beat iterator", itr_id);

                itr_id = m_itr->point().id;
                itr_hour = m_itr->point().timestamp.hour();
            }
        }
    }

    m_isend = true;
    return false;
}

inline bool NRBeatExtIterator::next()
{
    if (m_keepref && m_point.timestamp.refcount() < NRTimeStamp::MAX_REFCOUNT) {
        m_point.init(m_point.id, m_point.timestamp.hour(), m_point.timestamp.refcount() + 1);
        return true;
    }

    NRTimeStamp::Hour hour = m_point.timestamp.hour() + m_period;
    if (hour <= m_etime) {
        m_point.timestamp.init(hour, m_keepref ? 0 : NRTimeStamp::NA_REFCOUNT);
        return true;
    }

    unsigned id = m_point.id;
    while (m_itr->next()) {
        unsigned itr_id = m_itr->point().id;
        NRTimeStamp::Hour itr_hour = m_itr->point().timestamp.hour();

        if (id == itr_id)
            verror("Id %d appears multiple times in the initiation table of the beat iterator", itr_id);

        id = itr_id;

        if (itr_hour >= m_stime) {
            m_point.init(itr_id, itr_hour, m_keepref ? 0 : NRTimeStamp::NA_REFCOUNT);
            return true;
        } else {
            hour = m_period * (unsigned)ceil((m_stime - itr_hour) / (double)m_period) + itr_hour;
            if (hour <= m_etime) {
                m_point.init(id, hour, m_keepref ? 0 : NRTimeStamp::NA_REFCOUNT);
                return true;
            }
        }
    }

    m_isend = true;
    return false;
}

inline bool NRBeatExtIterator::next(const NRPoint &jumpto)
{
    if (m_point.id == jumpto.id) {
        NRTimeStamp::Hour prev_hour = m_point.timestamp.hour();
        NRTimeStamp::Hour hour = m_period * (unsigned)ceil((jumpto.timestamp.hour() - prev_hour) / (double)m_period) + prev_hour;

        if (hour <= m_etime) {
            m_point.init(jumpto.id, hour, m_keepref ? 0 : NRTimeStamp::NA_REFCOUNT);
            return true;
        }
    }

    unsigned id = m_point.id;
    if (m_itr->next(NRPoint(jumpto.id, g_db->mintime(), (NRTimeStamp::Refcount)-1))) {
        do {
            unsigned itr_id = m_itr->point().id;
            NRTimeStamp::Hour itr_hour = m_itr->point().timestamp.hour();

            if (id == itr_id)
                verror("Id %d appears multiple times in the initiation table of the beat iterator", itr_id);

            id = itr_id;

            if (itr_hour >= m_stime) {
                m_point.init(itr_id, itr_hour, m_keepref ? 0 : NRTimeStamp::NA_REFCOUNT);
                return true;
            } else {
                NRTimeStamp::Hour hour = m_period * (unsigned)ceil((m_stime - itr_hour) / (double)m_period) + itr_hour;
                if (hour <= m_etime) {
                    m_point.init(id, hour, m_keepref ? 0 : NRTimeStamp::NA_REFCOUNT);
                    return true;
                }
            }
        } while (m_itr->next());
    }

    m_isend = true;
    return false;
}

inline uint64_t NRBeatExtIterator::idx() const
{
    return m_keepref ?
        m_num_steps4id * (uint64_t)(m_point.id - g_db->minid()) + (NRTimeStamp::MAX_REFCOUNT + 1) * (uint64_t)(m_point.timestamp.hour() - m_stime) / m_period + m_point.timestamp.refcount() :
        m_num_steps4id * (uint64_t)(m_point.id - g_db->minid()) + (uint64_t)(m_point.timestamp.hour() - m_stime) / m_period;
}

#endif

