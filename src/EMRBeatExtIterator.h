#ifndef EMRBEATEXTITERATOR_H_INCLUDED
#define EMRBEATEXTITERATOR_H_INCLUDED

#include "EMRTrackExpressionIterator.h"
#include "naryn.h"

class EMRBeatExtIterator : public EMRTrackExpressionIterator {
public:
	EMRBeatExtIterator() : m_itr(NULL) {}
    EMRBeatExtIterator(unsigned period, EMRTrackExpressionIterator *itr, bool keepref, unsigned stime, unsigned etime);
	virtual ~EMRBeatExtIterator() { delete m_itr; }

    void init(unsigned period, EMRTrackExpressionIterator *itr, bool keepref, unsigned stime, unsigned etime);

	virtual bool begin();
	virtual bool next();
    virtual bool next(const EMRPoint &jumpto);

	virtual uint64_t size() const { return m_num_steps; }
    virtual uint64_t idx() const;

protected:
    unsigned                    m_period;
    EMRTrackExpressionIterator *m_itr;
    unsigned                    m_stime;
    unsigned                    m_etime;
    uint64_t                    m_num_steps;
    uint64_t                    m_num_steps4id;
};


//------------------------------ IMPLEMENTATION ----------------------------------------

inline EMRBeatExtIterator::EMRBeatExtIterator(unsigned period, EMRTrackExpressionIterator *itr, bool keepref, unsigned stime, unsigned etime) :
    m_itr(NULL)
{
    init(period, itr, keepref, stime, etime);
}

inline void EMRBeatExtIterator::init(unsigned period, EMRTrackExpressionIterator *itr, bool keepref, unsigned stime, unsigned etime)
{
    delete m_itr;
    m_itr = itr;
    m_keepref = keepref;
    m_period = period;
    m_stime = stime;
    m_etime = etime;
    m_num_steps4id = (uint64_t)ceil((etime - stime + 1) / (double)period);
    if (m_keepref)
        m_num_steps4id *= EMRTimeStamp::MAX_REFCOUNT + 1;
    m_num_steps = m_num_steps4id * (uint64_t)(g_db->maxid() - g_db->minid() + 1);
}

inline bool EMRBeatExtIterator::begin()
{
    if (m_itr->begin()) {
        m_isend = false;

        unsigned itr_id = m_itr->point().id;
        EMRTimeStamp::Hour itr_hour = m_itr->point().timestamp.hour();

        if (itr_hour >= m_stime) {
            m_point.init(itr_id, itr_hour, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
            return true;
        }

        while (1) {
            EMRTimeStamp::Hour hour = m_period * (unsigned)ceil((m_stime - itr_hour) / (double)m_period) + itr_hour;

            if (hour <= m_etime) {
                m_point.init(itr_id, hour, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
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

    m_isend = true;
    return false;
}

inline bool EMRBeatExtIterator::next()
{
    if (m_keepref && m_point.timestamp.refcount() < EMRTimeStamp::MAX_REFCOUNT) {
        m_point.init(m_point.id, m_point.timestamp.hour(), m_point.timestamp.refcount() + 1);
        return true;
    }

    EMRTimeStamp::Hour hour = m_point.timestamp.hour() + m_period;
    if (hour <= m_etime) {
        m_point.timestamp.init(hour, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
        return true;
    }

    unsigned id = m_point.id;
    while (m_itr->next()) {
        unsigned itr_id = m_itr->point().id;
        EMRTimeStamp::Hour itr_hour = m_itr->point().timestamp.hour();

        if (id == itr_id)
            verror("Id %d appears multiple times in the initiation table of the beat iterator", itr_id);

        id = itr_id;

        if (itr_hour >= m_stime) {
            m_point.init(itr_id, itr_hour, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
            return true;
        } else {
            hour = m_period * (unsigned)ceil((m_stime - itr_hour) / (double)m_period) + itr_hour;
            if (hour <= m_etime) {
                m_point.init(id, hour, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
                return true;
            }
        }
    }

    m_isend = true;
    return false;
}

inline bool EMRBeatExtIterator::next(const EMRPoint &jumpto)
{
    if (m_point.id == jumpto.id) {
        EMRTimeStamp::Hour prev_hour = m_point.timestamp.hour();
        EMRTimeStamp::Hour hour = m_period * (unsigned)ceil((jumpto.timestamp.hour() - prev_hour) / (double)m_period) + prev_hour;

        if (hour <= m_etime) {
            m_point.init(jumpto.id, hour, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
            return true;
        }
    }

    unsigned id = m_point.id;
    if (m_itr->next(EMRPoint(jumpto.id, 0, (EMRTimeStamp::Refcount)-1))) {
        do {
            unsigned itr_id = m_itr->point().id;
            EMRTimeStamp::Hour itr_hour = m_itr->point().timestamp.hour();

            if (id == itr_id)
                verror("Id %d appears multiple times in the initiation table of the beat iterator", itr_id);

            id = itr_id;

            if (itr_hour >= m_stime) {
                m_point.init(itr_id, itr_hour, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
                return true;
            } else {
                EMRTimeStamp::Hour hour = m_period * (unsigned)ceil((m_stime - itr_hour) / (double)m_period) + itr_hour;
                if (hour <= m_etime) {
                    m_point.init(id, hour, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
                    return true;
                }
            }
        } while (m_itr->next());
    }

    m_isend = true;
    return false;
}

inline uint64_t EMRBeatExtIterator::idx() const
{
    return m_keepref ?
        m_num_steps4id * (uint64_t)(m_point.id - g_db->minid()) + (EMRTimeStamp::MAX_REFCOUNT + 1) * (uint64_t)(m_point.timestamp.hour() - m_stime) / m_period + m_point.timestamp.refcount() :
        m_num_steps4id * (uint64_t)(m_point.id - g_db->minid()) + (uint64_t)(m_point.timestamp.hour() - m_stime) / m_period;
}

#endif

