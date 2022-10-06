#ifndef EMRTIMESITERATOR_H_INCLUDED
#define EMRTIMESITERATOR_H_INCLUDED

#include "EMRTimeInterval.h"
#include "EMRTrackExpressionIterator.h"

class EMRTimesIterator : public EMRTrackExpressionIterator {
public:
	EMRTimesIterator() {}
    EMRTimesIterator(const EMRTimeIntervals &intervs, bool keepref, unsigned stime, unsigned etime) { init(intervs, keepref, stime, etime); }
	virtual ~EMRTimesIterator() {}

    void init(const EMRTimeIntervals &intervs, bool keepref, unsigned stime, unsigned etime);

    const EMRTimeInterval &cur_interval() const { return *m_iinterv; }

	virtual bool begin();
	virtual bool next();
    virtual bool next(const EMRPoint &jumpto);

	virtual uint64_t size() const { return m_num_steps; }
    virtual uint64_t idx() const;

protected:
	EMRTimeIntervals                 m_intervs;
    EMRTimeIntervals::const_iterator m_iinterv;
    uint64_t                           m_id_idx{0};
    uint64_t                         m_num_steps;
    vector<uint64_t>                 m_num_steps4id;
};


//------------------------------ IMPLEMENTATION ----------------------------------------

inline void EMRTimesIterator::init(const EMRTimeIntervals &intervs, bool keepref, unsigned stime, unsigned etime)
{
    m_keepref = keepref;
    m_intervs = intervs;
    m_intervs.sort_and_unify_overlaps(stime, etime);
    m_id_idx = 0;

    m_num_steps4id.reserve(m_intervs.size() + 1);
    m_num_steps4id.push_back(0);
    for (EMRTimeIntervals::iterator iinterv = m_intervs.begin(); iinterv < m_intervs.end(); ++iinterv)
        m_num_steps4id.push_back(m_num_steps4id[iinterv - m_intervs.begin()] + iinterv->etime - iinterv->stime + 1);

    m_num_steps = m_num_steps4id.back() * g_db->num_ids();

    if (m_keepref) {
        for (vector<uint64_t>::iterator inum_steps4id = m_num_steps4id.begin(); inum_steps4id < m_num_steps4id.end(); ++inum_steps4id)
            *inum_steps4id *= EMRTimeStamp::MAX_REFCOUNT + 1;
        m_num_steps *= EMRTimeStamp::MAX_REFCOUNT + 1;
    }
}

inline bool EMRTimesIterator::begin()
{
    m_iinterv = m_intervs.begin();
    if (m_iinterv < m_intervs.end()) {
        uint64_t num_ids = g_db->num_ids();
        for (m_id_idx = 0; m_id_idx < num_ids; ++m_id_idx) {
            m_point.id = g_db->id(m_id_idx);
            if (g_db->is_in_subset(m_point.id)) {
                m_isend = false;
                m_point.timestamp.init(m_iinterv->stime, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
                return true;
            }
        }
    }

    m_isend = true;
    return false;
}

inline bool EMRTimesIterator::next()
{
    EMRTimeStamp::Hour hour = m_point.timestamp.hour();

    if (m_keepref && m_point.timestamp.refcount() < EMRTimeStamp::MAX_REFCOUNT) {
        m_point.timestamp.init(hour, m_point.timestamp.refcount() + 1);
        return true;
    }

    ++hour;
    if (hour <= m_iinterv->etime) {
        m_point.timestamp.init(hour, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
        return true;
    }

    ++m_iinterv;
    if (m_iinterv < m_intervs.end()) {
        m_point.timestamp.init(m_iinterv->stime, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
        return true;
    }

    while (++m_id_idx < g_db->num_ids()) {
        m_point.id = g_db->id(m_id_idx);
        if (g_db->is_in_subset(m_point.id)) {
            m_iinterv = m_intervs.begin();
            m_point.timestamp.init(m_iinterv->stime, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
            return true;
        }
    }

    m_isend = true;
    return false;
}

inline bool EMRTimesIterator::next(const EMRPoint &jumpto)
{
    m_id_idx = g_db->id2idx(jumpto.id);

    if (g_db->is_in_subset(jumpto.id)) {
        EMRTimeStamp::Hour hour = jumpto.timestamp.hour();

        if (m_iinterv->do_overlap(hour)) {
            m_point.init(jumpto.id, hour, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
            return true;
        }

        m_iinterv = m_intervs.lower_bound(hour);
        
        if (m_iinterv == m_intervs.end()) {
            m_iinterv = m_intervs.begin();
            m_point.init(jumpto.id, m_iinterv->stime, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
            return true;
        }

        if (m_iinterv->do_overlap(hour)) {
            m_point.init(jumpto.id, hour, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
            return true;
        }

        ++m_iinterv;
        if (m_iinterv < m_intervs.end()) {
            m_point.init(jumpto.id, m_iinterv->stime, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
            return true;
        }
    }

    while (++m_id_idx < g_db->num_ids()) {
        m_point.id = g_db->id(m_id_idx);
        if (g_db->is_in_subset(m_point.id)) {
            m_iinterv = m_intervs.begin();
            m_point.timestamp.init(m_iinterv->stime, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
        }
    }

    m_isend = true;
    return false;
}

inline uint64_t EMRTimesIterator::idx() const
{
    return m_keepref ?
        m_id_idx * m_num_steps4id.back() +
        (EMRTimeStamp::MAX_REFCOUNT + 1) * (uint64_t)(m_num_steps4id[m_iinterv - m_intervs.begin()] + m_point.timestamp.hour() - m_iinterv->stime) + m_point.timestamp.refcount() :
        m_id_idx * m_num_steps4id.back() + (uint64_t)(m_num_steps4id[m_iinterv - m_intervs.begin()] + m_point.timestamp.hour() - m_iinterv->stime);
}

#endif

