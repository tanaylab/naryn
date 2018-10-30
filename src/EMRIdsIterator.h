#ifndef EMRIDSITERATOR_H_INCLUDED
#define EMRIDSITERATOR_H_INCLUDED

#include "EMRTrackExpressionIterator.h"
#include "naryn.h"

class EMRIdsIterator : public EMRTrackExpressionIterator {
public:
	EMRIdsIterator() {}
    EMRIdsIterator(const vector<unsigned> &ids, bool keepref, unsigned stime, unsigned etime) { init(ids, keepref, stime, etime); }
	virtual ~EMRIdsIterator() {}

    void init(const vector<unsigned> &ids, bool keepref, unsigned stime, unsigned etime);

	virtual bool begin();
	virtual bool next();
    virtual bool next(const EMRPoint &jumpto);

	virtual uint64_t size() const { return m_num_steps; }
    virtual uint64_t idx() const;

protected:
	vector<unsigned>           m_ids;
    vector<unsigned>::iterator m_iid;
    unsigned                   m_stime;
    unsigned                   m_etime;
    uint64_t                   m_num_steps;
    uint64_t                   m_num_steps4id;
};


//------------------------------ IMPLEMENTATION ----------------------------------------

inline void EMRIdsIterator::init(const vector<unsigned> &ids, bool keepref, unsigned stime, unsigned etime)
{
    m_keepref = keepref;
    m_stime = stime;
    m_etime = etime;
    m_ids = ids;
    sort(m_ids.begin(), m_ids.end());

    for (vector<unsigned>::const_iterator iid = m_ids.begin() + 1; iid < m_ids.end(); ++iid) {
        if (*iid == *(iid - 1))
            verror("Ids list contains two or more identical ids");
    }

    m_num_steps4id = m_keepref ? (m_etime - m_stime + 1) * (uint64_t)(EMRTimeStamp::MAX_REFCOUNT + 1) : m_etime - m_stime + 1;
    m_num_steps = m_ids.size() * m_num_steps4id;
}

inline bool EMRIdsIterator::begin()
{
    m_isend = false;
    for (m_iid = m_ids.begin(); m_iid < m_ids.end(); ++m_iid) {
        if (g_db->is_in_subset(*m_iid)) {
            m_point.init(*m_iid, m_stime, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
            return true;
        }
    }

    m_isend = true;
    return false;
}

inline bool EMRIdsIterator::next()
{
    EMRTimeStamp::Hour hour = m_point.timestamp.hour();

    if (m_keepref && m_point.timestamp.refcount() < EMRTimeStamp::MAX_REFCOUNT) {
        m_point.timestamp.init(m_point.timestamp.hour(), m_point.timestamp.refcount() + 1);
        return true;
    }

    ++hour;
    if (hour <= m_etime) {
        m_point.timestamp.init(hour, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
        return true;
    }

    while (++m_iid < m_ids.end()) {
        if (g_db->is_in_subset(*m_iid)) {
            m_point.init(*m_iid, m_stime, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
            return true;
        }
    }

    m_isend = true;
    return false;
}

inline bool EMRIdsIterator::next(const EMRPoint &jumpto)
{
    while (m_iid < m_ids.end()) {
        if (*m_iid < jumpto.id) {
            m_iid = lower_bound(m_ids.begin(), m_ids.end(), jumpto.id);
            continue;
        }

        if (!g_db->is_in_subset(*m_iid)) {
            ++m_iid;
            continue;
        }

        if (*m_iid == jumpto.id) {
            EMRTimeStamp::Hour hour = jumpto.timestamp.hour();
            if (hour <= m_etime) {
                m_point.init(*m_iid, hour, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
                return true;
            }
            ++m_iid;
            continue;
        }

        m_point.init(*m_iid, m_stime, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
        return true;
    }

    m_isend = true;
    return false;
}

inline uint64_t EMRIdsIterator::idx() const
{
    return m_keepref ?
        (m_iid - m_ids.begin()) * m_num_steps4id + (EMRTimeStamp::MAX_REFCOUNT + 1) * (uint64_t)(m_point.timestamp.hour() - m_stime) + m_point.timestamp.refcount() :
        (m_iid - m_ids.begin()) * m_num_steps4id + m_point.timestamp.hour() - m_stime;
}

#endif


