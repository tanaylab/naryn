#ifndef NRTIMESITERATOR_H_INCLUDED
#define NRTIMESITERATOR_H_INCLUDED

#include "NRTimeInterval.h"
#include "NRTrackExpressionIterator.h"

class NRTimesIterator : public NRTrackExpressionIterator {
public:
	NRTimesIterator() {}
    NRTimesIterator(const NRTimeIntervals &intervs, bool keepref, unsigned stime, unsigned etime) { init(intervs, keepref, stime, etime); }
	virtual ~NRTimesIterator() {}

    void init(const NRTimeIntervals &intervs, bool keepref, unsigned stime, unsigned etime);

    const NRTimeInterval &cur_interval() const { return *m_iinterv; }

	virtual bool begin();
	virtual bool next();
    virtual bool next(const NRPoint &jumpto);

	virtual uint64_t size() const { return m_num_steps; }
    virtual uint64_t idx() const;

protected:
	NRTimeIntervals                 m_intervs;
    NRTimeIntervals::const_iterator m_iinterv;
    vector<NRTimeStamp::Hour>       m_interv_stimes;
    unsigned                        m_stime;
    unsigned                        m_etime;
    uint64_t                        m_num_steps;
    vector<uint64_t>                m_num_steps4id;
};


//------------------------------ IMPLEMENTATION ----------------------------------------

inline void NRTimesIterator::init(const NRTimeIntervals &intervs, bool keepref, unsigned stime, unsigned etime)
{
    m_keepref = keepref;
    m_stime = stime;
    m_etime = etime;
    m_intervs = intervs;
    m_intervs.sort_and_unify_overlaps(stime, etime);

    m_num_steps4id.reserve(m_intervs.size() + 1);
    m_num_steps4id.push_back(0);
    for (NRTimeIntervals::iterator iinterv = m_intervs.begin(); iinterv < m_intervs.end(); ++iinterv)
        m_num_steps4id.push_back(m_num_steps4id[iinterv - m_intervs.begin()] + iinterv->etime - iinterv->stime + 1);

    m_num_steps = m_num_steps4id.back() * (g_db->maxid() - g_db->minid() + 1);

    if (m_keepref) {
        for (vector<uint64_t>::iterator inum_steps4id = m_num_steps4id.begin(); inum_steps4id < m_num_steps4id.end(); ++inum_steps4id)
            *inum_steps4id *= NRTimeStamp::MAX_REFCOUNT + 1;
        m_num_steps *= NRTimeStamp::MAX_REFCOUNT + 1;
    }
}

inline bool NRTimesIterator::begin()
{
    m_isend = false;
    m_iinterv = m_intervs.begin();
    if (m_iinterv < m_intervs.end()) {
        for (unsigned id = g_db->minid(); id <= g_db->maxid(); ++id) {
            if (g_db->is_in_subset(id)) {
                m_point.init(id, m_iinterv->stime, m_keepref ? 0 : NRTimeStamp::NA_REFCOUNT);
                return true;
            }
        }
    }

    m_isend = true;
    return false;
}

inline bool NRTimesIterator::next()
{
    NRTimeStamp::Hour hour = m_point.timestamp.hour();

    if (m_keepref && m_point.timestamp.refcount() < NRTimeStamp::MAX_REFCOUNT) {
        m_point.timestamp.init(hour, m_point.timestamp.refcount() + 1);
        return true;
    }

    ++hour;
    if (hour <= m_iinterv->etime) {
        m_point.timestamp.init(hour, m_keepref ? 0 : NRTimeStamp::NA_REFCOUNT);
        return true;
    }

    ++m_iinterv;
    if (m_iinterv < m_intervs.end()) {
        m_point.timestamp.init(m_iinterv->stime, m_keepref ? 0 : NRTimeStamp::NA_REFCOUNT);
        return true;
    }

    while (++m_point.id <= g_db->maxid()) {
        if (g_db->is_in_subset(m_point.id)) {
            m_iinterv = m_intervs.begin();
            m_point.timestamp.init(m_iinterv->stime, m_keepref ? 0 : NRTimeStamp::NA_REFCOUNT);
            return true;
        }
    }

    m_isend = true;
    return false;
}

inline bool NRTimesIterator::next(const NRPoint &jumpto)
{
    if (g_db->is_in_subset(jumpto.id)) {
        NRTimeStamp::Hour hour = jumpto.timestamp.hour();

        if (m_iinterv->do_overlap(hour)) {
            m_point.init(jumpto.id, hour, m_keepref ? 0 : NRTimeStamp::NA_REFCOUNT);
            return true;
        }

        m_iinterv = m_intervs.lower_bound(hour);
        
        if (m_iinterv == m_intervs.end()) {
            m_iinterv = m_intervs.begin();
            m_point.init(jumpto.id, m_iinterv->stime, m_keepref ? 0 : NRTimeStamp::NA_REFCOUNT);
            return true;
        }

        if (m_iinterv->do_overlap(hour)) {
            m_point.init(jumpto.id, hour, m_keepref ? 0 : NRTimeStamp::NA_REFCOUNT);
            return true;
        }

        ++m_iinterv;
        if (m_iinterv < m_intervs.end()) {
            m_point.init(jumpto.id, m_iinterv->stime, m_keepref ? 0 : NRTimeStamp::NA_REFCOUNT);
            return true;
        }

        if (jumpto.id + 1 < g_db->maxid()) {
            m_iinterv = m_intervs.begin();
            m_point.init(jumpto.id + 1, m_iinterv->stime, m_keepref ? 0 : NRTimeStamp::NA_REFCOUNT);
            return true;
        }
    } else {
        for (unsigned id = jumpto.id + 1; id <= g_db->maxid(); ++id) {
            if (g_db->is_in_subset(id)) {
                m_iinterv = m_intervs.begin();
                m_point.init(id, m_iinterv->stime, m_keepref ? 0 : NRTimeStamp::NA_REFCOUNT);
            }
        }
    }

    m_isend = true;
    return false;
}

inline uint64_t NRTimesIterator::idx() const
{
    return m_keepref ?
        (m_point.id - g_db->minid()) * m_num_steps4id.back() +
        (NRTimeStamp::MAX_REFCOUNT + 1) * (uint64_t)(m_num_steps4id[m_iinterv - m_intervs.begin()] + m_point.timestamp.hour() - m_iinterv->stime) + m_point.timestamp.refcount() :
        (m_point.id - g_db->minid()) * m_num_steps4id.back() + (uint64_t)(m_num_steps4id[m_iinterv - m_intervs.begin()] + m_point.timestamp.hour() - m_iinterv->stime);
}

#endif

