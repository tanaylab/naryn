#ifndef NRIDTIMEINTERVALSITERATOR_H_INCLUDED
#define NRIDTIMEINTERVALSITERATOR_H_INCLUDED

#include "NRIdTimeInterval.h"
#include "NRTrackExpressionIterator.h"

class NRIdTimeIntervalsIterator : public NRTrackExpressionIterator {
public:
	NRIdTimeIntervalsIterator() {}
    NRIdTimeIntervalsIterator(const NRIdTimeIntervals &intervs, bool keepref, unsigned stime, unsigned etime) { init(intervs, keepref, stime, etime); }
	virtual ~NRIdTimeIntervalsIterator() {}

    void init(const NRIdTimeIntervals &intervs, bool keepref, unsigned stime, unsigned etime);

    const NRIdTimeInterval &cur_interval() const { return *m_iinterv; }

	virtual bool begin();
	virtual bool next();
    virtual bool next(const EMRPoint &jumpto);

	virtual uint64_t size() const { return m_num_steps; }
    virtual uint64_t idx() const;

protected:
	NRIdTimeIntervals                 m_intervs;
    NRIdTimeIntervals::const_iterator m_iinterv;
    uint64_t                          m_num_steps;
    vector<uint64_t>                  m_num_steps4interv;   // cumulative number of steps up to the given intervals
};


//------------------------------ IMPLEMENTATION ----------------------------------------

inline void NRIdTimeIntervalsIterator::init(const NRIdTimeIntervals &intervs, bool keepref, unsigned stime, unsigned etime)
{
    m_keepref = keepref;
    m_intervs = intervs;
    m_intervs.sort_and_unify_overlaps(stime, etime);

    m_num_steps4interv.reserve(m_intervs.size() + 1);
    m_num_steps4interv.push_back(0);
    for (NRIdTimeIntervals::iterator iinterv = m_intervs.begin(); iinterv < m_intervs.end(); ++iinterv)
        m_num_steps4interv.push_back(m_num_steps4interv[iinterv - m_intervs.begin()] + iinterv->tinterv.etime - iinterv->tinterv.stime + 1);

    m_num_steps = m_num_steps4interv.back();

    if (m_keepref) {
        for (vector<uint64_t>::iterator inum_steps4interv = m_num_steps4interv.begin(); inum_steps4interv < m_num_steps4interv.end(); ++inum_steps4interv)
            *inum_steps4interv *= EMRTimeStamp::MAX_REFCOUNT + 1;
        m_num_steps *= EMRTimeStamp::MAX_REFCOUNT + 1;
    }
}

inline bool NRIdTimeIntervalsIterator::begin()
{
    m_isend = false;
    for (m_iinterv = m_intervs.begin(); m_iinterv < m_intervs.end(); ++m_iinterv) {
        if (g_db->is_in_subset(m_iinterv->id)) {
            m_point.init(m_iinterv->id, m_iinterv->tinterv.stime, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
            return true;
        }
    }

    m_isend = true;
    return false;
}

inline bool NRIdTimeIntervalsIterator::next()
{
    EMRTimeStamp::Hour hour = m_point.timestamp.hour();

    if (m_keepref && m_point.timestamp.refcount() < EMRTimeStamp::MAX_REFCOUNT) {
        m_point.timestamp.init(hour, m_point.timestamp.refcount() + 1);
        return true;
    }

    ++hour;
    if (hour <= m_iinterv->tinterv.etime) {
        m_point.timestamp.init(hour, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
        return true;
    }

    for (++m_iinterv; m_iinterv < m_intervs.end(); ++m_iinterv) {
        if (g_db->is_in_subset(m_iinterv->id)) {
            m_point.init(m_iinterv->id, {m_iinterv->tinterv.stime, m_keepref ? (EMRTimeStamp::Refcount)0 : EMRTimeStamp::NA_REFCOUNT});
            return true;
        }
    }

    m_isend = true;
    return false;
}

inline bool NRIdTimeIntervalsIterator::next(const EMRPoint &jumpto)
{
    unsigned id = jumpto.id;

    if (g_db->is_in_subset(jumpto.id)) {
        EMRTimeStamp::Hour hour = jumpto.timestamp.hour();

        if (m_iinterv->do_overlap(jumpto.id, hour)) {
            m_point.init(id, hour, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
            return true;
        }

        m_iinterv = m_intervs.lower_bound(jumpto.id, hour);
        
        if (m_iinterv == m_intervs.end()) {
            m_isend = true;
            return false;
        }

        if (m_iinterv->do_overlap(jumpto.id, hour)) {
            m_point.init(jumpto.id, hour, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
            return true;
        }
    }

    for (++m_iinterv; m_iinterv != m_intervs.end(); ++m_iinterv) {
        if (g_db->is_in_subset(m_iinterv->id)) {
            m_point.init(m_iinterv->id, m_iinterv->tinterv.stime, m_keepref ? 0 : EMRTimeStamp::NA_REFCOUNT);
            return true;
        }
    }

    m_isend = true;
    return false;
}

inline uint64_t NRIdTimeIntervalsIterator::idx() const
{
    return m_keepref ?
        m_num_steps4interv[m_iinterv - m_intervs.begin()] +
        (EMRTimeStamp::MAX_REFCOUNT + 1) * (uint64_t)(m_point.timestamp.hour() - m_iinterv->tinterv.stime) + m_point.timestamp.refcount() :
        m_num_steps4interv[m_iinterv - m_intervs.begin()] + m_point.timestamp.hour() - m_iinterv->tinterv.stime;
}

#endif

