#ifndef NRPOINTSITERATOR_H_INCLUDED
#define NRPOINTSITERATOR_H_INCLUDED

#include "NRTrack.h"
#include "NRTrackExpressionIterator.h"

class NRPointsIterator : public NRTrackExpressionIterator {
public:
	NRPointsIterator() {}
    NRPointsIterator(const NRPoints &points, bool keepref, unsigned stime, unsigned etime) { init(points, keepref, stime, etime); }
	virtual ~NRPointsIterator() {}

    void init(const NRPoints &points, bool keepref, unsigned stime, unsigned etime);

	virtual bool begin();
	virtual bool next();
    virtual bool next(const NRPoint &jumpto);

	virtual uint64_t size() const { return m_points.size(); }
    virtual uint64_t idx() const { return m_ipoint - m_points.begin(); }

protected:
	NRPoints           m_points;
    NRPoints::iterator m_ipoint;
    unsigned           m_stime;
    unsigned           m_etime;
};


//------------------------------ IMPLEMENTATION ----------------------------------------

inline void NRPointsIterator::init(const NRPoints &points, bool keepref, unsigned stime, unsigned etime)
{
    m_keepref = keepref;
    m_stime = stime;
    m_etime = etime;
    m_points = points;
    sort(m_points.begin(), m_points.end());

    for (NRPoints::const_iterator ipoint = m_points.begin() + 1; ipoint < m_points.end(); ++ipoint) {
        if (*ipoint == *(ipoint - 1))
            verror("Id-time list contains two or more identical points");
        if (!keepref && ipoint->id == (ipoint - 1)->id && ipoint->timestamp.hour() == (ipoint - 1)->timestamp.hour())
            verror("Id-time list contains two or more points that differ only by reference");
    }
}

inline bool NRPointsIterator::begin()
{
    m_ipoint = m_points.begin() - 1;
    m_point.init(-1, -1, -1);
    m_isend = false;
    return next();
}

inline bool NRPointsIterator::next()
{
    ++m_ipoint;
	while (m_ipoint < m_points.end()) {
        if (g_db->is_in_subset(m_ipoint->id)) {
            NRTimeStamp::Hour hour = m_ipoint->timestamp.hour();
            if (hour >= m_stime && hour <= m_etime) {
                if (m_keepref) {
                    m_point = *m_ipoint;
                    return true;
                }

                if (m_point.id != m_ipoint->id || m_point.timestamp.hour() != hour) {
                    m_point.init(m_ipoint->id, hour, NRTimeStamp::NA_REFCOUNT);
                    return true;
                }
            }
        }
        ++m_ipoint;
	}
	m_isend = true;
	return false;
}

inline bool NRPointsIterator::next(const NRPoint &jumpto)
{
    ++m_ipoint;
    if (m_ipoint < m_points.end()) {
        if (m_keepref && jumpto.timestamp.refcount() != NRTimeStamp::NA_REFCOUNT) {
            if (*m_ipoint < jumpto)
                m_ipoint = lower_bound(m_ipoint, m_points.end(), jumpto);
        } else {
            NRPoint _jumpto(jumpto.id, jumpto.timestamp.hour(), 0);
            if (*m_ipoint < _jumpto)
                m_ipoint = lower_bound(m_ipoint, m_points.end(), _jumpto);
        }

        while (m_ipoint < m_points.end()) {
            if (g_db->is_in_subset(m_ipoint->id)) {
                NRTimeStamp::Hour hour = m_ipoint->timestamp.hour();
                if (hour >= m_stime && hour <= m_etime) {
                    if (m_keepref) {
                        m_point = *m_ipoint;
                        return true;
                    }

                    if (m_point.id != m_ipoint->id || m_point.timestamp.hour() != hour) {
                        m_point.init(m_ipoint->id, hour, NRTimeStamp::NA_REFCOUNT);
                        return true;
                    }
                }
            }
            ++m_ipoint;
        }
    }
    m_isend = true;
    return false;
}

#endif

