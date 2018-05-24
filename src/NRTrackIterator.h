#ifndef NRTRACKITERATOR_H_INCLUDED
#define NRTRACKITERATOR_H_INCLUDED

#include "NRTrack.h"
#include "NRTrackExpressionIterator.h"

class NRTrackIterator : public NRTrackExpressionIterator {
public:
	NRTrackIterator() {}
    NRTrackIterator(NRTrack *track, bool keepref, unsigned stime, unsigned etime, unordered_set<double> &&vals = unordered_set<double>(), NRTimeStamp::Hour expiration = 0);
	virtual ~NRTrackIterator() {}

    void init(NRTrack *track, bool keepref, unsigned stime, unsigned etime, unordered_set<double> &&vals = unordered_set<double>(), NRTimeStamp::Hour expiration = 0);

	virtual bool begin();
	virtual bool next();
    virtual bool next(const NRPoint &jumpto);

	virtual uint64_t size() const { return m_itr.size(); }
    virtual uint64_t idx() const { return m_itr.idx(); }

    virtual string tostr() const { return string("<Track iterator ") + string(m_itr.track()->name()) + string(">"); }

protected:
	NRTrack::Iterator m_itr;
};


//------------------------------ IMPLEMENTATION ----------------------------------------

inline NRTrackIterator::NRTrackIterator(NRTrack *track, bool keepref, unsigned stime, unsigned etime, unordered_set<double> &&vals, NRTimeStamp::Hour expiration)
{
    init(track, keepref, stime, etime, move(vals), expiration);
}

inline void NRTrackIterator::init(NRTrack *track, bool keepref, unsigned stime, unsigned etime, unordered_set<double> &&vals, NRTimeStamp::Hour expiration)
{
    m_keepref = keepref;
    m_itr.init(track, stime, etime, move(vals), expiration);
}

inline bool NRTrackIterator::begin()
{
	m_itr.begin();
	if (m_itr.isend()) {
        m_isend = true;
		return false;
    }
	m_isend = false;

    if (m_keepref)
        m_point = m_itr.point();
    else
        m_point.init(m_itr.point().id, m_itr.point().timestamp.hour(), -1);
    return true;
}

inline bool NRTrackIterator::next()
{
	while (m_itr.next()) {
        if (m_keepref || m_itr.point().timestamp.hour() != m_point.timestamp.hour() || m_itr.point().id != m_point.id) {
			if (m_keepref)
				m_point = m_itr.point();
			else
				m_point.init(m_itr.point().id, m_itr.point().timestamp.hour(), -1);
			return true;
		}
	}
	m_isend = true;
	return false;
}

inline bool NRTrackIterator::next(const NRPoint &jumpto)
{
    if (m_itr.next(jumpto)) {
        if (m_keepref)
            m_point = m_itr.point();
        else
            m_point.init(m_itr.point().id, m_itr.point().timestamp.hour(), -1);
        return true;
    }
    m_isend = true;
    return false;
}

#endif

