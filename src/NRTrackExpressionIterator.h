#ifndef NRTRACKEXPRESSIONITERATOR_H_INCLUDED
#define NRTRACKEXPRESSIONITERATOR_H_INCLUDED

#include "EMRPoint.h"

class NRTrackExpressionIterator {
public:
	NRTrackExpressionIterator() : m_isend(true) {}
	virtual ~NRTrackExpressionIterator() {}

	// returns last point of the interval
	const EMRPoint &point() const { return m_point; }

    // returns false if end is reached
    virtual bool begin() = 0;

	// returns false if end is reached
	virtual bool next() = 0;

    // returns next point which time is equal or greater than jumpto (reference in jumpto is ignored)
    virtual bool next(const EMRPoint &jumpto) = 0;

    bool isend() const { return m_isend; }

    // returns the maximal number of points iterator might produce;
    // note size() returns only the potential upper bound which can be used in conjunction with idx() for progress estimation
    virtual uint64_t size() const = 0;

    // returns current running index within [0, size()] range
    virtual uint64_t idx() const = 0;

    virtual string tostr() const { return "<Unknown iterator>"; }

protected:
    bool        m_keepref;
	bool        m_isend;  // true if iterator reached the end
	EMRPoint    m_point;  // current point
};

#endif

