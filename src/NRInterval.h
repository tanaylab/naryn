#ifndef NRINTERVAL_H_INCLUDED
#define NRINTERVAL_H_INCLUDED

#include <string>

using namespace std;

class NRInterval {
public:
	NRInterval() { init((unsigned)-1, -1, -1, (unsigned char)-1); }
	NRInterval(unsigned _id, int _stime, int _etime, unsigned char _refcount) { init(_id, _stime, _etime, _refcount); }

	void init(unsigned _id, int _stime, int _etime, unsigned char _refcount);

	string tostr() const;

	unsigned int  id;     // patient id
	int           stime;  // start time in hours
	int           etime;  // end time in hours
    unsigned char refcount;
};


//------------------------------ IMPLEMENTATION ----------------------------------------

inline void NRInterval::init(unsigned _id, int _stime, int _etime, unsigned char _refcount)
{
	id = _id;
	stime = _stime;
	etime = _etime;
	refcount = _refcount;
}

inline string NRInterval::tostr() const
{
	char buf[100];
	sprintf(buf, "id %d, time [%d, %d], ref %d", id, stime, etime, refcount);
	return buf;
}

#endif
