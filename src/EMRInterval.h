#ifndef EMRINTERVAL_H_INCLUDED
#define EMRINTERVAL_H_INCLUDED

#include <string>

using namespace std;

class EMRInterval {
public:
	EMRInterval() { init((unsigned)-1, -1, -1, (unsigned char)-1); }
	EMRInterval(unsigned _id, int _stime, int _etime, unsigned char _refcount) { init(_id, _stime, _etime, _refcount); }

	void init(unsigned _id, int _stime, int _etime, unsigned char _refcount);

	string tostr() const;

	unsigned int  id;     // patient id
	int           stime;  // start time in hours
	int           etime;  // end time in hours
    unsigned char refcount;
};


//------------------------------ IMPLEMENTATION ----------------------------------------

inline void EMRInterval::init(unsigned _id, int _stime, int _etime, unsigned char _refcount)
{
	id = _id;
	stime = _stime;
	etime = _etime;
	refcount = _refcount;
}

inline string EMRInterval::tostr() const
{
	char buf[100];
	snprintf(buf, sizeof(buf), "id %d, time [%d, %d], ref %d", id, stime, etime, refcount);
	return buf;
}

#endif
