#ifndef NRTIMESTAMP_H_INCLUDED
#define NRTIMESTAMP_H_INCLUDED

#include <string.h>
#include <string>

#include "BufferedFile.h"
#include "TGLException.h"

using namespace std;

class __attribute__((__packed__)) NRTimeStamp {
public:
    typedef uint32_t Hour;
    typedef uint8_t  Refcount;
    typedef uint32_t TimeStamp;

    static const Hour     MAX_HOUR{(Hour)(0xffffff - 1)};
    static const Hour     NA_HOUR{(Hour)0xffffff};
    static const Refcount MAX_REFCOUNT{(Refcount)(0xff - 1)};
    static const Refcount NA_REFCOUNT{(Refcount)0xff};

    constexpr NRTimeStamp() : NRTimeStamp(NA_HOUR, NA_REFCOUNT) {}
    constexpr NRTimeStamp(unsigned int hour, unsigned char refcount) : m_timestamp((hour << 8) | refcount) {}

    bool operator==(const NRTimeStamp &obj) const { return m_timestamp == obj.m_timestamp; }
    bool operator<(const NRTimeStamp &obj) const { return m_timestamp < obj.m_timestamp; }

    void init(unsigned int hour, unsigned char refcount) { m_timestamp = (hour << 8) | refcount; }

    Hour      hour() const { return (Hour)(m_timestamp >> 8); }
    Refcount  refcount() const { return (Refcount)(m_timestamp & 0xff); }
    TimeStamp timestamp() const { return m_timestamp; }

    void pack(void *buf) const { *(TimeStamp *)buf = m_timestamp; }
    void unpack(const void *buf) { m_timestamp = *(TimeStamp *)buf; }

    static constexpr size_t packed_size() { return sizeof(TimeStamp); }

    string tostr() const;

    void serialize(BufferedFile &bfile);
    void unserialize(BufferedFile &bfile);

private:
    TimeStamp m_timestamp;
};


//------------------------------ IMPLEMENTATION ----------------------------------------

inline string NRTimeStamp::tostr() const
{
    char buf[100];
    sprintf(buf, "(hour %d, ref %d)", (int)hour(), (int)refcount());
    return buf;
}

#endif

