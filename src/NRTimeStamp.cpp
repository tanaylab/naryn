#include "NRTimeStamp.h"

//const NRTimeStamp::Hour     NRTimeStamp::MAX_HOUR = (Hour)(0xffffff - 1);
//const NRTimeStamp::Hour     NRTimeStamp::NA_HOUR = (Hour)0xffffff;
//const NRTimeStamp::Refcount NRTimeStamp::MAX_REFCOUNT = (Refcount)(0xff - 1);
//const NRTimeStamp::Refcount NRTimeStamp::NA_REFCOUNT = (Refcount)0xff;

void NRTimeStamp::serialize(BufferedFile &bfile)
{
    if (bfile.write(&m_timestamp, sizeof(m_timestamp)) != sizeof(m_timestamp)) {
        if (bfile.error())
            TGLError("Failed to write a file %s: %s", bfile.file_name().c_str(), strerror(errno));
        TGLError("Failed to write a file %s", bfile.file_name().c_str());
    }
}

void NRTimeStamp::unserialize(BufferedFile &bfile)
{
    if (bfile.read(&m_timestamp, sizeof(m_timestamp)) != sizeof(m_timestamp)) {
        if (bfile.error())
            TGLError("Reading a file %s: %s", bfile.file_name().c_str(), strerror(errno));
        TGLError("Invalid format of a file %s", bfile.file_name().c_str());
    }
}

