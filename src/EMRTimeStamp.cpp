#include "EMRTimeStamp.h"

void EMRTimeStamp::serialize(BufferedFile &bfile)
{
    if (bfile.write(&m_timestamp, sizeof(m_timestamp)) != sizeof(m_timestamp)) {
        if (bfile.error())
            TGLError("Failed to write a file %s: %s", bfile.file_name().c_str(), strerror(errno));
        TGLError("Failed to write a file %s", bfile.file_name().c_str());
    }
}

void EMRTimeStamp::unserialize(BufferedFile &bfile)
{
    if (bfile.read(&m_timestamp, sizeof(m_timestamp)) != sizeof(m_timestamp)) {
        if (bfile.error())
            TGLError("Reading a file %s: %s", bfile.file_name().c_str(), strerror(errno));
        TGLError("Invalid format of a file %s", bfile.file_name().c_str());
    }
}

