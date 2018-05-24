#ifndef NRTRACKDATA_H_INCLUDED
#define NRTRACKDATA_H_INCLUDED

#include <sys/param.h>
#ifdef _BSD
#include <sys/endian.h>
#elif defined(__linux__)
#include <byteswap.h>
#endif

#include <vector>
#include <unordered_map>

#include "NRTimeStamp.h"
#include "TGLException.h"

using namespace std;

template <class T>
class NRTrackData {
public:
    NRTrackData() {}

    void add_data(unsigned id, NRTimeStamp timestamp, T val);
    size_t size() const { return m_key2val.size(); }

public:
    struct Key {
        unsigned     id;
        NRTimeStamp  timestamp;

        Key(unsigned _id, NRTimeStamp _timestamp) : id(_id), timestamp(_timestamp) {}
        bool operator==(const Key &obj) const { return id == obj.id && timestamp == obj.timestamp; }
    };

    struct KeyHash {
        size_t operator()(const Key &key) const {
            return sizeof(size_t) == 8 ?
                key.id ^ (((size_t)key.timestamp.timestamp()) << 32) :
                key.id ^ bswap_32(key.timestamp.timestamp());
        }
    };

    struct DataRec {
        unsigned    id;
        NRTimeStamp timestamp;
        T           val;

        bool operator==(const DataRec &obj) const { return id == obj.id && timestamp == obj.timestamp && val == obj.val; }
        bool operator<(const DataRec &obj) const { return id < obj.id || id == obj.id && timestamp < obj.timestamp; }
    };


    typedef unordered_map<Key, T, KeyHash> Key2Val;
    typedef vector<DataRec> DataRecs;

    Key2Val m_key2val;
};


//------------------------------ IMPLEMENTATION ----------------------------------------

template <class T>
void NRTrackData<T>::add_data(unsigned id, NRTimeStamp timestamp, T val)
{
    pair<typename Key2Val::const_iterator, bool> res = m_key2val.insert(pair<Key, T>(Key(id, timestamp), val));
    if (!res.second)
        TGLError("Patient id %d at time %s already exists", id, timestamp.tostr().c_str());
}

#endif

