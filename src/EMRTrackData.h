#ifndef EMRTRACKDATA_H_INCLUDED
#define EMRTRACKDATA_H_INCLUDED

#include <sys/param.h>
#ifdef _BSD
#include <sys/endian.h>
#elif defined(__linux__)
#include <byteswap.h>
#endif

#include <unordered_map>
#include <vector>

#include "EMRTimeStamp.h"
#include "TGLException.h"

using namespace std;

template <class T>
struct EMRTrackData {
    struct DataRec {
        unsigned id;
        EMRTimeStamp timestamp;
        T val;

        DataRec() {}
        DataRec(unsigned _id, const EMRTimeStamp &_timestamp, T _val);
        DataRec(const DataRec &rec)
            : id(rec.id), timestamp(rec.timestamp), val(rec.val) {}
        bool operator==(const DataRec &obj) const {
            return id == obj.id && timestamp == obj.timestamp && val == obj.val;
        }
        bool operator<(const DataRec &obj) const {
            return id < obj.id || (id == obj.id && timestamp < obj.timestamp);
        }
    };

    typedef vector<DataRec> DataRecs;

    EMRTrackData() {}

    void add(unsigned id, EMRTimeStamp timestamp, T val);
    void finalize();

    DataRecs data;
};

//------------------------------ IMPLEMENTATION
//----------------------------------------

template <class T>
EMRTrackData<T>::DataRec::DataRec(unsigned _id, const EMRTimeStamp &_timestamp,
                                  T _val) {
    id = _id;
    timestamp = _timestamp;
    val = _val;
}

template <class T>
void EMRTrackData<T>::add(unsigned id, EMRTimeStamp timestamp, T val) {
    data.emplace_back(id, timestamp, val);
}

template <class T>
void EMRTrackData<T>::finalize() {
    bool finalized = true;

    for (auto idata = data.begin() + 1; idata < data.end(); ++idata) {
        if (*idata < *(idata - 1)) {
            finalized = false;
            break;
        }
    }

    if (!finalized) {
        sort(data.begin(), data.end());
        // unify same data records (same id,time,ref and value)
        data.resize(distance(data.begin(), unique(data.begin(), data.end())));

        // make sure there is no record with same id,time,ref and different
        // value
        for (auto idata = data.begin() + 1; idata < data.end(); ++idata) {
            if (idata->id == (idata - 1)->id &&
                idata->timestamp == (idata - 1)->timestamp) {
                TGLError("Id %d at time %s already exists", idata->id,
                         idata->timestamp.tostr().c_str());
            }
        }
    }
}

#endif
