#ifndef NRTRACKSPARSE_H_INCLUDED
#define NRTRACKSPARSE_H_INCLUDED

#include <cmath>

#include "NRDb.h"
#include "NRTrack.h"

template <class T>
class NRTrackSparse : public NRTrack {
public:
    virtual unsigned size() const { return m_num_recs; }
    virtual unsigned unique_size() const { return m_base_track ? m_base_track->unique_size() : m_num_percentiles; }
    virtual void     unique_vals(vector<double> &vals) const;
    virtual float    percentile_upper(void *rec) const;
    virtual float    percentile_upper(double val) const;
    virtual float    percentile_lower(void *rec) const;
    virtual float    percentile_lower(double val) const;
    virtual double   minval() const { return m_base_track ? m_base_track->minval() : (double)m_sorted_unique_vals[0]; }
    virtual double   maxval() const { return m_base_track ? m_base_track->maxval() : (double)m_sorted_unique_vals[m_num_percentiles - 1]; }

    virtual void ids(vector<unsigned> &ids);
	virtual void data_recs(NRTrackData<double>::DataRecs &data_recs);

    virtual size_t count_ids(const vector<unsigned> &ids) const;

	static void serialize(BufferedFile &bfile, const NRTrackData<T> &data, unsigned data_size, unsigned flags);

protected:
	friend class NRTrack;

#pragma pack(push)
#pragma pack(1)

    struct __attribute__((__packed__)) Data {
		unsigned id;
		unsigned rec_idx;

        Data() {}
        Data(unsigned _id, unsigned _rec_idx) : id(_id), rec_idx(_rec_idx) {}
        bool operator<(const Data &o) const { return id < o.id; }
	};

	struct __attribute__((__packed__)) Rec {
		NRTimeStamp timestamp;
		T           val;

        Rec() {}
        Rec(const NRTimeStamp &_timestamp, T _val) : timestamp(_timestamp), val(_val) {}
        bool operator<(const Rec &o) const { return timestamp.hour() < o.timestamp.hour(); }

        double v() const { return (double)val; }
        const NRTimeStamp &time() const { return timestamp; }
	};

#pragma pack(pop)

    unsigned  m_data_size;
    unsigned  m_num_recs;
    size_t    m_num_percentiles;
    Data     *m_data;
    Rec      *m_recs;
    float    *m_percentiles;
    T        *m_sorted_unique_vals;

	NRTrackSparse(const char *name, DataType data_type, unsigned flags, void *&mem, size_t &pos, size_t size,
                  unsigned minid, unsigned maxid, unsigned mintime, unsigned maxtime);

    NRTrackSparse(const char *name, NRTrack *base_track, const NRTrackData<T> &track_data, unsigned data_size, DataType data_type,
                  unsigned flags, unsigned minid, unsigned maxid, unsigned mintime, unsigned maxtime);

    unsigned num_recs(Data *idata) const;

	void serialize(const char *filename);

	virtual void set_vals(DataFetcher &df, const NRInterval &interv);
    void set_vals4data(DataFetcher &df, const NRInterval &interv, unsigned end_rec_idx);

	virtual bool begin(Iterator &itr);
	virtual bool next(Iterator &itr);
    virtual bool next(Iterator &itr, const NRPoint &jumpto);
};


//------------------------------ IMPLEMENTATION ----------------------------------------

template <class T>
NRTrackSparse<T>::NRTrackSparse(const char *name, DataType data_type, unsigned flags, void *&mem, size_t &pos, size_t size,
                                unsigned minid, unsigned maxid, unsigned mintime, unsigned maxtime) :
	NRTrack(name, SPARSE, data_type, flags, mem, size, minid, maxid, mintime, maxtime)
{
    read_datum(m_shmem, pos, m_shmem_size, m_data_size, name);
    read_datum(m_shmem, pos, m_shmem_size, m_num_recs, name);
    read_datum(m_shmem, pos, m_shmem_size, m_num_percentiles, name);

    if (pos + m_data_size * sizeof(*m_data) + m_num_recs * sizeof(*m_recs) + m_num_percentiles * sizeof(T) > m_shmem_size)
        TGLError(BAD_FORMAT, "Invalid format of track %s (2)\n", name);

    m_data = (Data *)((char *)m_shmem + pos);
    pos += m_data_size * sizeof(*m_data);

    m_recs = (Rec *)((char *)m_shmem + pos);
    pos += m_num_recs * sizeof(*m_recs);

    m_sorted_unique_vals = (T *)((char *)m_shmem + pos);
    pos += m_num_percentiles * sizeof(T);

    if (is_categorial())
        m_percentiles = NULL;
    else {
        if (pos + m_num_percentiles * sizeof(float) > m_shmem_size)
            TGLError(BAD_FORMAT, "Invalid format of track %s (3)\n", name);

        m_percentiles = (float *)((char *)m_shmem + pos);
        pos += m_num_percentiles * sizeof(float);
    }
}

template <class T>
NRTrackSparse<T>::NRTrackSparse(const char *name, NRTrack *base_track, const NRTrackData<T> &track_data, unsigned data_size, DataType data_type,
                                unsigned flags, unsigned minid, unsigned maxid, unsigned mintime, unsigned maxtime) :
	NRTrack(name, SPARSE, data_type, flags, base_track, minid, maxid, mintime, maxtime)
{
    m_data_size = data_size;
    m_num_recs = track_data.m_key2val.size();

    typename NRTrackData<T>::DataRecs data_recs;
    data_recs.reserve(track_data.m_key2val.size());

    vector<Data> data(data_size);
    vector<Rec> recs(m_num_recs);

    for (typename NRTrackData<T>::Key2Val::const_iterator idata = track_data.m_key2val.begin(); idata != track_data.m_key2val.end(); ++idata) {
        typename NRTrackData<T>::DataRec rec;
        rec.id = idata->first.id;
        rec.timestamp = idata->first.timestamp;
        rec.val = idata->second;
        data_recs.push_back(rec);
    }
    sort(data_recs.begin(), data_recs.end());

    unsigned cur_dataid = (unsigned)-1;
    unsigned data_idx = 0;
    unsigned rec_idx = 0;

    for (typename NRTrackData<T>::DataRecs::const_iterator irec = data_recs.begin(); irec != data_recs.end(); ++irec) {
        if (irec->id != cur_dataid) {
            cur_dataid = irec->id;
            data[data_idx].id = irec->id;
            data[data_idx].rec_idx = rec_idx;
            ++data_idx;
        }
        recs[rec_idx].timestamp = irec->timestamp;
        recs[rec_idx].val = irec->val;
        ++rec_idx;
    }

    size_t mem_size = sizeof(Data) * data.size() + sizeof(Rec) * recs.size();

    if (posix_memalign((void **)&m_mem, 64, mem_size))
        verror("%s", strerror(errno));

    size_t pos = 0;

    memcpy(m_mem + pos, &data.front(), sizeof(Data) * data.size());
    m_data = (Data *)(m_mem + pos);
    pos += sizeof(Data) * data.size();

    memcpy(m_mem + pos, &recs.front(), sizeof(Rec) * recs.size());
    m_recs = (Rec *)(m_mem + pos);
    pos += sizeof(Rec) * recs.size();

    // percentiles are taken from the base track
    m_num_percentiles = 0;
    m_sorted_unique_vals = NULL;
    m_percentiles = NULL;
}

template <class T>
void NRTrackSparse<T>::serialize(BufferedFile &bfile, const NRTrackData<T> &track_data, unsigned data_size, unsigned flags)
{
	typename NRTrackData<T>::DataRecs data_recs;
	data_recs.reserve(track_data.m_key2val.size());

    unsigned num_recs = track_data.m_key2val.size();
    vector<Data> data(data_size);
    vector<Rec> recs(num_recs);
    vector<float> percentiles;
    vector<T> vals;
    vector<T> sorted_unique_vals;

    vals.reserve(track_data.m_key2val.size());
	for (typename NRTrackData<T>::Key2Val::const_iterator idata = track_data.m_key2val.begin(); idata != track_data.m_key2val.end(); ++idata) {
		typename NRTrackData<T>::DataRec rec;
		rec.id = idata->first.id;
		rec.timestamp = idata->first.timestamp;
		rec.val = idata->second;
		data_recs.push_back(rec);
        if ((typeid(rec.val) == typeid(float) || typeid(rec.val) == typeid(double) || typeid(rec.val) == typeid(long double)) && !std::isnan(rec.val))
            vals.push_back(rec.val);
	}
	sort(data_recs.begin(), data_recs.end());

    sort(vals.begin(), vals.end());
    if (vals.size()) {
        sorted_unique_vals.push_back(vals.front());
        for (typename vector<T>::const_iterator ival = vals.begin() + 1; ival < vals.end(); ++ival) {
            if (*ival != *(ival - 1)) {
                percentiles.push_back((ival - vals.begin()) / (double)vals.size());
                sorted_unique_vals.push_back(*ival);
            }
        }
        percentiles.push_back(1.);
    }
    size_t num_percentiles = percentiles.size();

	unsigned cur_dataid = (unsigned)-1;
	unsigned data_idx = 0;
	unsigned rec_idx = 0;

	for (typename NRTrackData<T>::DataRecs::const_iterator irec = data_recs.begin(); irec != data_recs.end(); ++irec) {
		if (irec->id != cur_dataid) {
			cur_dataid = irec->id;
			data[data_idx].id = irec->id;
			data[data_idx].rec_idx = rec_idx;
			++data_idx;
		}
		recs[rec_idx].timestamp = irec->timestamp;
		recs[rec_idx].val = irec->val;
		++rec_idx;
	}

	if (bfile.write(&data_size, sizeof(data_size)) != sizeof(data_size) ||
        bfile.write(&num_recs, sizeof(num_recs)) != sizeof(num_recs) ||
        bfile.write(&num_percentiles, sizeof(num_percentiles)) != sizeof(num_percentiles) ||
        bfile.write(&data.front(), sizeof(Data) * data.size()) != sizeof(Data) * data.size() ||
        bfile.write(&recs.front(), sizeof(Rec) * recs.size()) != sizeof(Rec) * recs.size() ||
        num_percentiles &&
        (bfile.write(&sorted_unique_vals[0], sizeof(sorted_unique_vals[0]) * num_percentiles) != sizeof(sorted_unique_vals[0]) * num_percentiles ||
        !(flags & IS_CATEGORIAL) && bfile.write(&percentiles[0], sizeof(percentiles[0]) * num_percentiles) != sizeof(percentiles[0]) * num_percentiles))
    {
		if (bfile.error())
			TGLError<NRTrack>(FILE_ERROR, "Failed to write a track file %s: %s", bfile.file_name().c_str(), strerror(errno));
		TGLError<NRTrack>(FILE_ERROR, "Failed to write a track file %s", bfile.file_name().c_str());
	}
}

template <class T>
void NRTrackSparse<T>::unique_vals(vector<double> &vals) const
{
    if (m_base_track)
        m_base_track->unique_vals(vals);
    else {
        vals.reserve(m_num_percentiles);
        for (size_t i = 0; i < m_num_percentiles; ++i)
            vals.push_back((double)m_sorted_unique_vals[i]);
    }
}

template <class T>
unsigned NRTrackSparse<T>::num_recs(Data *idata) const
{
	return idata < m_data + m_data_size - 1 ? (idata + 1)->rec_idx - idata->rec_idx : m_num_recs - idata->rec_idx;
}

template <class T>
void NRTrackSparse<T>::ids(vector<unsigned> &ids)
{
    ids.clear();
    ids.reserve(m_data_size);
    for (unsigned idata = 0; idata < m_data_size; ++idata)
        ids.push_back(m_data[idata].id);
}

template <class T>
void NRTrackSparse<T>::data_recs(NRTrackData<double>::DataRecs &data_recs)
{
	data_recs.clear();
	data_recs.reserve(m_num_recs);
	for (unsigned idata = 0; idata < m_data_size; ++idata) {
		unsigned n = num_recs(m_data + idata);
		for (unsigned irec = m_data[idata].rec_idx; irec < m_data[idata].rec_idx + n; ++irec) {
			NRTrackData<double>::DataRec rec;
			rec.id = m_data[idata].id;
			rec.timestamp = m_recs[irec].timestamp;
			rec.val = (double)m_recs[irec].val;
			data_recs.push_back(rec);
		}
	}
}

template <class T>
size_t NRTrackSparse<T>::count_ids(const vector<unsigned> &ids) const
{
    size_t count = 0;
    Data *sdata = m_data;

    for (vector<unsigned>::const_iterator iid = ids.begin(); iid != ids.end(); ++iid) {
        sdata = lower_bound(sdata, m_data + m_data_size, Data(*iid, 0));

        if (sdata >= m_data + m_data_size)
            break;

        if (sdata->id == *iid) {
            if (g_db->is_in_subset(*iid))
                ++count;
            ++sdata;
        }
    }
    return count;
}

template <class T>
float NRTrackSparse<T>::percentile_upper(void *rec) const
{
    if (m_base_track)
        return m_base_track->percentile_upper((double)((Rec *)rec)->v());

    T *ival = lower_bound(m_sorted_unique_vals, m_sorted_unique_vals + m_num_percentiles, ((Rec *)rec)->v());
    return m_percentiles[ival - m_sorted_unique_vals];
}

template <class T>
float NRTrackSparse<T>::percentile_upper(double value) const
{
    if (m_base_track)
        return m_base_track->percentile_upper(value);

    T *ival = lower_bound(m_sorted_unique_vals, m_sorted_unique_vals + m_num_percentiles, (T)value);
    return m_percentiles[ival - m_sorted_unique_vals];
}

template <class T>
float NRTrackSparse<T>::percentile_lower(void *rec) const
{
    if (m_base_track)
        return m_base_track->percentile_lower((double)((Rec *)rec)->v());

    T *ival = lower_bound(m_sorted_unique_vals, m_sorted_unique_vals + m_num_percentiles, ((Rec *)rec)->v());
    return ival == m_sorted_unique_vals ? 0 : m_percentiles[ival - m_sorted_unique_vals - 1];
}

template <class T>
float NRTrackSparse<T>::percentile_lower(double value) const
{
    if (m_base_track)
        return m_base_track->percentile_lower(value);

    T *ival = lower_bound(m_sorted_unique_vals, m_sorted_unique_vals + m_num_percentiles, (T)value);
    return ival == m_sorted_unique_vals ? 0 : m_percentiles[ival - m_sorted_unique_vals - 1];
}

template <class T>
void NRTrackSparse<T>::set_vals(DataFetcher &df, const NRInterval &interv)
{
	// find the patient
    while (1) {
        if (df.m_data_idx >= m_data_size || m_data[df.m_data_idx].id > interv.id) {
            set_nan_vals(df);
            break;
        }

        if (m_data[df.m_data_idx].id == interv.id) {
            set_vals4data(df, interv, m_data[df.m_data_idx].rec_idx + num_recs(m_data + df.m_data_idx));
            break;
        }

        // try next patient index, in many cases that would be the right guess
        ++df.m_data_idx;

        // patient id is still preceeding the given id => no other choice but to run full binary search
        if (df.m_data_idx < m_data_size) {
            if (m_data[df.m_data_idx].id < interv.id) {
                Data *datum = lower_bound(m_data + df.m_data_idx + 1, m_data + m_data_size, Data(interv.id, 0));
                df.m_data_idx = datum - m_data;
            }
            if (df.m_data_idx < m_data_size)
                df.m_rec_idx = m_data[df.m_data_idx].rec_idx;
        }
    }
}

template <class T>
void NRTrackSparse<T>::set_vals4data(DataFetcher &df, const NRInterval &interv, unsigned end_rec_idx)
{
    while (1) {
        if (df.m_rec_idx >= end_rec_idx || m_recs[df.m_rec_idx].timestamp.hour() > interv.etime) {
            set_nan_vals(df);
            break;
        }

        if (m_recs[df.m_rec_idx].timestamp.hour() >= interv.stime && m_recs[df.m_rec_idx].timestamp.hour() <= interv.etime) {
            calc_vals(df, interv, m_recs + df.m_rec_idx, m_recs + end_rec_idx);
            break;
        }

        // try next record index, in many cases that would be the right guess
        ++df.m_rec_idx;

        // timestamp is still preceeding the interval => no choice but to run a binary search
        if (df.m_rec_idx < end_rec_idx && m_recs[df.m_rec_idx].timestamp.hour() < interv.stime) {
            Rec *rec = lower_bound(m_recs + df.m_rec_idx + 1, m_recs + end_rec_idx, Rec(NRTimeStamp(interv.stime, 0), 0));
            df.m_rec_idx = rec - m_recs;
        }
    }
}

template <class T>
bool NRTrackSparse<T>::begin(Iterator &itr)
{
	itr.m_data_idx = 0;
	itr.m_rec_idx = -1;
    itr.m_running_idx = 0;
    return next(itr);
}

template <class T>
bool NRTrackSparse<T>::next(Iterator &itr)
{
	++itr.m_rec_idx;

	while (1) {
		if (itr.m_rec_idx >= m_num_recs) 
			break;

        if (!g_db->is_in_subset(m_data[itr.m_data_idx].id)) {
			++itr.m_data_idx;
            if (itr.m_data_idx >= m_data_size)
                break;
            itr.m_rec_idx = m_data[itr.m_data_idx].rec_idx;
			continue;
		}

		unsigned end_rec_idx = m_data[itr.m_data_idx].rec_idx + num_recs(m_data + itr.m_data_idx);

		// we incremented rec index: does it belong to the next patient already?
		if (itr.m_rec_idx >= end_rec_idx) {
			++itr.m_data_idx;
			continue;
		}

        NRTimeStamp::Hour hour = m_recs[itr.m_rec_idx].timestamp.hour();

		// most of the chances are that this point is fine
        if (hour >= itr.m_stime && hour <= itr.m_etime) {
            if (!itr.m_vals.empty() && itr.m_vals.find(m_recs[itr.m_rec_idx].v()) == itr.m_vals.end()) {
                ++itr.m_rec_idx;
                continue;
            }

            if (itr.m_expiration) {
                bool has_competitors = false;

                for (int irec = (int)itr.m_rec_idx - 1; irec >= (int)m_data[itr.m_data_idx].rec_idx; --irec) {
                    NRTimeStamp::Hour prev_hour = m_recs[irec].timestamp.hour();
                    if (prev_hour != hour && (itr.m_vals.empty() || itr.m_vals.find(m_recs[irec].v()) != itr.m_vals.end())) {
                        if (prev_hour + itr.m_expiration >= hour)
                            has_competitors = true;
                        break;
                    }
                }

                if (has_competitors) {
                    ++itr.m_rec_idx;
                    continue;
                }
            }

            itr.m_point.init(m_data[itr.m_data_idx].id, m_recs[itr.m_rec_idx].timestamp);
            itr.m_running_idx = itr.m_rec_idx;
			return true;
		}

		// the time of the current record already exceeds the scope => skip this patient
		if (hour > itr.m_etime) {
			itr.m_rec_idx = end_rec_idx;
			continue;
		}

		// scope does not fit => run binary search
		Rec *rec = lower_bound(m_recs + itr.m_rec_idx + 1, m_recs + end_rec_idx, Rec(NRTimeStamp(itr.m_stime, 0), 0));
		itr.m_rec_idx = rec - m_recs;
	}

    itr.m_isend = true;
    itr.m_running_idx = m_num_recs;
	return false;
}

template <class T>
bool NRTrackSparse<T>::next(Iterator &itr, const NRPoint &jumpto)
{
    // run a binary search to find the id
    if (m_data[itr.m_data_idx].id < jumpto.id) {
        Data *datum = lower_bound(m_data + itr.m_data_idx + 1, m_data + m_data_size, Data(jumpto.id, 0));
        itr.m_data_idx = datum - m_data;
        if (itr.m_data_idx >= m_data_size) {
            itr.m_isend = true;
            return false;
        }
        itr.m_rec_idx = m_data[itr.m_data_idx].rec_idx;
    } else
        ++itr.m_rec_idx;

    NRTimeStamp::Hour jumpto_hour = jumpto.timestamp.hour();

	while (1) {
        if (itr.m_rec_idx >= m_num_recs)
			break;

        if (!g_db->is_in_subset(m_data[itr.m_data_idx].id)) {
            ++itr.m_data_idx;
            if (itr.m_data_idx >= m_data_size)
                break;
            itr.m_rec_idx = m_data[itr.m_data_idx].rec_idx;
            continue;
        }

		unsigned end_rec_idx = m_data[itr.m_data_idx].rec_idx + num_recs(m_data + itr.m_data_idx);

		// we incremented rec index: does it belong to the next patient already?
		if (itr.m_rec_idx >= end_rec_idx) {
			++itr.m_data_idx;
			continue;
		}

        NRTimeStamp::Hour hour = m_recs[itr.m_rec_idx].timestamp.hour();

		// did we find the matching point?
		if (hour >= itr.m_stime && hour <= itr.m_etime && (m_data[itr.m_data_idx].id != jumpto.id || hour >= jumpto_hour)) {
            if (!itr.m_vals.empty() && itr.m_vals.find(m_recs[itr.m_rec_idx].v()) == itr.m_vals.end()) {
                ++itr.m_rec_idx;
                continue;
            }

            if (itr.m_expiration) {
                bool has_competitors = false;

                for (int irec = (int)itr.m_rec_idx - 1; irec >= (int)m_data[itr.m_data_idx].rec_idx; --irec) {
                    NRTimeStamp::Hour prev_hour = m_recs[irec].timestamp.hour();
                    if (prev_hour != hour && (itr.m_vals.empty() || itr.m_vals.find(m_recs[irec].v()) != itr.m_vals.end())) {
                        if (prev_hour + itr.m_expiration >= hour)
                            has_competitors = true;
                        break;
                    }
                }

                if (has_competitors) {
                    ++itr.m_rec_idx;
                    continue;
                }
            }

            itr.m_point.init(m_data[itr.m_data_idx].id, m_recs[itr.m_rec_idx].timestamp);
            itr.m_running_idx = itr.m_rec_idx;
			return true;
		}

		// the time of the current record already exceeds the scope => skip this patient
		if (hour > itr.m_etime) {
			itr.m_rec_idx = end_rec_idx;
			continue;
		}

		// scope does not fit => run binary search
        unsigned stime = m_data[itr.m_data_idx].id == jumpto.id ? max(itr.m_stime, jumpto_hour) : itr.m_stime;
        Rec *rec = lower_bound(m_recs + itr.m_rec_idx + 1, m_recs + end_rec_idx, Rec(NRTimeStamp(stime, 0), 0));
        itr.m_rec_idx = rec - m_recs;
	}

    itr.m_isend = true;
    itr.m_running_idx = m_num_recs;
	return false;
}

#endif
