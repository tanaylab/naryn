#ifndef NRTRACKDENSE_H_INCLUDED
#define NRTRACKDENSE_H_INCLUDED

#include <algorithm>
#include <cmath>

#include "NRDb.h"
#include "NRTrack.h"

template <class T>
class NRTrackDense : public NRTrack {
public:
    virtual unsigned size() const { return m_num_recs; }
    virtual unsigned unique_size() const { return m_num_percentiles; }
    virtual void     unique_vals(vector<double> &vals);
    virtual float    percentile_upper(void *rec) const;
    virtual float    percentile_upper(double val) const;
    virtual float    percentile_lower(void *rec) const;
    virtual float    percentile_lower(double val) const;
    virtual double   minval() const { return (double)m_sorted_unique_vals[0]; }
    virtual double   maxval() const { return (double)m_sorted_unique_vals[m_num_percentiles - 1]; }

    virtual void ids(vector<unsigned> &ids);
	virtual void data_recs(NRTrackData<double>::DataRecs &data_recs);

    virtual size_t count_ids(const vector<unsigned> &ids) const;

	static void serialize(BufferedFile &bfile, const NRTrackData<T> &data, unsigned minid, unsigned maxid, unsigned flags);

protected:
	friend class NRTrack;

#pragma pack(push)
#pragma pack(1)

	struct __attribute__((__packed__)) Rec {
        NRTimeStamp timestamp;
        T           val;

        Rec() {}
        Rec(const NRTimeStamp &_timestamp, T _val) : timestamp(_timestamp), val(_val) {}
        bool operator<(const Rec &o) const { return timestamp.hour() < o.timestamp.hour(); }

        double v() const { return val; }
        const NRTimeStamp &time() const { return timestamp; }
    };

#pragma pack(pop)

    unsigned  m_num_recs;
    size_t    m_num_percentiles;
    unsigned *m_data;
    Rec      *m_recs;
    float    *m_percentiles;
    T        *m_sorted_unique_vals;

	NRTrackDense(const char *name, DataType data_type, unsigned flags, void *&mem, size_t &pos, size_t size,
                 unsigned minid, unsigned maxid, unsigned mintime, unsigned maxtime);

    unsigned data_size() const { return m_max_id - m_min_id + 1; }
    unsigned num_recs(unsigned dataidx) const;
    bool     data_empty(unsigned dataidx) const { return m_data[dataidx] == (unsigned)-1; }
    unsigned next_dataid(unsigned dataid) const { return next_dataidx(dataid + m_min_id); }
    unsigned next_dataidx(unsigned dataidx) const;

	void serialize(const char *filename);

    virtual void set_vals(DataFetcher &df, const NRInterval &interv);

	virtual bool begin(Iterator &itr);
	virtual bool next(Iterator &itr);
    virtual bool next(Iterator &itr, const NRPoint &jumpto);
};


//------------------------------ IMPLEMENTATION ----------------------------------------

#ifdef error
#undef error
#endif

template <class T>
NRTrackDense<T>::NRTrackDense(const char *name, DataType data_type, unsigned flags, void *&mem, size_t &pos, size_t size,
                              unsigned minid, unsigned maxid, unsigned mintime, unsigned maxtime) :
	NRTrack(name, DENSE, data_type, flags, mem, size, minid, maxid, mintime, maxtime)
{
    read_datum(m_shmem, pos, m_shmem_size, m_num_recs, name);
    read_datum(m_shmem, pos, m_shmem_size, m_num_percentiles, name);

    if (pos + data_size() * sizeof(*m_data) + m_num_recs * sizeof(*m_recs) + m_num_percentiles * sizeof(T) > m_shmem_size)
        TGLError(BAD_FORMAT, "Invalid format of track %s (2)\n", name);

    m_data = (unsigned *)((char *)m_shmem + pos);
    pos += data_size() * sizeof(*m_data);

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
void NRTrackDense<T>::serialize(BufferedFile &bfile, const NRTrackData<T> &track_data, unsigned minid, unsigned maxid, unsigned flags)
{
	typename NRTrackData<T>::DataRecs data_recs;
	data_recs.reserve(track_data.m_key2val.size());

    unsigned data_size = maxid - minid + 1;
    unsigned num_recs = track_data.m_key2val.size();
    vector<unsigned> data(data_size, (unsigned)-1);
    vector<Rec> recs(num_recs);
    vector<float> percentiles;
    vector<T> vals;
    vector<T> sorted_unique_vals;

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

	unsigned rec_idx = 0;

	for (typename NRTrackData<T>::DataRecs::const_iterator irec = data_recs.begin(); irec != data_recs.end(); ++irec) {
		unsigned &_rec_idx = data[irec->id - minid];

		if (_rec_idx == (unsigned)-1)
			_rec_idx = rec_idx;
		recs[rec_idx].timestamp = irec->timestamp;
		recs[rec_idx].val = irec->val;
		++rec_idx;
	}

	if (bfile.write(&num_recs, sizeof(num_recs)) != sizeof(num_recs) ||
        bfile.write(&num_percentiles, sizeof(num_percentiles)) != sizeof(num_percentiles) ||
        bfile.write(&data.front(), sizeof(unsigned) * data.size()) != sizeof(unsigned) * data.size() ||
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
unsigned NRTrackDense<T>::next_dataidx(unsigned dataidx) const
{
    unsigned idrange = data_size();
	for (++dataidx; dataidx < idrange; ++dataidx) {
		if (m_data[dataidx] != (unsigned)-1) 
			return dataidx;
	}
	return (unsigned)-1;
}

template <class T>
void NRTrackDense<T>::unique_vals(vector<double> &vals)
{
    vals.clear();
    vals.reserve(m_num_percentiles);
    for (size_t i = 0; i < m_num_percentiles; ++i)
        vals.push_back((double)m_sorted_unique_vals[i]);
}

template <class T>
unsigned NRTrackDense<T>::num_recs(unsigned dataidx) const
{
	unsigned next_idx = next_dataidx(dataidx);
	return next_idx == (unsigned)-1 ? m_num_recs - m_data[dataidx] : m_data[next_idx] - m_data[dataidx];
}

template <class T>
float NRTrackDense<T>::percentile_upper(void *rec) const
{
    T *ival = lower_bound(m_sorted_unique_vals, m_sorted_unique_vals + m_num_percentiles, ((Rec *)rec)->v());
    return m_percentiles[ival - m_sorted_unique_vals];
}

template <class T>
float NRTrackDense<T>::percentile_upper(double value) const
{
    T *ival = lower_bound(m_sorted_unique_vals, m_sorted_unique_vals + m_num_percentiles, (T)value);
    return m_percentiles[ival - m_sorted_unique_vals];
}

template <class T>
float NRTrackDense<T>::percentile_lower(void *rec) const
{
    T *ival = lower_bound(m_sorted_unique_vals, m_sorted_unique_vals + m_num_percentiles, ((Rec *)rec)->v());
    return ival == m_sorted_unique_vals ? 0 : m_percentiles[ival - m_sorted_unique_vals - 1];
}

template <class T>
float NRTrackDense<T>::percentile_lower(double value) const
{
    T *ival = lower_bound(m_sorted_unique_vals, m_sorted_unique_vals + m_num_percentiles, (T)value);
    return ival == m_sorted_unique_vals ? 0 : m_percentiles[ival - m_sorted_unique_vals - 1];
}

template <class T>
void NRTrackDense<T>::ids(vector<unsigned> &ids)
{
    unsigned idrange = data_size();

    ids.clear();
    for (unsigned dataidx = 0; dataidx < idrange; ++dataidx) {
        if (!data_empty(dataidx))
            ids.push_back(dataidx + m_min_id);
    }
}

template <class T>
void NRTrackDense<T>::data_recs(NRTrackData<double>::DataRecs &data_recs)
{
    unsigned idrange = data_size();

    data_recs.clear();
	data_recs.reserve(m_num_recs);
	for (unsigned dataidx = 0; dataidx < idrange; ++dataidx) {
		unsigned n = num_recs(dataidx);
		for (unsigned irec = m_data[dataidx]; irec < m_data[dataidx] + n; ++irec) {
			NRTrackData<double>::DataRec rec;
			rec.id = dataidx + m_min_id;
			rec.timestamp = m_recs[irec].timestamp;
			rec.val = (double)m_recs[irec].val;
			data_recs.push_back(rec);
		}
	}
}

template <class T>
size_t NRTrackDense<T>::count_ids(const vector<unsigned> &ids) const
{
    size_t count = 0;

    for (vector<unsigned>::const_iterator iid = ids.begin(); iid != ids.end(); ++iid) {
        if (*iid > maxid())
            break;

        if (*iid >= minid() && !data_empty(*iid - minid()) && g_db->is_in_subset(*iid))
            ++count;
    }
    return count;
}

template <class T>
void NRTrackDense<T>::set_vals(DataFetcher &df, const NRInterval &interv)
{
    unsigned data_idx = interv.id - minid();

    if (interv.id < minid() || interv.id > maxid() || data_empty(data_idx))
        set_nan_vals(df);
    else {
        unsigned end_rec_idx = m_data[data_idx] + num_recs(data_idx);

        // check whether df.m_rec_idx is pointing to one of the previous patients and if needed set at to the first record of the current patient
        df.m_rec_idx = max(df.m_rec_idx, m_data[data_idx]);

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
}

template <class T>
bool NRTrackDense<T>::begin(Iterator &itr)
{
	itr.m_data_idx = 0;
	itr.m_rec_idx = -1;
    itr.m_running_idx = 0;
	return next(itr);
}

template <class T>
bool NRTrackDense<T>::next(Iterator &itr)
{
	++itr.m_rec_idx;

	while (1) {
		if (itr.m_rec_idx >= m_num_recs)
			break;

        if (data_empty(itr.m_data_idx)) {
            ++itr.m_data_idx;
            continue;
        }

        unsigned end_rec_idx = m_data[itr.m_data_idx] + num_recs(itr.m_data_idx);

		if (!g_db->is_in_subset(itr.m_data_idx + minid())) {
            itr.m_rec_idx = end_rec_idx;
			++itr.m_data_idx;
            if (itr.m_data_idx >= data_size())
                break;
			continue;
		}

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

                for (int irec = (int)itr.m_rec_idx - 1; irec >= (int)m_data[itr.m_data_idx]; --irec) {
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

			itr.m_point.init(itr.m_data_idx + minid(), m_recs[itr.m_rec_idx].timestamp);
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
bool NRTrackDense<T>::next(Iterator &itr, const NRPoint &jumpto)
{
    if (jumpto.id < minid() || jumpto.id > maxid()) {
        itr.m_isend = true;
        return false;
    }

    unsigned data_idx = jumpto.id - minid();

    if (itr.m_data_idx < data_idx) {
        itr.m_data_idx = data_idx;
        while (itr.m_data_idx < data_size() && data_empty(itr.m_data_idx))
            ++itr.m_data_idx;

        if (itr.m_data_idx >= data_size()) {
            itr.m_isend = true;
            return false;
        }

        itr.m_rec_idx = m_data[itr.m_data_idx];
    } else
        ++itr.m_rec_idx;

    NRTimeStamp::Hour jumpto_hour = jumpto.timestamp.hour();

	while (1) {
		if (itr.m_rec_idx >= m_num_recs)
			break;

		if (data_empty(itr.m_data_idx)) {
			++itr.m_data_idx;
			continue;
		}

        unsigned end_rec_idx = m_data[itr.m_data_idx] + num_recs(itr.m_data_idx);

        if (!g_db->is_in_subset(itr.m_data_idx + minid())) {
            itr.m_rec_idx = end_rec_idx;
            ++itr.m_data_idx;
            if (itr.m_data_idx >= data_size())
                break;
            continue;
        }

		// we incremented rec index: does it belong to the next patient already?
		if (itr.m_rec_idx >= end_rec_idx) {
			++itr.m_data_idx;
			continue;
		}

        NRTimeStamp::Hour hour = m_recs[itr.m_rec_idx].timestamp.hour();

        // did we find the matching point?
		if (hour >= itr.m_stime && hour <= itr.m_etime && (itr.m_data_idx != data_idx || hour >= jumpto_hour)) {
            if (!itr.m_vals.empty() && itr.m_vals.find(m_recs[itr.m_rec_idx].v()) == itr.m_vals.end()) {
                ++itr.m_rec_idx;
                continue;
            }

            if (itr.m_expiration) {
                bool has_competitors = false;

                for (int irec = (int)itr.m_rec_idx - 1; irec >= (int)m_data[itr.m_data_idx]; --irec) {
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

            itr.m_point.init(itr.m_data_idx + minid(), m_recs[itr.m_rec_idx].timestamp);
            itr.m_running_idx = itr.m_rec_idx;
			return true;
		}

		// the time of the current record already exceeds the scope => skip this patient
		if (hour > itr.m_etime) {
			itr.m_rec_idx = end_rec_idx;
			continue;
		}

		// scope does not fit => run binary search
        unsigned stime = itr.m_data_idx == data_idx ? max(itr.m_stime, jumpto_hour) : itr.m_stime;
        Rec *rec = lower_bound(m_recs + itr.m_rec_idx + 1, m_recs + end_rec_idx, Rec(NRTimeStamp(stime, 0), 0));
        itr.m_rec_idx = rec - m_recs;
	}

    itr.m_isend = true;
    itr.m_running_idx = m_num_recs;
	return false;
}

#endif

