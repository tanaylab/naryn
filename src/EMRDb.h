#ifndef EMRDB_H_INCLUDED
#define EMRDB_H_INCLUDED

#include <sys/mman.h>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "EMR.h"
#include "HashFunc.h"
#include "naryn.h"

using namespace std;

class EMRTrack;

class EMRDb {
public:
    typedef unordered_set<unsigned> IdsSubset;

    struct TrackInfo {
        EMRTrack *track;
        string   filename;
        time_t   timestamp;
        bool     is_global;

        TrackInfo(EMRTrack *_track, const char *_filename, time_t _timestamp, bool _is_global) :
            track(_track), filename(_filename), timestamp(_timestamp), is_global(_is_global) {}
    };

    static const string TRACK_FILE_EXT;

	~EMRDb() { clear(); }

	const string &grootdir() const { return m_grootdir; }
    const string &urootdir() const { return m_urootdir; }

	EMRTrack *track(const string &track);
    const TrackInfo *track_info(const string &track);
	const vector<string> &track_names() { return m_track_names; }
    const vector<string> &global_track_names() { return m_global_track_names; }
    const vector<string> &user_track_names() { return m_user_track_names; }

    static void check_track_name(const string &track);

	void load(const char *grootdir, const char *urootdir, bool load_on_demand);
    void load_track(const char *track_name, bool is_global);
    void unload_track(const char *track_name);

    unsigned id(size_t idx);
    size_t id2idx(unsigned id);  // returns id index given id
    bool id_exists(unsigned id);
    unsigned num_ids();

    const IdsSubset &ids_subset() const { return m_ids_subset; }
    const string &ids_subset_src() const { return m_ids_subset_src; }
    double ids_subset_fraction() const { return m_ids_subset_fraction; }
    bool   ids_subset_complementary() const { return m_ids_subset_complementary; }

    void ids_subset(vector<unsigned> &ids, const char *src, double fraction, bool complementary);
    bool is_in_subset(unsigned id) const { return m_ids_subset.empty() || m_ids_subset.find(id) != m_ids_subset.end(); }
    void clear_ids_subset();

protected:
	typedef unordered_map<string, TrackInfo> Name2Track;
    typedef unordered_map<unsigned, size_t> Id2Idx;

    static const char *CACHE_FILENAME;
    static const char *IDS_FILENAME;
    static const int IDS_SIGNATURE;

	Name2Track       m_tracks;
	vector<string>   m_track_names;
    vector<string>   m_global_track_names;
    vector<string>   m_user_track_names;
    IdsSubset        m_ids_subset;
    string           m_ids_subset_src;
    double           m_ids_subset_fraction;
    bool             m_ids_subset_complementary;
	string           m_grootdir;
    string           m_urootdir;
    void            *m_shmem_ids{MAP_FAILED};
    size_t           m_shmem_ids_size;
    unsigned         m_ids_session_ts{0}; // session timestamp for ids
    unsigned        *m_ids{NULL};
    size_t           m_num_ids{0};
    Id2Idx           m_id2idx;

    void clear();
    void update_track_cache(bool is_user_dir);
    void clear_ids();
    void load_ids();
    void create_ids_file();
};

extern EMRDb *g_db;
extern unsigned g_session_id;


//---------------------------------------- IMPLEMENTATION --------------------------------------------

inline unsigned EMRDb::id(size_t idx)
{
    if (m_ids_session_ts != g_session_id)
        load_ids();
    return m_ids[idx];
}

inline size_t EMRDb::id2idx(unsigned id)
{
    if (m_ids_session_ts != g_session_id)
        load_ids();
    auto itr = m_id2idx.find(id);
    if (itr == m_id2idx.end())
        verror("Id %u that was generated during the iteration does not exist in 'dob' track.\n"
               "Make sure the iterator / filter are based on a source containing only the valid ids.", id);
    return itr->second;
}

inline bool EMRDb::id_exists(unsigned id)
{
    if (m_ids_session_ts != g_session_id)
        load_ids();
    return m_id2idx.find(id) != m_id2idx.end();
}

inline unsigned EMRDb::num_ids()
{
    if (m_ids_session_ts != g_session_id)
        load_ids();
    return m_num_ids;
}

#endif

