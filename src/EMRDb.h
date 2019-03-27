#ifndef EMRDB_H_INCLUDED
#define EMRDB_H_INCLUDED

#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "EMR.h"
#include "HashFunc.h"

using namespace std;

class EMRTrack;

class EMRDb {
public:
    typedef unordered_set<unsigned> IdsSubset;

    struct TrackInfo {
        EMRTrack *track;
        string   filename;
        bool     is_global;

        TrackInfo(EMRTrack *_track, const char *_filename, bool _is_global) : track(_track), filename(_filename), is_global(_is_global) {}
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

    unsigned minid() const { return m_minid; }
    unsigned maxid() const { return m_maxid; }

	void load(const char *grootdir, const char *urootdir, bool load_on_demand);
    void load_track(const char *track_name, bool is_global);
    void unload_track(const char *track_name);

    const IdsSubset &ids_subset() const { return m_ids_subset; }
    const string &ids_subset_src() const { return m_ids_subset_src; }
    double ids_subset_fraction() const { return m_ids_subset_fraction; }
    bool   ids_subset_complementary() const { return m_ids_subset_complementary; }

    void ids_subset(vector<unsigned> &ids, const char *src, double fraction, bool complementary);
    bool is_in_subset(unsigned id) const { return m_ids_subset.empty() || m_ids_subset.find(id) != m_ids_subset.end(); }
    void clear_ids_subset();

protected:
	typedef unordered_map<string, TrackInfo> Name2Track;

    static const char *CACHE_FILENAME;

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
    unsigned         m_minid;
    unsigned         m_maxid;
    unsigned         m_dir_minid[2];
    unsigned         m_dir_maxid[2];

    void clear();
    void update_track_cache(bool is_user_dir);
};

extern EMRDb *g_db;

#endif

