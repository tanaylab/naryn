#ifndef EMRDB_H_INCLUDED
#define EMRDB_H_INCLUDED

#include <map>
#include <sys/mman.h>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "EMR.h"
#include "HashFunc.h"
#include "naryn.h"
#include "EMRLogicalTrack.h"

using namespace std;

class BufferedFile;
class EMRTrack;

class EMRDb {
public:
    typedef unordered_set<unsigned> IdsSubset;
    typedef map<string, string> TrackAttrs;
    typedef map<string, TrackAttrs> Track2Attrs;

    struct TrackInfo {
        EMRTrack        *track;
        string           filename;
        struct timespec  timestamp;
        string           db_id; // holds the current db which holds the track data.
        vector<string>   dbs; // holds the ids of all the other dbs in which the track appears. 
        bool             overridden;

        TrackInfo(EMRTrack *_track, const string &_filename, const struct timespec &_timestamp, string db_id) :
            track(_track), filename(_filename), timestamp(_timestamp), db_id(db_id), overridden(0) {}
    };

    static const string TRACK_FILE_EXT;    
    static const string TRACK_ATTRS_FILE_EXT;
    static const string LOGICAL_TRACK_FILE_EXT;

    ~EMRDb();
    
    const string &grootdir() { return m_rootdirs[0]; }
    const string &urootdir() { return m_rootdirs[0]; }
    const char* dob_trackname() { return DOB_TRACKNAME; }

	EMRTrack *track(const string &track);
    const bool track_exists(const string &track) {
        return (m_tracks.find(track) != m_tracks.end());
    }
    const EMRLogicalTrack *logical_track(const string &track);
    const bool logical_track_exists(const string &track) {
        return (m_logical_tracks.find(track) != m_logical_tracks.end());
    }
    const TrackInfo *track_info(const string &track);
    const vector<string> &track_names(string db_id) { return m_track_names[db_id]; }
    const vector<string> &rootdirs() { return m_rootdirs; }
    const vector<string> logical_track_names() { 
        vector<string> ltrack_names;
        ltrack_names.reserve(m_logical_tracks.size());
        for (auto ltrack : m_logical_tracks) {
            ltrack_names.push_back(ltrack.first);
        }
        return ltrack_names;
    }

    const vector<string> dependent_logical_tracks(const string &src) {
        vector<string> dependent_ltracks;
        dependent_ltracks.reserve(m_logical_tracks.size());
        for (auto ltrack : m_logical_tracks) {
            if((string)ltrack.second.get_source() == src) {
                dependent_ltracks.push_back(ltrack.first);
            }
        }
        return dependent_ltracks;
    }

    static void check_track_name(const string &track);

    string logical_track_filename(const string &track_name) const {
        return logical_tracks_dir() + string("/") + track_name +
               LOGICAL_TRACK_FILE_EXT;
    }

    // Sets roots - 
    // Loads track list files and tracks (if load_on_demand==false).
    // If track list file is missing => builds it.
    // Removes outdated tracks from the memory.

    void init(const vector<string>& rootdirs, const vector<bool>& dirs_load_on_demand, const bool& do_reload);

    // Same as init, but roots must be set already.
    // Intended to be called at the beginning of each transaction.
    // if force is true, refresh rereads .naryn files even if ts
    // hasn't changed.

    void refresh(bool force=false);

    // Rescans file directory and rebuilds track list file.
    // Loads tracks, if load_on_demand==false.
    // Removes outdated tracks from the memory.
    // Should be called when track list file is suspected to be out of sync
    // with the tracks.
    void reload();

    void load_track(const char *track_name, const string& db_id);
    
    // Unloads the track from internal state and updates .naryn file
    // If soft is set to true, unload track does not update .naryn file, used mainly for overriding mechanism
    void unload_track(const char *track_name, const bool& overridden=true, const bool& soft=false);

    // Add a logical track to the database
    void add_logical_track(const char *track_name, const char *source_name, const vector<int> &values, const bool& create_file, const bool& update);
    void add_logical_track(const char *track_name, const char *source_name, const bool& create_file, const bool& update);

    void remove_logical_track(const char *track_name, const bool& update);

    // Writes the logical track list file. If the file doesn't exist - recreates it. 
    void update_logical_tracks_file();

    // clear loaded logical tracks
    void clear_logical_tracks();

    // Returns tracks attributes.
    // Prefer this method over EMRTrack::load_attrs as the current method
    // might return cached attributes thus eliminating the need for a file
    // access.
    Track2Attrs get_tracks_attrs(const vector<string> &tracks,
                                    vector<string> &attrs);

    // sets track attribute; if val == NULL, the attribute is removed
    void set_track_attr(const char *track_name, const char *attr,
                        const char *val, const bool &update);

    // Writes data into tracks attributes file.
    void update_tracks_attrs_file(string db_id, bool locked);

    unsigned id(size_t idx);
    size_t id2idx(unsigned id);  // returns id index given id
    bool id_exists(unsigned id);
    unsigned num_ids();

    const IdsSubset &ids_subset() const { return m_ids_subset; }
    const string &ids_subset_src() const { return m_ids_subset_src; }
    double ids_subset_fraction() const { return m_ids_subset_fraction; }
    bool ids_subset_complementary() const {
        return m_ids_subset_complementary; }

    void ids_subset(vector<unsigned> &ids, const char *src, double fraction, bool complementary);
    bool is_in_subset(unsigned id) const { return m_ids_subset.empty() || m_ids_subset.find(id) != m_ids_subset.end(); }
    void clear_ids_subset(bool warn);
    int get_db_idx(const string& db_id);

protected:
	typedef unordered_map<string, TrackInfo> Name2Track;
    typedef unordered_map<string, EMRLogicalTrack> Name2LogicalTrack;
    typedef unordered_map<unsigned, size_t> Id2Idx;

    static const char *TRACK_LIST_FILENAME;
    static const char *TRACKS_ATTRS_FILENAME;
    static const char *LOGICAL_TRACKS_FILENAME;
    static const char *DOB_TRACKNAME;
    static const char *IDS_FILENAME;
    static const int   IDS_SIGNATURE;

    unsigned          m_transact_id{1};
    Name2Track        m_tracks;
    struct timespec   m_logical_tracks_ts{0, 0};
    Name2LogicalTrack m_logical_tracks;

    vector<string>                              m_rootdirs;
    vector<bool>                                m_load_on_demand;
    unordered_map<string, struct timespec>      m_track_list_ts;
    unordered_map<string, struct timespec>      m_tracks_attrs_ts;
    unordered_map<string, vector<string>>       m_track_names;
    unordered_map<string, Track2Attrs>          m_track2attrs;
    

    IdsSubset        m_ids_subset;
    string           m_ids_subset_src;
    double           m_ids_subset_fraction;
    bool             m_ids_subset_complementary;
    void            *m_shmem_ids{MAP_FAILED};
    size_t           m_shmem_ids_size;
    struct timespec  m_ids_ts{0, 0};
    struct timespec  m_dob_ts{0, 0};
    unsigned         m_ids_transact_ts{0}; // transact timestamp for ids: if differs from m_transact_ids, ids are reloaded
    unsigned        *m_ids{NULL};
    size_t           m_num_ids{0};
    Id2Idx           m_id2idx;

    
    string track_filename(string db_id, const string &track_name) const { return db_id + string("/") + track_name + TRACK_FILE_EXT; }
    string track_attrs_filename(string db_id, const string &track_name) const { return db_id + string("/.") + track_name + TRACK_ATTRS_FILE_EXT; }
    string logical_track_attrs_filename(const string &track_name) const { return logical_tracks_dir() + string("/.") + track_name + TRACK_ATTRS_FILE_EXT; }
    string logical_tracks_dir() const { return m_rootdirs[0] + string("/logical"); }    
    string track_list_filename(string db_id) const { return db_id + "/" + TRACK_LIST_FILENAME; }
    string tracks_attrs_filename(string db_id) const { return db_id + "/" + TRACKS_ATTRS_FILENAME; }    
    string logical_tracks_filename() const { return m_rootdirs[0] + "/" + LOGICAL_TRACKS_FILENAME;}
    string ids_filename() const { return m_rootdirs[0] + "/" + IDS_FILENAME; }

    // make sure that rootdirs are readable
    void validate_rootdirs(const vector<string> &rootdirs);

    // clear tracks from irrelevant resources by resource idx
    void clear(string db_id);

    void clear_ids();

    void cache_tracks();

    // opens and locks track list file according to the mode: "r", "r+", "w"
    void lock_track_list(string db_id, BufferedFile &lock, const char *mode);

    // opens and locks track list files (both global and user) according to the mode: "r", "r+", "w"
    void lock_track_lists(vector<BufferedFile> &locks, const char *mode);

    // opens and locks logical track list file according to the mode: "r", "r+", "w"
    void lock_logical_track_list(BufferedFile &lock, const char *mode);

    // Scans root directory for tracks, creates track list file with the gathered data.
    void create_track_list_file(string db_id, BufferedFile *pbf);

    // Writes data into track list file.
    void update_track_list_file(const Name2Track &tracks, string db_id, BufferedFile &pbf);

    // Scans logical tracks directory for logical tracks, updates track list file with the gathered data.
    void load_logical_tracks_from_disk();

    // Loads track list file. If corrupted or missing, recreates it.
    // Removes outdated tracks from memory.
    void load_track_list(string db_id, BufferedFile *pbf, bool force=false);

    // Loads track list before update (opens the file for r+w and locks it).
    void load_track_list(string db_id, BufferedFile &bf, bool force=false);

    // Loads logical track list
    void load_logical_tracks();

    // Scans root directory for tracks attributes, creates tracks attributes file with the gathered data.
    void create_tracks_attrs_file(string db_id, bool locked);

    // Loads track attributes file. If corrupted or missing, recreates it.
    void load_tracks_attrs(string db_id, bool locked);

    void create_ids_file();
    void load_ids();

    // Rebuilds ids file if dob track has changed. Returns true if file was rebuilt or false if ids file is up-to-date.
    bool rebuild_ids_file_on_dob_change();
};

extern EMRDb *g_db;


//---------------------------------------- IMPLEMENTATION --------------------------------------------

inline unsigned EMRDb::id(size_t idx){
    if (m_ids_transact_ts != m_transact_id)
        load_ids();
    return m_ids[idx];
}

inline size_t EMRDb::id2idx(unsigned id){
    if (m_ids_transact_ts != m_transact_id)
        load_ids();
    auto itr = m_id2idx.find(id);
    if (itr == m_id2idx.end())
        verror("Id %u that was generated during the iteration does not exist in '%s' track.\n"
               "Make sure the iterator / filter are based on a source containing only the valid ids.", id, DOB_TRACKNAME);
    return itr->second;
}

inline bool EMRDb::id_exists(unsigned id){
    if (m_ids_transact_ts != m_transact_id)
        load_ids();
    return m_id2idx.find(id) != m_id2idx.end();
}

inline unsigned EMRDb::num_ids(){
    if (m_ids_transact_ts != m_transact_id)
        load_ids();
    return m_num_ids;
}

#endif

