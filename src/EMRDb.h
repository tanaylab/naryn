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
        bool             is_global;

        TrackInfo(EMRTrack *_track, const string &_filename, const struct timespec &_timestamp, bool _is_global) :
            track(_track), filename(_filename), timestamp(_timestamp), is_global(_is_global) {}
    };


    static const string TRACK_FILE_EXT;    
    static const string TRACK_ATTRS_FILE_EXT;
    static const string LOGICAL_TRACK_FILE_EXT;

    ~EMRDb();

    const string &grootdir() const { return m_rootdirs[1]; }
    const string &urootdir() const { return m_rootdirs[0]; }

	EMRTrack *track(const string &track);
    const EMRLogicalTrack *logical_track(const string &track);
    const TrackInfo *track_info(const string &track);
    const vector<string> &track_names(bool is_global) { return m_track_names[is_global]; }
    const vector<string> logical_track_names() { 
        vector<string> ltrack_names;
        ltrack_names.reserve(m_logical_tracks.size());
        for (auto ltrack : m_logical_tracks) {
            ltrack_names.push_back(ltrack.first);
        }
        return ltrack_names;
    }

    static void check_track_name(const string &track);

    string logical_track_filename(const string &track_name) const {
        return logical_tracks_dir() + string("/") + track_name +
               LOGICAL_TRACK_FILE_EXT;
    }

    // Sets groot/uroot.
    // Loads track list files and tracks (if load_on_demand==false).
    // If track list file is missing => builds it.
    // Removes outdated tracks from the memory.
    void init(const char *grootdir, const char *urootdir,
                bool gload_on_demand, bool uload_on_demand, bool do_reload);

    // Same as init, but groot/uroot must be set already.
    // Intended to be called at the beginning of each transaction.
    void refresh();

    // Rescans file directory and rebuilds track list file.
    // Loads tracks, if load_on_demand==false.
    // Removes outdated tracks from the memory.
    // Should be called when track list file is suspected to be out of sync
    // with the tracks.
    void reload();

    void load_track(const char *track_name, bool is_global);
    void unload_track(const char *track_name);

    // Add a logical track to the database
    void add_logical_track(const char *track_name, const char *source_name,
                            const vector<int> &values, bool update);
    void add_logical_track(const char *track_name, const char *source_name,
                            bool update);

    void remove_logical_track(const char *track_name, bool update);

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
                        const char *val);

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

    unsigned         m_transact_id{1};
    Name2Track       m_tracks;
    Name2LogicalTrack m_logical_tracks;
    string m_rootdirs[2]; // 0 - user, 1 - global
    bool             m_load_on_demand[2]{ false, false };
    struct timespec  m_track_list_ts[2]{{0, 0}, {0, 0}};
    struct timespec  m_tracks_attrs_ts[2]{{0, 0}, {0, 0}};
    struct timespec  m_logical_tracks_ts{0, 0};
    vector<string> m_track_names[2];    
    Track2Attrs      m_track2attrs[2];
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

    string track_filename(bool is_global, const string &track_name) const { return m_rootdirs[is_global] + string("/") + track_name + TRACK_FILE_EXT; }
    string track_attrs_filename(bool is_global, const string &track_name) const { return m_rootdirs[is_global] + string("/.") + track_name + TRACK_ATTRS_FILE_EXT; }
    string logical_tracks_dir() const { return m_rootdirs[1] + string("/logical"); }    
    string track_list_filename(bool is_global) const { return m_rootdirs[is_global] + "/" + TRACK_LIST_FILENAME; }
    string tracks_attrs_filename(bool is_global) const { return m_rootdirs[is_global] + "/" + TRACKS_ATTRS_FILENAME; }    
    string logical_tracks_filename() const {
        return m_rootdirs[1] + "/" + LOGICAL_TRACKS_FILENAME;
    }
    string ids_filename() const { return m_rootdirs[1] + "/" + IDS_FILENAME; }

    void clear(bool is_global);
    void clear_ids();

    void cache_tracks();

    // opens and locks track list file according to the mode: "r", "r+", "w"
    void lock_track_list(bool is_global, BufferedFile &lock, const char *mode);

    // opens and locks track list files (both global and user) according to the mode: "r", "r+", "w"
    void lock_track_lists(BufferedFile *locks, const char *mode);

    // opens and locks logical track list file according to the mode: "r", "r+", "w"
    void lock_logical_track_list(BufferedFile &lock, const char *mode);

    // Scans root directory for tracks, creates track list file with the gathered data.
    void create_track_list_file(bool is_global, BufferedFile *pbf);

    // Writes data into track list file.
    void update_track_list_file(const Name2Track &tracks, bool is_global, BufferedFile &pbf);

    // Scans logical tracks directory for logical tracks, updates track list file with the gathered data.
    void load_logical_tracks_from_disk();

    // Writes the logical track list file. If the file doesn't exist - recreates it. 
    void update_logical_tracks_file();

    // Loads track list file. If corrupted or missing, recreates it.
    // Removes outdated tracks from memory.
    void load_track_list(bool is_global, BufferedFile *pbf);

    // Loads track list before update (opens the file for r+w and locks it).
    void load_track_list(bool is_global, BufferedFile &bf);

    // Loads logical track list
    void load_logical_tracks();

    // Scans root directory for tracks attributes, creates tracks attributes file with the gathered data.
    void create_tracks_attrs_file(bool is_global, bool locked);

    // Writes data into tracks attributes file.
    void update_tracks_attrs_file(bool is_global, bool locked);

    // Loads track attributes file. If corrupted or missing, recreates it.
    void load_tracks_attrs(bool is_global, bool locked);

    void create_ids_file();
    void load_ids();

    // Rebuilds ids file if dob track has changed. Returns true if file was rebuilt or false if ids file is up-to-date.
    bool rebuild_ids_file_on_dob_change();
};

extern EMRDb *g_db;


//---------------------------------------- IMPLEMENTATION --------------------------------------------

inline unsigned EMRDb::id(size_t idx)
{
    if (m_ids_transact_ts != m_transact_id)
        load_ids();
    return m_ids[idx];
}

inline size_t EMRDb::id2idx(unsigned id)
{
    if (m_ids_transact_ts != m_transact_id)
        load_ids();
    auto itr = m_id2idx.find(id);
    if (itr == m_id2idx.end())
        verror("Id %u that was generated during the iteration does not exist in '%s' track.\n"
               "Make sure the iterator / filter are based on a source containing only the valid ids.", id, DOB_TRACKNAME);
    return itr->second;
}

inline bool EMRDb::id_exists(unsigned id)
{
    if (m_ids_transact_ts != m_transact_id)
        load_ids();
    return m_id2idx.find(id) != m_id2idx.end();
}

inline unsigned EMRDb::num_ids()
{
    if (m_ids_transact_ts != m_transact_id)
        load_ids();
    return m_num_ids;
}

#endif

