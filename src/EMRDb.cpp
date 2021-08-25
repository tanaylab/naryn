#include "EMRDb.h"

#include <dirent.h>
#include <fcntl.h>
#include <limits.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include <fstream>
#include <iomanip>
#include <iostream>

#include "BufferedFile.h"
#include "EMRProgressReporter.h"
#include "EMRTrack.h"
#include "strutil.h"

const string EMRDb::TRACK_FILE_EXT(".nrtrack");
const string EMRDb::TRACK_ATTRS_FILE_EXT(".attrs");
const char *EMRDb::TRACK_LIST_FILENAME = ".naryn";
const char *EMRDb::TRACKS_ATTRS_FILENAME = ".attrs";
const char *EMRDb::LOGICAL_TRACKS_FILENAME = ".logical_tracks";
const char *EMRDb::DOB_TRACKNAME = "patients.dob";
const char *EMRDb::IDS_FILENAME = ".ids";
const int EMRDb::IDS_SIGNATURE = 0xC0FFEE;

EMRDb *g_db = NULL;

EMRDb::~EMRDb() {
    for (Name2Track::iterator itrack = m_tracks.begin();
         itrack != m_tracks.end(); ++itrack)
        delete itrack->second.track;
}

void EMRDb::clear(bool is_global) {
    for (auto itrack = m_tracks.begin(); itrack != m_tracks.end();) {
        if (itrack->second.is_global == is_global) {
            delete itrack->second.track;
            itrack = m_tracks.erase(itrack);
        } else
            ++itrack;
    }

    m_track_list_ts[is_global] = {0, 0};
    m_track_names[is_global].clear();
    m_rootdirs[is_global] = "";

    if (is_global) clear_ids();
}

EMRTrack *EMRDb::track(const string &track) {
    Name2Track::iterator itrack = m_tracks.find(track);
    if (itrack == m_tracks.end()) return NULL;
    if (!itrack->second.track) {
        itrack->second.track = EMRTrack::unserialize(
            track.c_str(), itrack->second.filename.c_str());
        if (itrack->second.track->timestamp() > itrack->second.timestamp) {
#ifdef RNARYN
            vwarning(
                "Track %s seems to be modified outside of naryn.\n"
                "This might slow down the performance.\n"
                "Please call emr_db.reload to fix the problem",
                track.c_str());
#else
            vwarning(
                "Track %s seems to be modified outside of naryn.\n"
                "This might slow down the performance.\n"
                "Please call db_reload to fix the problem",
                track.c_str());
#endif
        }
    }
    return itrack->second.track;
}

const EMRLogicalTrack *EMRDb::logical_track(const string &track) {
    Name2LogicalTrack::iterator itrack = m_logical_tracks.find(track);

    if (itrack == m_logical_tracks.end()) return NULL;

    return (&itrack->second);
}

const EMRDb::TrackInfo *EMRDb::track_info(const string &track) {
    Name2Track::iterator itrack = m_tracks.find(track);
    return itrack == m_tracks.end() ? NULL : &itrack->second;
}

void EMRDb::check_track_name(const string &track) {
    if (track.empty() || track[0] == '.' ||
        (track.length() >= TRACK_FILE_EXT.length() &&
         !track.compare(track.length() - TRACK_FILE_EXT.length(),
                        TRACK_FILE_EXT.length(), TRACK_FILE_EXT)))
        verror("Invalid track name: \"%s\"", track.c_str());
}

void EMRDb::init(const char *grootdir, const char *urootdir,
                 bool gload_on_demand, bool uload_on_demand, bool do_reload) {
    vdebug("EMRDb::init()\n");

    if (!grootdir) verror("Global root directory cannot be NULL");

    ++m_transact_id;

    string rootdirs[] = {urootdir ? urootdir : "", grootdir ? grootdir : ""};
    bool load_on_demand[] = {uload_on_demand, gload_on_demand};

    for (int is_global = 0; is_global < 2; ++is_global) {
        if (m_rootdirs[is_global] != rootdirs[is_global]) {
            clear(is_global);
            if (is_global) clear_ids_subset(true);
            m_rootdirs[is_global] = rootdirs[is_global];
        }
        m_load_on_demand[is_global] =
            rootdirs[is_global].empty() || load_on_demand[is_global];
    }

    if (do_reload)
        reload();
    else
        refresh();
}

void EMRDb::refresh() {
    if (m_rootdirs[1].empty())
#ifdef RNARYN
        verror("Database was not loaded. Please call emr_db.init.");
#else
        verror("Database was not loaded. Please call db_init.");
#endif

    vdebug("EMRDb::refresh()\n");
    ++m_transact_id;
    load_track_list(true, NULL);
    if (!m_rootdirs[0].empty()) load_track_list(false, NULL);

    load_logical_tracks();

    cache_tracks();
}

void EMRDb::reload() {
    if (m_rootdirs[1].empty())
#ifdef RNARYN
        verror("Database was not loaded. Please call emr_db.init.");
#else
        verror("Database was not loaded. Please call db_init.");
#endif

    vdebug("EMRDb::reload()\n");
    create_track_list_file(true, NULL);
    create_tracks_attrs_file(true, false);
    update_logical_tracks_file();
    if (!m_rootdirs[0].empty()) {
        create_track_list_file(false, NULL);
        create_tracks_attrs_file(false, false);
    }

    refresh();
}

void EMRDb::cache_tracks() {
    if (m_load_on_demand[0] && m_load_on_demand[1]) return;

    // If load_on_demand==false, load all the missing tracks into memory.
    EMRProgressReporter progress;
    bool outdated_db = false;

    progress.init((!m_load_on_demand[0]) * m_track_names[0].size() +
                      (!m_load_on_demand[1]) * m_track_names[1].size(),
                  1);

    for (int is_global = 0; is_global < 2; ++is_global) {
        if (m_load_on_demand[is_global]) continue;

        vdebug("Caching %s tracks", is_global ? "global" : "local");
        int n = 0;
        for (auto &track : m_tracks) {
            if (!track.second.track &&
                (!access(track.second.filename.c_str(), F_OK) ||
                 errno != ENOENT)) {
                track.second.track = EMRTrack::unserialize(
                    track.first.c_str(), track.second.filename.c_str());
                if (n++ < 5) vdebug("Track %s cached", track.first.c_str());
                if (track.second.track->timestamp() > track.second.timestamp)
                    outdated_db = true;
            }
            if (n > 5) vdebug("%d tracks cached (only first 5 listed)", n);

            check_interrupt();
            progress.report(1);
        }

        if (is_global) {
            // ignore errors while loading ids: dob track might be missing
            try {
                load_ids();
            } catch (...) {
            }
        }
    }
    progress.report_last();

    if (outdated_db)
#ifdef RNARYN
        vwarning(
            "Database seems to be out of sync, which might slow down "
            "performance.\n"
            "Please call emr_db.reload to fix the problem");
#else
        vwarning(
            "Database seems to be out of sync, which might slow down "
            "performance.\n"
            "Please call db_reload to fix the problem");
#endif
}

void EMRDb::lock_track_lists(BufferedFile *locks, const char *mode) {
    if (!m_rootdirs[0].empty()) lock_track_list(false, locks[0], mode);
    if (!m_rootdirs[1].empty()) lock_track_list(true, locks[1], mode);
}

void EMRDb::lock_track_list(bool is_global, BufferedFile &lock,
                            const char *mode) {
    vdebug("MODE: %s", mode);
    if (!lock.opened()) {
        string filename = track_list_filename(is_global);
        if (lock.open(filename.c_str(), mode, true))
            verror("Failed to open file %s: %s", filename.c_str(),
                   strerror(errno));
        if (!strcmp(mode, "r"))
            vdebug("R lock acquired\n");
        else if (!strcmp(mode, "w"))
            vdebug("W lock acquired\n");
        else
            vdebug("R/W lock acquired\n");
    }
}

void EMRDb::lock_logical_track_list(BufferedFile &lock, const char *mode) {
    vdebug("MODE: %s", mode);
    if (!lock.opened()) {
        string filename = logical_tracks_filename();
        if (lock.open(filename.c_str(), mode, true))
            verror("Failed to open file %s: %s", filename.c_str(),
                   strerror(errno));
        if (!strcmp(mode, "r"))
            vdebug("R lock acquired for logical tracks file\n");
        else if (!strcmp(mode, "w"))
            vdebug("W lock acquired for logical tracks file\n");
        else
            vdebug("R/W lock acquired for logical tracks file\n");
    }
}

void EMRDb::load_track_list(bool is_global, BufferedFile &bf) {
    vdebug("Loading %s track list before update\n",
           is_global ? "global" : "local");
    lock_track_list(is_global, bf, "r+");
    load_track_list(is_global, &bf);
}

void EMRDb::load_track_list(bool is_global, BufferedFile *_pbf) {
    Name2Track track_list;

    while (1) {
        BufferedFile bf;
        BufferedFile *pbf = _pbf ? _pbf : &bf;
        string filename = track_list_filename(is_global);

        if (!_pbf)
            vdebug("Loading %s track list\n", is_global ? "global" : "local");

        if (!_pbf && pbf->open(filename.c_str(), "r", true)) {
            if (errno != ENOENT)
                verror("Failed to open file %s: %s", filename.c_str(),
                       strerror(errno));
            create_track_list_file(is_global, NULL);
            continue;
        }

        if (!_pbf) vdebug("R lock acquired\n");

        pbf->seek(0, SEEK_SET);  // rewind the file position

        struct stat fs;
        if (pbf->stat(&fs) == -1)
            verror("stat failed on file %s: %s", pbf->file_name().c_str(),
                   strerror(errno));

        // track list in memory is synced with the track list on disk
        if (m_track_list_ts[is_global] == fs.st_mtim) {
            vdebug("Up-to-date %s track list is already in memory",
                   is_global ? "global" : "local");
            if (g_naryn->debug()) {
                int n = 0;
                for (auto track : m_track_names[is_global]) {
                    vdebug("%d. %s\n", n + 1, track.c_str());
                    if (++n >= 5) {
                        vdebug("(Only the first %d tracks are listed)\n", n);
                        break;
                    }
                }
            }
            return;
        }

        int c;
        int pos = 0;
        char track_name[PATH_MAX];

        while ((c = pbf->getc()) != EOF) {
            track_name[pos++] = c;

            if ((c && pos >= (int)sizeof(track_name))) break;

            if (!c) {  // end of track name
                struct timespec timestamp;

                if (pbf->read(&timestamp.tv_sec, sizeof(timestamp.tv_sec)) !=
                        sizeof(timestamp.tv_sec) ||
                    pbf->read(&timestamp.tv_nsec, sizeof(timestamp.tv_nsec)) !=
                        sizeof(timestamp.tv_nsec))
                    break;

                if (!track_list
                         .emplace(
                             track_name,
                             TrackInfo(NULL,
                                       track_filename(is_global, track_name),
                                       timestamp, is_global))
                         .second)
                    break;

                pos = 0;
            }
        }

        if (pbf->error())
            verror("Failed to read file %s: %s", pbf->file_name().c_str(),
                   strerror(errno));

        if (c != EOF) {
            vwarning("Invalid format of file %s, rebuilding it",
                     pbf->file_name().c_str());
            pbf->close();  // release the read lock
            create_track_list_file(is_global, _pbf);
            track_list.clear();
            continue;
        }

        m_track_list_ts[is_global] = fs.st_mtim;
        vdebug("Read %lu tracks", track_list.size());
        if (g_naryn->debug()) {
            int n = 0;
            for (auto track : track_list) {
                vdebug("%d. %s\n", n + 1, track.first.c_str());
                if (++n >= 5) {
                    vdebug("(Only the first %d tracks are listed)\n", n);
                    break;
                }
            }
        }
        break;
    }

    for (const auto &fresh_track : track_list) {
        Name2Track::iterator itrack = m_tracks.find(fresh_track.first);
        if (itrack != m_tracks.end() &&
            itrack->second.is_global != fresh_track.second.is_global)
            verror("Track %s appears both in global and user directories",
                   itrack->first.c_str());
    }

    // From this point no more errors => time to replace our old list of tracks
    // with the new one

    // Look for the common tracks that are already in the memory and are up to
    // date => preserve them
    int n = 0;
    for (auto &fresh_track : track_list) {
        Name2Track::iterator itrack = m_tracks.find(fresh_track.first);
        if (itrack != m_tracks.end() && itrack->second.track &&
            itrack->second.track->timestamp() >= fresh_track.second.timestamp) {
            if (n++ < 5)
                vdebug("Keep track %s in memory", fresh_track.first.c_str());
            fresh_track.second.track = itrack->second.track;
            itrack->second.track = NULL;
        }
    }
    if (n > 5) vdebug("%d tracks kept in memory (only first 5 listed)", n);

    // Clear the old tracks
    for (auto itrack = m_tracks.begin(); itrack != m_tracks.end();) {
        if (itrack->second.is_global == is_global) {
            delete itrack->second.track;
            itrack = m_tracks.erase(itrack);
        } else
            ++itrack;
    }
    m_track_names[is_global].clear();

    // Copy the new list of tracks
    m_track_names[is_global].reserve(track_list.size());
    for (const auto &track : track_list) {
        m_tracks.insert(track);
        m_track_names[is_global].emplace_back(track.first);
    }
}

void EMRDb::create_track_list_file(bool is_global, BufferedFile *_pbf) {
    DIR *dir = NULL;

    vdebug("Rescanning %s dir to acquire list of tracks",
           is_global ? "global" : "local");
    try {
        BufferedFile bf;
        BufferedFile *pbf = _pbf ? _pbf : &bf;

        if (!_pbf) {
            vdebug("Opening %s track list for write",
                   is_global ? "global" : "local");
            // Lock the file for writing before scan to prevent concurrent scan
            // at the same time. If not done, an earlier scan might write its
            // results to track list file after a later scan.
            lock_track_list(is_global, bf, "w");
        }

        // scan directory structure
        struct dirent *dirp;
        Name2Track track_list;
        char filename[PATH_MAX + 100];

        dir = opendir(m_rootdirs[is_global].c_str());
        if (!dir)
            verror("Failed to open directory %s: %s",
                   m_rootdirs[is_global].c_str(), strerror(errno));

        while ((dirp = readdir(dir))) {
            struct stat fs;
            int len = strlen(dirp->d_name);

            sprintf(filename, "%s/%s", m_rootdirs[is_global].c_str(),
                    dirp->d_name);
            if (stat(filename, &fs))
                verror("Failed to stat file %s: %s", filename, strerror(errno));

            // is it a normal file having file extension of a track?
            if (S_ISREG(fs.st_mode) && (size_t)len > TRACK_FILE_EXT.size() &&
                !strncmp(dirp->d_name + len - TRACK_FILE_EXT.size(),
                         TRACK_FILE_EXT.c_str(), TRACK_FILE_EXT.size())) {
                string track_name(dirp->d_name, 0, len - TRACK_FILE_EXT.size());
                track_list.emplace(
                    track_name,
                    TrackInfo(NULL, track_filename(is_global, track_name),
                              fs.st_mtim, is_global));
            }

            check_interrupt();
        }

        closedir(dir);
        dir = NULL;

        // write the results into track list file
        update_track_list_file(track_list, is_global, *pbf);
    } catch (...) {
        if (dir) closedir(dir);
        throw;
    }
}

void EMRDb::update_track_list_file(const Name2Track &tracks, bool is_global,
                                   BufferedFile &bf) {
    vdebug("Writing %ld %s tracks to track list file", tracks.size(),
           is_global ? "global" : "local");
    bf.seek(0, SEEK_SET);  // rewind the file position

    for (const auto &name2track : tracks) {
        if (name2track.second.is_global == is_global &&
            (bf.write(name2track.first.c_str(), name2track.first.size() + 1) !=
                 name2track.first.size() + 1 ||
             (bf.write(&name2track.second.timestamp.tv_sec,
                       sizeof(name2track.second.timestamp.tv_sec)) !=
              sizeof(name2track.second.timestamp.tv_sec)) ||
             (bf.write(&name2track.second.timestamp.tv_nsec,
                       sizeof(name2track.second.timestamp).tv_nsec) !=
              sizeof(name2track.second.timestamp.tv_nsec))))
            verror("Failed to write file %s: %s", bf.file_name().c_str(),
                   strerror(errno));
    }

    bf.truncate();  // file might be open for update and not just for write
}

void EMRDb::add_logical_track(const char *track_name, const char *source_name,
                              const vector<int> &values, bool update) {    

    m_logical_tracks.emplace(track_name, EMRLogicalTrack(source_name, values));

    if (update) {
        update_logical_tracks_file();
    }
}

void EMRDb::add_logical_track(const char *track_name, const char *source_name,
                              bool update) {    

    m_logical_tracks.emplace(track_name, EMRLogicalTrack(source_name));   

    if (update) {
        update_logical_tracks_file();
    }
}

void EMRDb::remove_logical_track(const char *track_name, bool update) {
    m_logical_tracks.erase(track_name);    

    if (update) {
        update_logical_tracks_file();
    }
}

void EMRDb::load_track(const char *track_name, bool is_global) {
    string filename = track_filename(is_global, track_name);
    Name2Track::iterator itrack = m_tracks.find(track_name);

    vdebug("Adding track %s to DB\n", track_name);
    if (itrack == m_tracks.end())
        m_track_names[is_global].push_back(track_name);
    else {
        delete itrack->second.track;
        m_tracks.erase(itrack);
    }

    BufferedFile bf;
    load_track_list(is_global, bf);  // lock track list for write

    EMRTrack *track = EMRTrack::unserialize(track_name, filename.c_str());
    itrack = m_tracks.find(track_name);  // search again, load_track_list might
                                         // have loaded this track already
    if (itrack == m_tracks.end())
        m_tracks.emplace(track_name, TrackInfo(track, filename.c_str(),
                                               track->timestamp(), is_global));
    else
        itrack->second =
            TrackInfo(track, filename.c_str(), track->timestamp(), is_global);

    update_track_list_file(m_tracks, is_global, bf);
}

void EMRDb::unload_track(const char *track_name) {
    Name2Track::iterator itrack = m_tracks.find(track_name);

    if (itrack == m_tracks.end()) return;

    bool is_global = itrack->second.is_global;

    vector<string>::iterator itr =
        find(m_track_names[is_global].begin(), m_track_names[is_global].end(),
             track_name);
    if (itr != m_track_names[is_global].end()) {
        m_track_names[is_global].erase(itr);
        vdebug("Unloaded track %s from memory", track_name);
    }

    delete itrack->second.track;
    itrack->second.track = NULL;

    BufferedFile bf;
    load_track_list(is_global, bf);  // lock track list for write
    m_tracks.erase(track_name);
    update_track_list_file(m_tracks, is_global, bf);
}

EMRDb::Track2Attrs EMRDb::get_tracks_attrs(const vector<string> &tracks,
                                           vector<string> &attrs) {
    Track2Attrs res;
    bool tracks_attrs_loaded[2] = {false, false};
    BufferedFile locks[2];
    lock_track_lists(locks, "r+");

    for (const auto &trackname : tracks) {
        Name2Track::iterator itrack = m_tracks.find(trackname);

        if (itrack == m_tracks.end()){
            Name2LogicalTrack::iterator itrackLogical = m_logical_tracks.find(trackname);
            if (itrackLogical == m_logical_tracks.end()){
                verror("Track %s does not exist", trackname.c_str());
            } else {
                verror("Track %s is a logical track", trackname.c_str());
            }            
        }

        bool is_global = itrack->second.is_global;
        if (!tracks_attrs_loaded[is_global]) {
            load_tracks_attrs(is_global, true);
            tracks_attrs_loaded[is_global] = true;
        }

        auto itrack2attrs = m_track2attrs[is_global].find(trackname);
        if (itrack2attrs != m_track2attrs[is_global].end()) {
            if (attrs.empty())
                res.emplace(trackname, itrack2attrs->second);
            else {
                Track2Attrs::iterator ires_track2attrs = res.end();
                for (const auto &attr : attrs) {
                    auto iattr = itrack2attrs->second.find(attr);
                    if (iattr != itrack2attrs->second.end()) {
                        if (ires_track2attrs == res.end())
                            ires_track2attrs =
                                res.emplace(trackname, TrackAttrs()).first;
                        ires_track2attrs->second.emplace(iattr->first,
                                                         iattr->second);
                    }
                }
            }
        }
    }
    return res;
}

void EMRDb::set_track_attr(const char *trackname, const char *attr,
                           const char *val) {
    BufferedFile locks[2];
    lock_track_lists(locks, "r+");

    Name2Track::iterator itrack = m_tracks.find(trackname);

    

    if (itrack == m_tracks.end()){
        Name2LogicalTrack::iterator itrackLogical = m_logical_tracks.find(trackname);
        if (itrackLogical == m_logical_tracks.end()){
            verror("Track %s does not exist", trackname);
        } else {
            verror("Track %s is a logical track", trackname);
        }          
    } 

    bool is_global = itrack->second.is_global;
    locks[!is_global].close();  // release lock for the other space

    string track_attrs_fname(track_attrs_filename(is_global, trackname));
    TrackAttrs track_attrs =
        EMRTrack::load_attrs(trackname, track_attrs_fname.c_str());

    if (val)
        track_attrs[attr] = val;
    else
        track_attrs.erase(attr);

    EMRTrack::save_attrs(trackname, track_attrs_fname.c_str(), track_attrs);

    load_tracks_attrs(is_global, true);

    if (track_attrs.empty())
        m_track2attrs[is_global].erase(trackname);
    else
        m_track2attrs[is_global][trackname] = track_attrs;

    update_tracks_attrs_file(is_global, true);
}

void EMRDb::load_tracks_attrs(bool is_global, bool locked) {
    BufferedFile lock;
    if (!locked) lock_track_list(is_global, lock, "r+");

    while (1) {
        BufferedFile bf;
        string filename = tracks_attrs_filename(is_global);

        if (bf.open(filename.c_str(), "r")) {
            if (errno != ENOENT)
                verror("Failed to open file %s: %s", filename.c_str(),
                       strerror(errno));
            create_tracks_attrs_file(is_global, true);
            continue;
        }

        struct stat fs;
        if (bf.stat(&fs) == -1)
            verror("stat failed on file %s: %s", bf.file_name().c_str(),
                   strerror(errno));

        // tracks attributes in memory are synced with the attributes file on
        // disk
        if (m_tracks_attrs_ts[is_global] == fs.st_mtim) {
            vdebug("Up-to-date %s tracks attributes are already in memory",
                   is_global ? "global" : "local");
            return;
        }

        m_track2attrs[is_global].clear();

        if (bf.file_size()) {
            int c;
            string buf;
            string attr;
            uint32_t num_attrs;
            enum { TRACK, ATTR, VAL } token = TRACK;
            Track2Attrs::iterator itrack2attrs;

            while ((c = bf.getc()) != EOF) {
                if (c) buf.push_back(c);

                if (!c) {  // end of string
                    if (token == TRACK) {
                        if (bf.read(&num_attrs, sizeof(num_attrs)) !=
                            sizeof(num_attrs))
                            break;
                        if (!num_attrs) break;
                        itrack2attrs = m_track2attrs[is_global]
                                           .emplace(buf, TrackAttrs())
                                           .first;
                        token = ATTR;
                    } else if (token == ATTR) {
                        attr = buf;
                        token = VAL;
                    } else {
                        itrack2attrs->second.emplace(attr, buf);
                        --num_attrs;
                        token = num_attrs ? ATTR : TRACK;
                    }

                    buf.clear();
                }
            }

            if (bf.error())
                verror("Failed to read file %s: %s", bf.file_name().c_str(),
                       strerror(errno));

            if (c != EOF || num_attrs) {
                vwarning("Invalid format of file %s, rebuilding it",
                         bf.file_name().c_str());
                bf.close();
                create_tracks_attrs_file(is_global, true);
                continue;
            }
        }

        m_tracks_attrs_ts[is_global] = fs.st_mtim;
        vdebug("Read %lu tracks with attributes",
               m_track2attrs[is_global].size());
        break;
    }
}

void EMRDb::create_tracks_attrs_file(bool is_global, bool locked) {
    BufferedFile lock;
    if (!locked) lock_track_list(is_global, lock, "r+");

    EMRProgressReporter progress;
    progress.init(m_tracks.size(), 1);
    m_track2attrs[is_global].clear();

    vdebug("Scanning tracks in %s space for attributes\n",
           is_global ? "global" : "user");
    for (const auto &name2track_info : m_tracks) {
        const string &trackname = name2track_info.first;
        string filename = track_attrs_filename(is_global, trackname);
        EMRTrack::TrackAttrs attrs =
            EMRTrack::load_attrs(trackname.c_str(), filename.c_str());
        if (!attrs.empty()) m_track2attrs[is_global].emplace(trackname, attrs);
        check_interrupt();
        progress.report(1);
    }
    progress.report_last();

    vdebug("Found %lu tracks with attributes\n",
           m_track2attrs[is_global].size());
    update_tracks_attrs_file(is_global, true);
}

void EMRDb::update_tracks_attrs_file(bool is_global, bool locked) {
    BufferedFile lock;
    if (!locked) lock_track_list(is_global, lock, "r+");

    BufferedFile bf;
    string filename = tracks_attrs_filename(is_global);

    vdebug("Creating %s with attributes from %lu tracks", filename.c_str(),
           m_track2attrs[is_global].size());
    if (bf.open(filename.c_str(), "w"))
        verror("Failed to open file %s: %s", filename.c_str(), strerror(errno));

    for (auto const &track2attr : m_track2attrs[is_global]) {
        bf.write(track2attr.first.c_str(),
                 track2attr.first.size() + 1);  // track name
        uint32_t num_attrs =
            (uint32_t)track2attr.second.size();  // number of attributes
        bf.write(&num_attrs, sizeof(num_attrs));
        for (const auto &attr : track2attr.second) {
            bf.write(attr.first.c_str(), attr.first.size() + 1);    // attribute
            bf.write(attr.second.c_str(), attr.second.size() + 1);  // value
        }
    }
    if (bf.error())
        verror("Error while writing file %s: %s\n", bf.file_name().c_str(),
               strerror(errno));
}

void EMRDb::update_logical_tracks_file() {
    BufferedFile bf;
    string filename = logical_tracks_filename();

    lock_logical_track_list(bf, "w");

    vdebug("Creating %s with %lu logical tracks", filename.c_str(),
           m_logical_tracks.size());
    if (bf.open(filename.c_str(), "w")){
        verror("Failed to open file %s: %s", filename.c_str(), strerror(errno));
    }
    
    for (auto const &ltracks : m_logical_tracks) {
        bf.write(ltracks.first.c_str(),
                 ltracks.first.size() + 1);  // track name        
        
        bf.write(ltracks.second.source.c_str(),
                 ltracks.second.source.size() + 1);  // source track

        uint32_t num_values =
            (uint32_t)ltracks.second.values.size();  // number of attributes

        bf.write(&num_values, sizeof(num_values));
        if (!ltracks.second.values.empty()) {
            bf.write(                
                ltracks.second.values.data(), 
                sizeof(int)*ltracks.second.values.size()
                );
        }
    }      

    if (bf.error())
        verror("Error while writing file %s: %s\n", bf.file_name().c_str(),
               strerror(errno));

    // release lock
    bf.close();
}

void EMRDb::clear_logical_tracks() {
    m_logical_tracks.clear();    
}

void EMRDb::load_logical_tracks() {
    Name2LogicalTrack track_list;    

    vdebug("Loading logical track list");
    
    BufferedFile bf;        
    string filename = logical_tracks_filename();
    // we open the file and lock it
    if (bf.open(filename.c_str(), "r", true)) {
        if (errno != ENOENT) {
            verror("Failed to open file %s for reading: %s",
                    filename.c_str(), strerror(errno));
        }
        // if file doesn't exist - create it
        update_logical_tracks_file();
        return;
    }

    struct stat fs;
    if (bf.stat(&fs) == -1)
        verror("stat failed on file %s: %s", bf.file_name().c_str(),
                strerror(errno));

    if (m_logical_tracks_ts == fs.st_mtim) {
        vdebug("Up-to-date logical tracks are already in memory");
        bf.close();
        return;
    }      

    // save the current list of logical tracks in case the file is corrupt
    Name2LogicalTrack cur_logical_tracks(m_logical_tracks); 
    clear_logical_tracks();        

    if (bf.file_size()) {
        int c;
        string buf;
        string track;
        string source;
        vector<int> values;
        uint32_t num_vals;
        enum { TRACK, SOURCE } token = TRACK;               

        // the structure of file is track,source,number of values,values(if exists)        
        while ((c = bf.getc()) != EOF) {
            if (c) {
                buf.push_back(c); 
            } else { // end of track string
                if (token == TRACK){                         
                    track = buf;                    
                    token = SOURCE;
                } else if (token == SOURCE){                        
                    source = buf;                    
                    if (bf.read(&num_vals, sizeof(num_vals)) !=
                        sizeof(num_vals))
                        break;                    
                    if (num_vals > 0) {
                        values.clear();
                        values.resize(num_vals);
                        bf.read(values.data(), sizeof(int) * num_vals);                        
                        add_logical_track(track.c_str(), source.c_str(), values,
                                          false);
                    } else {
                        add_logical_track(track.c_str(), source.c_str(), false);
                    }
                    token = TRACK;
                }
                buf.clear();
            }            
        }

        if (bf.error())
            verror("Failed to read file %s: %s", bf.file_name().c_str(),
                    strerror(errno));
        
        if (c != EOF) {
            vwarning("Invalid format of file %s, rebuilding it",
                     bf.file_name().c_str());
            bf.close();
            clear_logical_tracks();
            m_logical_tracks = cur_logical_tracks;
            update_logical_tracks_file();         
        } else {
            bf.close();

            m_logical_tracks_ts = fs.st_mtim;
            vdebug("Read %lu logical tracks", m_logical_tracks.size());
            
            if (g_naryn->debug()) {
                int n = 0;
                for (auto track : g_db->logical_track_names()) {
                    vdebug("%d. %s\n", n + 1, track.c_str());
                    if (++n >= 5) {
                        vdebug("(Only the first %d logical tracks are listed)\n", n);
                        break;
                    }
                }
            }
        }
    }
}

void EMRDb::clear_ids_subset(bool warn) {
    if (warn && !m_ids_subset.empty())
        vwarning("Current subset of ids will be reset");

    m_ids_subset.clear();
    m_ids_subset_fraction = 1;
    m_ids_subset_complementary = false;
}

void EMRDb::ids_subset(vector<unsigned> &ids, const char *src, double fraction,
                       bool complementary) {
    if (fraction < 0 || fraction > 1)
        verror("Invalid value of fraction, must be in [0,1] range.");

    if ((fraction == 1 && complementary) || (fraction == 0 && !complementary))
        verror(
            "The subset is empty. Please choose a different fraction value.");

    if (!ids.size() && !complementary) verror("Source ids are empty.");

    size_t subset_size = (size_t)(ids.size() * fraction + .5);

    if ((!subset_size && !complementary) ||
        (subset_size == ids.size() && complementary))
        verror(
            "The subset is empty. Please choose a different fraction value.");

    clear_ids_subset(true);
    m_ids_subset_src = src;
    m_ids_subset_fraction = fraction;
    m_ids_subset_complementary = complementary;

    for (size_t i = 0; i < subset_size; ++i) {
        size_t idx = (size_t)(unif_rand() * (ids.size() - subset_size));

        if (!complementary) m_ids_subset.insert(ids[idx]);

        swap(ids[idx], ids[ids.size() - i - 1]);
    }

    if (complementary) {
        for (vector<unsigned>::const_iterator iid = ids.begin();
             iid != ids.end() - subset_size; ++iid)
            m_ids_subset.insert(*iid);
    }
}

void EMRDb::clear_ids() {
    if (m_shmem_ids != MAP_FAILED) munmap(m_shmem_ids, m_shmem_ids_size);
    m_shmem_ids = MAP_FAILED;

    m_ids_ts = {0, 0};
    m_dob_ts = {0, 0};
    m_ids_transact_ts = 0;
    m_ids = NULL;
    m_num_ids = 0;
    m_id2idx.clear();
}

void EMRDb::load_ids() {
    int fd = -1;

    try {
        vdebug("Loading ids...\n");

        string filename = ids_filename();
        struct stat sb;

        while (1) {
            if ((fd = open(filename.c_str(), O_RDONLY, 0)) == -1) {
                if (errno != ENOENT)
                    verror("Opening file %s: %s", filename.c_str(),
                           strerror(errno));
                create_ids_file();
                continue;
            }

            struct flock fl;
            memset(&fl, 0, sizeof(fl));
            fl.l_type = F_RDLCK;

            vdebug("Acquiring write lock for %s\n", filename.c_str());
            while (fcntl(fd, F_SETLKW, &fl) == -1) {
                if (errno != EINTR)
                    verror("Locking file %s: %s", filename.c_str(),
                           strerror(errno));
            }
            vdebug("Lock set\n", filename.c_str());

            if (fstat(fd, &sb) == -1)
                verror("stat failed on file %s: %s", filename.c_str(),
                       strerror(errno));

            if (m_ids_ts == sb.st_mtim) {  // the up-to-date ids file has
                                           // already been read into memory
                close(fd);
                fd = -1;
                if (rebuild_ids_file_on_dob_change()) continue;
                m_ids_transact_ts = m_transact_id;
                vdebug("Up-to-date ids are already in memory\n");
                return;
            }

            vdebug("Loading IDs file\n");
            clear_ids();

            m_shmem_ids_size = sb.st_size;

            if (!m_shmem_ids_size) {
                close(fd);
                fd = -1;
                vwarning("File %s is empty, rebuilding it", filename.c_str());
                create_ids_file();
                continue;
            }

            if ((m_shmem_ids = mmap(NULL, m_shmem_ids_size, PROT_READ,
                                    MAP_PRIVATE | MAP_POPULATE, fd, 0)) ==
                MAP_FAILED)
                verror("mmap failed on file %s: %s", filename.c_str(),
                       strerror(errno));

            close(fd);
            fd = -1;

            if (m_shmem_ids_size < sizeof(int) + sizeof(m_dob_ts.tv_sec) +
                                       sizeof(m_dob_ts.tv_nsec) ||
                (m_shmem_ids_size - sizeof(int) - sizeof(m_dob_ts.tv_sec) -
                 sizeof(m_dob_ts.tv_nsec)) %
                    sizeof(unsigned) ||
                *(int *)m_shmem_ids != IDS_SIGNATURE) {
                vwarning("Invalid format of %s file, rebuilding it (%d)",
                         filename.c_str());
                create_ids_file();
                continue;
            }
            memcpy(&m_dob_ts.tv_sec, (char *)m_shmem_ids + sizeof(int),
                   sizeof(m_dob_ts.tv_sec));
            memcpy(&m_dob_ts.tv_nsec,
                   (char *)m_shmem_ids + sizeof(int) + sizeof(m_dob_ts.tv_sec),
                   sizeof(m_dob_ts.tv_nsec));

            if (rebuild_ids_file_on_dob_change()) continue;

            m_ids = (unsigned *)((char *)m_shmem_ids + sizeof(int) +
                                 sizeof(m_dob_ts.tv_sec) +
                                 sizeof(m_dob_ts.tv_nsec));
            m_num_ids = (m_shmem_ids_size - sizeof(int) -
                         sizeof(m_dob_ts.tv_sec) - sizeof(m_dob_ts.tv_nsec)) /
                        sizeof(unsigned);
            m_ids_ts = sb.st_mtim;
            m_ids_transact_ts = m_transact_id;

            for (size_t i = 0; i < m_num_ids; ++i) m_id2idx[m_ids[i]] = i;

            break;
        }
    } catch (...) {
        if (fd != -1) close(fd);
        clear_ids();
        throw;
    }
}

void EMRDb::create_ids_file() {
    int fd = -1;

    try {
        vdebug("Creating IDs file\n");
        string filename = ids_filename();
        fd = creat(filename.c_str(), S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP |
                                         S_IROTH | S_IWOTH);  // rwrwrw

        struct flock fl;
        memset(&fl, 0, sizeof(fl));
        fl.l_type = F_WRLCK;
        while (fcntl(fd, F_SETLKW, &fl) == -1) {
            if (errno != EINTR)
                verror("Locking file %s: %s", filename.c_str(),
                       strerror(errno));
        }

        Name2Track::iterator itrack = m_tracks.find(DOB_TRACKNAME);
        if (itrack == m_tracks.end())
            verror("Cannot retrieve ids: '%s' track is missing", DOB_TRACKNAME);

        if (!itrack->second.is_global)
            verror("Cannot retrieve ids: '%s' track is not in the global space",
                   DOB_TRACKNAME);

        EMRTrack *dob = track(DOB_TRACKNAME);
        const struct timespec &timestamp = dob->timestamp();
        vector<unsigned> ids;
        dob->ids(ids);

        if (write(fd, &IDS_SIGNATURE, sizeof(IDS_SIGNATURE)) !=
                sizeof(IDS_SIGNATURE) ||
            write(fd, &timestamp.tv_sec, sizeof(timestamp.tv_sec)) !=
                sizeof(timestamp.tv_sec) ||
            write(fd, &timestamp.tv_nsec, sizeof(timestamp.tv_nsec)) !=
                sizeof(timestamp.tv_nsec) ||
            write(fd, &ids.front(), sizeof(unsigned) * ids.size()) !=
                (int64_t)(sizeof(unsigned) * ids.size()))
            verror("Failed to write file %s: %s", filename.c_str(),
                   strerror(errno));
    } catch (TGLException &e) {
        vdebug("%s", e.msg());
        close(fd);  // closing fd will also release the lock (see: fcntl,
                    // F_SETLKW)
        unlink(ids_filename().c_str());
        throw;
    }
    close(fd);
}

bool EMRDb::rebuild_ids_file_on_dob_change() {
    struct stat fs;
    if (stat((m_rootdirs[1] + "/" + DOB_TRACKNAME + TRACK_FILE_EXT).c_str(),
             &fs) == -1) {
        if (errno == ENOENT)
            verror("Failed to retrieve ids: '%s' track is missing",
                   DOB_TRACKNAME);
        verror("Failed to stat '%s' track: %s", DOB_TRACKNAME, strerror(errno));
    }

    if (m_dob_ts != fs.st_mtim) {
        // remove an outdated version of dob track from the memory
        // (it is there if the session has already accessed dob track in the
        // past)
        Name2Track::iterator itrack = m_tracks.find(DOB_TRACKNAME);
        if (itrack != m_tracks.end() && itrack->second.track &&
            fs.st_mtim != itrack->second.track->timestamp()) {
            delete itrack->second.track;
            itrack->second.track = NULL;
        }

        vdebug("'%s' track had been updated, rebuilding %s file\n",
               DOB_TRACKNAME, ids_filename().c_str());
        create_ids_file();
        return true;
    }
    return false;
}
