#include "EMRDb.h"

#include <dirent.h>
#include <fcntl.h>
#include <limits.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <utime.h>

#include <fstream>
#include <iomanip>
#include <numeric>
#include <algorithm>

#include "BufferedFile.h"
#include "EMRProgressReporter.h"
#include "EMRTrack.h"
#include "strutil.h"

const string EMRDb::TRACK_FILE_EXT(".nrtrack");
const string EMRDb::TRACK_ATTRS_FILE_EXT(".attrs");
const string EMRDb::LOGICAL_TRACK_FILE_EXT(".ltrack");
const char *EMRDb::TRACK_LIST_FILENAME = ".naryn";
const char *EMRDb::TRACKS_ATTRS_FILENAME = ".attrs";
const char *EMRDb::LOGICAL_TRACKS_FILENAME = ".logical_tracks";
const char *EMRDb::DOB_TRACKNAME = "patients.dob";
const char *EMRDb::IDS_FILENAME = ".ids";
const int EMRDb::IDS_SIGNATURE = 0xC0FFEE;

EMRDb *g_db = NULL;

EMRDb::~EMRDb() {
    clear_ids();
    for (Name2Track::iterator itrack = m_tracks.begin();
         itrack != m_tracks.end(); ++itrack) {
        delete itrack->second.track;
    }
}

EMRTrack *EMRDb::track(const string &track) {

    Name2Track::iterator itrack = m_tracks.find(track);
    if (itrack == m_tracks.end())
        return NULL;
    if (!itrack->second.track)
    {
        itrack->second.track = EMRTrack::unserialize(
            track.c_str(), itrack->second.filename.c_str());
        if (itrack->second.track->timestamp() > itrack->second.timestamp)
        {
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

    if (itrack == m_logical_tracks.end())
        return NULL;

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

void EMRDb::cache_tracks() {
    if (std::all_of(m_load_on_demand.begin(), m_load_on_demand.end(), [](bool v) { return v; })) {
        return;
    }

    // If load_on_demand==false, load all the missing tracks into memory.
    EMRProgressReporter progress;
    bool outdated_db = false;
    vector<int> track_names_sizes;
    int progress_length;

    for (int db_idx = 0; db_idx < (int)m_rootdirs.size(); db_idx++) {
        track_names_sizes.push_back((m_track_names[m_rootdirs[db_idx]].size() * (!m_load_on_demand[db_idx])));
    }

    progress_length = std::accumulate(track_names_sizes.begin(),
                                      track_names_sizes.end(), 
                                      decltype(track_names_sizes)::value_type(0)
                        );

    progress.init(progress_length, 1);

    for (int db_idx = 0; db_idx < (int)m_rootdirs.size(); ++db_idx)
    {
        if (m_load_on_demand[db_idx])
            continue;

        vdebug("Caching %s tracks", m_rootdirs[db_idx].c_str());

        int n = 0;

        for (auto &track : m_tracks)
        {
            if (!track.second.track && 
                (!access(track.second.filename.c_str(), F_OK) || errno != ENOENT) &&
                track.second.db_id == m_rootdirs[db_idx]) {
                track.second.track = EMRTrack::unserialize(
                    track.first.c_str(), track.second.filename.c_str());
                if (n++ < 5)
                    vdebug("Track %s cached", track.first.c_str());
                if (track.second.track->timestamp() > track.second.timestamp)
                    outdated_db = true;
            }
            if (n > 5)
                vdebug("%d tracks cached (only first 5 listed)", n);

            check_interrupt();
            progress.report(1);
        }

        if (db_idx == 0){
            // ignore errors while loading ids: dob track might be missing
            try{
                load_ids();
            }
            catch (...){
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

void EMRDb::load_logical_tracks_from_disk() {
    DIR *dir = NULL;

    clear_logical_tracks();

    // scan directory structure
    struct dirent *dirp;
    char filename[PATH_MAX + 100];

    try
    {
        dir = opendir(logical_tracks_dir().c_str());
        if (!dir)
        {
            if (ENOENT == errno)
            {
                mkdir(logical_tracks_dir().c_str(),
                      S_IRUSR | S_IWUSR | S_IXUSR | S_IRGRP | S_IWGRP | S_IWGRP);
                dir = opendir(logical_tracks_dir().c_str());
            }
            else
            {
                verror("Failed to open directory %s: %s",
                       logical_tracks_dir().c_str(), strerror(errno));
            }
        }

        while ((dirp = readdir(dir)))
        {
            struct stat fs;
            int len = strlen(dirp->d_name);

            snprintf(filename, sizeof(filename), "%s/%s", logical_tracks_dir().c_str(),
                    dirp->d_name);
            if (stat(filename, &fs))
                verror("Failed to stat file %s: %s", filename, strerror(errno));

            // is it a normal file having file extension of a track?
            if (S_ISREG(fs.st_mode) &&
                (uint64_t)len > LOGICAL_TRACK_FILE_EXT.size() &&
                !strncmp(dirp->d_name + len - LOGICAL_TRACK_FILE_EXT.size(),
                         LOGICAL_TRACK_FILE_EXT.c_str(),
                         LOGICAL_TRACK_FILE_EXT.size()))
            {
                string ltrack_name(
                    dirp->d_name, 0, len - LOGICAL_TRACK_FILE_EXT.size());

                EMRLogicalTrack ltrack = EMRLogicalTrack::unserialize(filename);

                if (ltrack.source.length() > 0)
                {
                    m_logical_tracks.emplace(ltrack_name, std::move(ltrack));
                }
                else
                {
                    vwarning(
                        "Invalid format of file %s. Please recreate the track and run emr_db.reload().",
                        filename);
                }
            }

            check_interrupt();
        }

        closedir(dir);
        dir = NULL;

        // write the results into logical track list file
        update_logical_tracks_file();
    }
    catch (...){
        if (dir){
            closedir(dir);
        }
        throw;
    }
}

void EMRDb::add_logical_track(const char *track_name, const char *source_name,
                              const vector<int> &values, const bool& create_file, const bool& update) {
    EMRLogicalTrack ltrack(source_name, values);    

    m_logical_tracks.emplace(track_name, ltrack);

    if (create_file){
        string filename = logical_track_filename(string(track_name));
        if (!ltrack.serialize(filename.c_str())) {
            verror("failed to write logical track %s", track_name);
        }
    }

    if (update) {
        update_logical_tracks_file();
    }
}

void EMRDb::add_logical_track(const char *track_name, const char *source_name,
                              const bool& create_file, const bool& update) {
    EMRLogicalTrack ltrack(source_name);    

    m_logical_tracks.emplace(track_name, ltrack);     

    if (create_file){
        string filename = logical_track_filename(string(track_name));
        if (!ltrack.serialize(filename.c_str())) {
            verror("failed to write logical track %s", track_name);
        }
    }

    if (update) {       
        update_logical_tracks_file();
    }
}

void EMRDb::remove_logical_track(const char *track_name, const bool &update) {
    m_logical_tracks.erase(track_name);
    string filename = logical_track_filename(string(track_name));
    if (unlink(filename.c_str()) == -1)
    {
        if (errno != ENOENT)
        {
            verror("Failed to remove file %s: %s", filename.c_str(),
                   strerror(errno));
        }
    }

    if (update) {
        update_logical_tracks_file();
    }
}

void EMRDb::update_logical_tracks_file() {
    BufferedFile bf;
    string filename = logical_tracks_filename();

    lock_logical_track_list(bf, "w");

    vdebug("Creating %s with %lu logical tracks", filename.c_str(),
           m_logical_tracks.size());
    if (bf.open(filename.c_str(), "w"))
    {
        verror("Failed to open file %s: %s", filename.c_str(), strerror(errno));
    }

    for (auto const &ltracks : m_logical_tracks)
    {
        bf.write(ltracks.first.c_str(),
                 ltracks.first.size() + 1); // track name

        bf.write(ltracks.second.source.c_str(),
                 ltracks.second.source.size() + 1); // source track

        uint32_t num_values =
            (uint32_t)ltracks.second.values.size(); // number of attributes

        bf.write(&num_values, sizeof(num_values));
        if (!ltracks.second.values.empty())
        {
            bf.write(
                ltracks.second.values.data(),
                sizeof(int) * ltracks.second.values.size());
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
    m_logical_tracks_ts[m_rootdirs[0]] = {0, 0};
}

void EMRDb::load_logical_tracks() {
    Name2LogicalTrack track_list;

    vdebug("Loading logical track list\n");

    BufferedFile bf;
    string filename = logical_tracks_filename();
    // we open the file and lock it
    if (bf.open(filename.c_str(), "r", true))
    {
        if (errno != ENOENT)
        {
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

    if (m_logical_tracks_ts[m_rootdirs[0]] == get_file_mtime(fs))
    {
        vdebug("Up-to-date logical tracks are already in memory");
        bf.close();
        return;
    }

    // save the current list of logical tracks in case the file is corrupt
    Name2LogicalTrack cur_logical_tracks(m_logical_tracks);
    clear_logical_tracks();

    if (bf.file_size())
    {
        int c;
        string buf;
        string track;
        string source;
        vector<int> values;
        uint32_t num_vals;
        enum
        {
            TRACK,
            SOURCE
        } token = TRACK;

        // the structure of file is track,source,number of values,values(if exists)
        while ((c = bf.getc()) != EOF)
        {
            if (c)
            {
                buf.push_back(c);
            }
            else
            { // end of track string
                if (token == TRACK)
                {
                    track = buf;
                    token = SOURCE;
                }
                else if (token == SOURCE)
                {
                    source = buf;
                    if (bf.read(&num_vals, sizeof(num_vals)) !=
                        sizeof(num_vals))
                        break;
                    if (num_vals > 0)
                    {
                        values.clear();
                        values.resize(num_vals);
                        bf.read(values.data(), sizeof(int) * num_vals);
                        add_logical_track(track.c_str(), source.c_str(), values,
                                          false, false);
                    }
                    else
                    {
                        add_logical_track(track.c_str(), source.c_str(), false, false);
                    }
                    token = TRACK;
                }
                buf.clear();
            }
        }

        if (bf.error())
            verror("Failed to read file %s: %s", bf.file_name().c_str(),
                   strerror(errno));

        if (c != EOF)
        {
            vwarning("Invalid format of file %s, rebuilding it",
                     bf.file_name().c_str());
            bf.close();
            clear_logical_tracks();
            m_logical_tracks = cur_logical_tracks;
            update_logical_tracks_file();
        }
        else
        {
            bf.close();

            m_logical_tracks_ts[m_rootdirs[0]] = get_file_mtime(fs);
            vdebug("Read %lu logical tracks", m_logical_tracks.size());

            if (g_naryn->debug()) {
                int n = 0;
                for (auto track : g_db->logical_track_names())
                {
                    vdebug("%d. %s\n", n + 1, track.c_str());
                    if (++n >= 5)
                    {
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

    if (!ids.size() && !complementary)
        verror("Source ids are empty.");

    uint64_t subset_size = (uint64_t)(ids.size() * fraction + .5);

    if ((!subset_size && !complementary) ||
        (subset_size == ids.size() && complementary))
        verror(
            "The subset is empty. Please choose a different fraction value.");

    clear_ids_subset(true);
    m_ids_subset_src = src;
    m_ids_subset_fraction = fraction;
    m_ids_subset_complementary = complementary;

    for (uint64_t i = 0; i < subset_size; ++i)
    {
        uint64_t idx = (uint64_t)(unif_rand() * (ids.size() - subset_size));

        if (!complementary)
            m_ids_subset.insert(ids[idx]);

        swap(ids[idx], ids[ids.size() - i - 1]);
    }

    if (complementary)
    {
        for (vector<unsigned>::const_iterator iid = ids.begin();
             iid != ids.end() - subset_size; ++iid)
            m_ids_subset.insert(*iid);
    }
}

void EMRDb::clear_ids() {
    if (m_shmem_ids != MAP_FAILED)
        munmap(m_shmem_ids, m_shmem_ids_size);
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

    try
    {
        vdebug("Loading ids...\n");

        string filename = ids_filename();
        struct stat sb;

        while (1)
        {
            if ((fd = open(filename.c_str(), O_RDONLY, 0)) == -1)
            {
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
            while (fcntl(fd, F_SETLKW, &fl) == -1)
            {
                if (errno != EINTR)
                    verror("Locking file %s: %s", filename.c_str(),
                           strerror(errno));
            }
            vdebug("Lock set\n", filename.c_str());

            if (fstat(fd, &sb) == -1)
                verror("stat failed on file %s: %s", filename.c_str(),
                       strerror(errno));

            if (m_ids_ts == get_file_mtime(sb))
            {   // the up-to-date ids file has
                // already been read into memory
                close(fd);
                fd = -1;
                if (rebuild_ids_file_on_dob_change())
                    continue;
                m_ids_transact_ts = m_transact_id;
                vdebug("Up-to-date ids are already in memory\n");
                return;
            }

            vdebug("Loading IDs file\n");
            clear_ids();

            m_shmem_ids_size = sb.st_size;

            if (!m_shmem_ids_size)
            {
                close(fd);
                fd = -1;
                vwarning("File %s is empty, rebuilding it", filename.c_str());
                create_ids_file();
                continue;
            }
#if defined(__APPLE__)
            if ((m_shmem_ids = mmap(NULL, m_shmem_ids_size, PROT_READ,
                                    MAP_PRIVATE, fd, 0)) ==
                MAP_FAILED)
                verror("mmap failed on file %s: %s", filename.c_str(),
                       strerror(errno));
#else
            if ((m_shmem_ids = mmap(NULL, m_shmem_ids_size, PROT_READ,
                                    MAP_PRIVATE | MAP_POPULATE, fd, 0)) ==
                MAP_FAILED)
                verror("mmap failed on file %s: %s", filename.c_str(),
                       strerror(errno));
#endif            

            close(fd);
            fd = -1;

            if (m_shmem_ids_size < sizeof(int) + sizeof(m_dob_ts.tv_sec) +
                                       sizeof(m_dob_ts.tv_nsec) ||
                (m_shmem_ids_size - sizeof(int) - sizeof(m_dob_ts.tv_sec) -
                 sizeof(m_dob_ts.tv_nsec)) %
                    sizeof(unsigned) ||
                *(int *)m_shmem_ids != IDS_SIGNATURE)
            {
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

            if (rebuild_ids_file_on_dob_change())
                continue;

            m_ids = (unsigned *)((char *)m_shmem_ids + sizeof(int) +
                                 sizeof(m_dob_ts.tv_sec) +
                                 sizeof(m_dob_ts.tv_nsec));
            m_num_ids = (m_shmem_ids_size - sizeof(int) -
                         sizeof(m_dob_ts.tv_sec) - sizeof(m_dob_ts.tv_nsec)) /
                        sizeof(unsigned);
            m_ids_ts = get_file_mtime(sb);
            m_ids_transact_ts = m_transact_id;

            for (uint64_t i = 0; i < m_num_ids; ++i)
                m_id2idx[m_ids[i]] = i;

            break;
        }
    }
    catch (...)
    {
        if (fd != -1)
            close(fd);
        clear_ids();
        throw;
    }
}

void EMRDb::create_ids_file() {
    int fd = -1;

    try
    {
        vdebug("Creating IDs file\n");
        string filename = ids_filename();
        fd = creat(filename.c_str(), S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH); // rwrwrw

        struct flock fl;
        memset(&fl, 0, sizeof(fl));
        fl.l_type = F_WRLCK;
        while (fcntl(fd, F_SETLKW, &fl) == -1){
            if (errno != EINTR)
                verror("Locking file %s: %s", filename.c_str(), strerror(errno));
        }

        Name2Track::iterator itrack = m_tracks.find(DOB_TRACKNAME);

        if (itrack == m_tracks.end())
            verror("Cannot retrieve ids: '%s' track is missing", DOB_TRACKNAME);

        if (get_db_idx(itrack->second.db_id) != 0)
            verror("Cannot retrieve ids: '%s' track is not in the global space", DOB_TRACKNAME);

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
    }
    catch (TGLException &e)
    {
        vdebug("%s", e.msg());
        close(fd); // closing fd will also release the lock (see: fcntl,
                   // F_SETLKW)
        unlink(ids_filename().c_str());
        throw;
    }
    close(fd);
}

bool EMRDb::rebuild_ids_file_on_dob_change() {
    struct stat fs;

    if (stat((m_rootdirs[0] + "/" + DOB_TRACKNAME + TRACK_FILE_EXT).c_str(), &fs) == -1) {
        if (errno == ENOENT){
            verror("Failed to retrieve ids: '%s' track is missing", DOB_TRACKNAME);
        }
        verror("Failed to stat '%s' track: %s", DOB_TRACKNAME, strerror(errno));
    }

    if (m_dob_ts != get_file_mtime(fs)){
        // remove an outdated version of dob track from the memory
        // (it is there if the session has already accessed dob track in the
        // past)
        Name2Track::iterator itrack = m_tracks.find(DOB_TRACKNAME);
        if (itrack != m_tracks.end() && itrack->second.track &&
            get_file_mtime(fs) != itrack->second.track->timestamp()){
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

int EMRDb::get_db_idx(const string& db_id) {
    
    vector<string>::iterator itr = std::find(m_rootdirs.begin(), m_rootdirs.end(), db_id);

    if (itr != m_rootdirs.cend()) {
        return distance(m_rootdirs.begin(), itr);
    }

    return -1;
    
}

void EMRDb::validate_rootdirs(const vector<string>& rootdirs){
    struct stat fs;
    int fd1 = -1;
    int fd2 = -1;
    for (auto db : rootdirs){
        // make sure that the directory has read and search permissions        
        try {
            if ((fd1 = open(db.c_str(), O_RDONLY, 0)) == -1) {
                verror("Opening directory %s failed: %s", db.c_str(), strerror(errno));
            }
            if (stat(db.c_str(), &fs)){
                verror("Failed to stat directory %s: %s", db.c_str(),
                       strerror(errno));
            }
            if (!S_ISDIR(fs.st_mode)){
                verror("%s is not a directory", db.c_str());
            }
            if (!(fs.st_mode & S_IXUSR)){
                verror("%s is not searchable ('x' permissions)", db.c_str());
            }
        } catch (...) {
            if (fd1 != -1) {
                close(fd1);
            }
            throw;
        }

        // make sure that .naryn file has read permissions
        string filename = track_list_filename(db);
        try {
            if (access(filename.c_str(), F_OK) == 0){
                if ((fd2 = open(filename.c_str(), O_RDONLY, 0)) == -1) {
                    verror("Opening file %s failed: %s", filename.c_str(),
                        strerror(errno));
                }
            }            
        } catch (...) {
            if (fd2 != -1) {
                close(fd2);
            }
            throw;
        }
    }
}

void EMRDb::init(const vector<string>& rootdirs, const vector<bool>& dirs_load_on_demand, const bool& do_reload) {

    vdebug("EMRDb::init()\n");

    validate_rootdirs(rootdirs);

    ++m_transact_id;

    // if changed the global db - clear logical tracks
    if (m_rootdirs.size() > 0 && rootdirs.size() > 0 && m_rootdirs[0] != rootdirs[0]){
        clear_logical_tracks();
    }

    vector<string> current_rootdirs = m_rootdirs;
    vector<string> new_rootdirs = rootdirs;
    vector<string> dirs_keep;

    sort(new_rootdirs.begin(), new_rootdirs.end());
    sort(current_rootdirs.begin(), current_rootdirs.end());

    // find dbs to keep (do not unload)
    set_intersection(
        new_rootdirs.begin(),
        new_rootdirs.end(),
        current_rootdirs.begin(),
        current_rootdirs.end(),
        back_inserter(dirs_keep));

    for (auto db_id = m_rootdirs.begin(); db_id != m_rootdirs.end(); db_id++)
    {
        // if we don't need the db any more
        if (count(dirs_keep.begin(), dirs_keep.end(), *db_id) == 0){

            clear(*db_id);

            // current global db
            if (get_db_idx(*db_id) == 0){
                clear_ids_subset(true);
            }
        }
    }

    m_rootdirs = rootdirs;
    m_load_on_demand = dirs_load_on_demand;

    if (do_reload) {
        reload();
    } else {
        refresh(true);
    }
}


void EMRDb::clear(string db_id) {
    //go over the tracks list, delete tracks from the relevant dir_idx
    for (auto itrack = m_tracks.begin(); itrack != m_tracks.end();) {
        if (itrack->second.db_id == db_id) {
            delete itrack->second.track;
            itrack = m_tracks.erase(itrack);
        } else {
            ++itrack;
        }
    }

    m_track_list_ts[db_id] = {0, 0};
    m_track_names[db_id].clear();
}

void EMRDb::refresh(bool force) {
    if (m_rootdirs[0].empty())
#ifdef RNARYN
        verror("Database was not loaded. Please call emr_db.connect.");
#else
        verror("Database was not loaded. Please call db_connect.");
#endif
    vdebug("EMRDb::refresh()\n");
    ++m_transact_id;

    for (auto db_id = m_rootdirs.begin(); db_id != m_rootdirs.end(); db_id++) {
        load_track_list(*db_id, NULL, force);
    }

    load_logical_tracks();

    cache_tracks();
}

void EMRDb::reload() {
    if (m_rootdirs[0].empty())
#ifdef RNARYN
        verror("Database was not loaded. Please call emr_db.connect.");
#else
        verror("Database was not loaded. Please call db_connect.");
#endif

    vdebug("EMRDb::reload()\n");

    for (auto db_id = m_rootdirs.begin(); db_id != m_rootdirs.end(); db_id++){
        create_track_list_file(*db_id, NULL);
        create_tracks_attrs_file(*db_id, false);
    }

    load_logical_tracks_from_disk();

    refresh();
}

void EMRDb::lock_track_lists(vector<BufferedFile> &locks, const char *mode) {
    for (int db_idx = 0; db_idx < (int)m_rootdirs.size(); db_idx++) {
        lock_track_list(m_rootdirs[db_idx], locks[db_idx], mode);
    }
}

void EMRDb::lock_track_list(string db_id, BufferedFile &lock, const char *mode){

    vdebug("MODE: %s", mode);

    if (!lock.opened()) {

        string filename = track_list_filename(db_id);

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


void EMRDb::create_track_list_file(string db_id, BufferedFile *_pbf) {
    DIR *dir = NULL;

    vdebug("Rescanning %s dir to acquire list of tracks", db_id.c_str());

    try {
        BufferedFile bf;
        BufferedFile *pbf = _pbf ? _pbf : &bf;

        if (!_pbf) {
            vdebug("Opening %s track list for write", db_id.c_str());
            // Lock the file for writing before scan to prevent concurrent scan
            // at the same time. If not done, an earlier scan might write its
            // results to track list file after a later scan.
            lock_track_list(db_id, bf, "w");
        }

        // scan directory structure
        struct dirent *dirp;
        Name2Track track_list;
        char filename[PATH_MAX + 100];

        // db_id currently represents the db path
        dir = opendir(db_id.c_str());

        if (!dir){
            verror("Failed to open directory %s: %s", db_id.c_str(), strerror(errno));
        }

        while ((dirp = readdir(dir))) {
            struct stat fs;
            int len = strlen(dirp->d_name);

            snprintf(filename, sizeof(filename), "%s/%s", db_id.c_str(), dirp->d_name);

            if (stat(filename, &fs)){
                verror("Failed to stat file %s: %s", filename, strerror(errno));
            }
                
            if (S_ISREG(fs.st_mode) && (uint64_t)len > TRACK_FILE_EXT.size() &&
                !strncmp(dirp->d_name + len - TRACK_FILE_EXT.size(),
                         TRACK_FILE_EXT.c_str(), TRACK_FILE_EXT.size())) {
                string track_name(dirp->d_name, 0, len - TRACK_FILE_EXT.size());
                track_list.emplace(
                    track_name,
                    TrackInfo(NULL, track_filename(db_id, track_name), get_file_mtime(fs), db_id));
            }

            check_interrupt();
        }

        closedir(dir);
        dir = NULL;

        // write the results into track list file
        update_track_list_file(track_list, db_id, *pbf);
    }
    catch (...) {
        if (dir){
            closedir(dir);
        }
        throw;
    }
}

void EMRDb::update_track_list_file(const Name2Track &tracks, string db_id, BufferedFile &bf) {

    vdebug("Writing %ld %s tracks to track list file", tracks.size(), db_id.c_str());

    bf.seek(0, SEEK_SET); // rewind the file position
    bool in_dbs = false;

    for (const auto &name2track : tracks){

        auto pos = std::find(name2track.second.dbs.begin(), name2track.second.dbs.end(), db_id);

        in_dbs = pos != name2track.second.dbs.end();

        if (((name2track.second.db_id == db_id) || in_dbs) &&
            (bf.write(name2track.first.c_str(), name2track.first.size() + 1) !=
                 name2track.first.size() + 1 ||
             (bf.write(&name2track.second.timestamp.tv_sec,
                       sizeof(name2track.second.timestamp.tv_sec)) !=
              sizeof(name2track.second.timestamp.tv_sec)) ||
             (bf.write(&name2track.second.timestamp.tv_nsec,
                       sizeof(name2track.second.timestamp.tv_nsec)) !=
              sizeof(name2track.second.timestamp.tv_nsec)))){
                verror("Failed to write file %s: %s", bf.file_name().c_str(), strerror(errno));
        }

        in_dbs = false; 
            
    }

    bf.truncate(); // file might be open for update and not just for write
}

void EMRDb::load_track_list(string db_id, BufferedFile &bf, bool force) {
    vdebug("Loading %s track list before update\n", db_id.c_str());
    lock_track_list(db_id, bf, "r+");
    load_track_list(db_id, &bf, force);
}

void EMRDb::load_track_list(string db_id, BufferedFile *_pbf, bool force){

    Name2Track track_list;

    while (1) {
        BufferedFile bf;
        BufferedFile *pbf = _pbf ? _pbf : &bf;
        string filename = track_list_filename(db_id);

        if (!_pbf){
            vdebug("Loading %s track list\n", db_id.c_str());
        }
            
        if (!_pbf && pbf->open(filename.c_str(), "r", true)) {
            if (errno != ENOENT){
                verror("Failed to open file %s: %s", filename.c_str(), strerror(errno));
            }

            create_track_list_file(db_id, NULL);
            continue;
        }

        if (!_pbf){
            vdebug("R lock acquired\n");
        }

        pbf->seek(0, SEEK_SET); // rewind the file position

        struct stat fs;
        if (pbf->stat(&fs) == -1){
            verror("stat failed on file %s: %s", pbf->file_name().c_str(), strerror(errno));
        }

        // track list in memory is synced with the track list on disk
        if ((m_track_list_ts[db_id] == get_file_mtime(fs)) && !force) {
            vdebug("Up-to-date %s track list is already in memory", db_id.c_str());
            if (g_naryn->debug()) {
                int n = 0;
                for (auto track : m_track_names[db_id]) {
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

        while ((c = pbf->getc()) != EOF)
        {
            track_name[pos++] = c;

            if ((c && pos >= (int)sizeof(track_name)))
                break;

            if (!c)
            { // end of track name
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
                                       track_filename(db_id, track_name),
                                       timestamp, db_id)) 
                         .second)
                    break;

                pos = 0;
            }
        }

        if (pbf->error())
            verror("Failed to read file %s: %s", pbf->file_name().c_str(), strerror(errno));

        if (c != EOF){
            vwarning("Invalid format of file %s, rebuilding it", pbf->file_name().c_str());
            pbf->close(); // release the read lock
            create_track_list_file(db_id, _pbf);
            track_list.clear();
            continue;
        }

        m_track_list_ts[db_id] = get_file_mtime(fs);
        vdebug("Read %lu tracks", track_list.size());

        if (g_naryn->debug()) {

            int n = 0;
            for (auto track : track_list)
            {
                vdebug("%d. %s\n", n + 1, track.first.c_str());
                if (++n >= 5)
                {
                    vdebug("(Only the first %d tracks are listed)\n", n);
                    break;
                }
            }
        }
        break;
    }

    for (auto &fresh_track : track_list) {
        Name2Track::iterator itrack = m_tracks.find(fresh_track.first);

        if (itrack != m_tracks.end() && itrack->second.db_id != fresh_track.second.db_id){

            //Overriding mechanism
            if (fresh_track.first == DOB_TRACKNAME) {
                verror("Can not override patients.dob track");
            }

            vector<string>::iterator pos = std::find(m_track_names[itrack->second.db_id].begin(), 
                                                     m_track_names[itrack->second.db_id].end(), 
                                                     itrack->first);

            if (pos != m_track_names[itrack->second.db_id].end()){
                m_track_names[itrack->second.db_id].erase(pos);
            }
            
            //when coming to override, save the cascade of dbs
            //already overridden. Then, add the latest one.
            fresh_track.second.dbs = itrack->second.dbs;

            vector<string>::iterator db_exists = std::find(fresh_track.second.dbs.begin(), fresh_track.second.dbs.end(), itrack->second.db_id);

            if (db_exists == fresh_track.second.dbs.end() && get_db_idx(itrack->second.db_id) < get_db_idx(fresh_track.second.db_id)){
                fresh_track.second.dbs.push_back(itrack->second.db_id);
            }

            itrack->second.overridden = 1;
        }
    }

    // From this point no more errors => time to replace our old list of tracks
    // with the new one

    // Look for the common tracks that are already in the memory and are up to
    // date => preserve them
    int n = 0;
    for (auto &fresh_track : track_list)
    {
        Name2Track::iterator itrack = m_tracks.find(fresh_track.first);
        if (itrack != m_tracks.end() && itrack->second.track ){

            //same db and up-to-date
            if ((itrack->second.db_id == fresh_track.second.db_id) && 
                (itrack->second.track->timestamp() >= fresh_track.second.timestamp)) {

                fresh_track.second.track = itrack->second.track;
                itrack->second.track = NULL;

            } if (n++ < 5){
                vdebug("Keep track %s in memory", fresh_track.first.c_str());
            }
        }
    }
    if (n > 5)
        vdebug("%d tracks kept in memory (only first 5 listed)", n);

    // Clear the old tracks
    for (auto itrack = m_tracks.begin(); itrack != m_tracks.end();)
    {   //clear if relevant db, or if overridden
        if ((itrack->second.db_id == db_id) || itrack->second.overridden){
            delete itrack->second.track;
            itrack = m_tracks.erase(itrack);
        }
        else
            ++itrack;
    }
    m_track_names[db_id].clear();

    // Copy the new list of tracks
    m_track_names[db_id].reserve(track_list.size());
    for (const auto &track : track_list)
    {
        m_tracks.insert(track);
        m_track_names[db_id].emplace_back(track.first);
    }
}


void EMRDb::load_track(const char *track_name, const string& db_id){

    string filename = track_filename(db_id, track_name);
    Name2Track::iterator itrack = m_tracks.find(track_name);

    vdebug("Adding track %s to DB\n", track_name);

    if (itrack == m_tracks.end()){
        m_track_names[db_id].push_back(track_name);
    } else {
        delete itrack->second.track;
        m_tracks.erase(itrack);
    }

    BufferedFile bf;
    load_track_list(db_id, bf); // lock track list for write

    EMRTrack *track = EMRTrack::unserialize(track_name, filename.c_str());
    itrack = m_tracks.find(track_name); // search again, load_track_list might
                                        // have loaded this track already
    
    if (itrack == m_tracks.end()){
        m_tracks.emplace(track_name, TrackInfo(track, filename.c_str(), track->timestamp(), db_id));
    } else {      
        itrack->second = TrackInfo(track, filename.c_str(), track->timestamp(), db_id);
    }
        
    update_track_list_file(m_tracks, db_id, bf);
}

void EMRDb::unload_track(const char *track_name, const bool& overridden, const bool& soft){
    
    Name2Track::iterator itrack = m_tracks.find(track_name);

    if (itrack == m_tracks.end())
        return;

    string db_id = itrack->second.db_id;

    vector<string>::iterator itr = find(m_track_names[db_id].begin(), m_track_names[db_id].end(), track_name);

    if (itr != m_track_names[db_id].end()) {
        m_track_names[db_id].erase(itr);
        vdebug("Unloaded track %s from memory", track_name);
    }

    //If the track was overriding another track
    //touch  the relevant  .naryn file  so next
    //refresh will reload the overridden track 
    
    if ((itrack->second.dbs.size() > 0) || overridden) {

        int fd;

        for (int i=0; i < (int)m_rootdirs.size(); i++) {
            if ((fd = open(track_list_filename(m_rootdirs[i]).c_str(), O_WRONLY, 0)) == -1) {
                verror("Failed opening file %s", track_list_filename(m_rootdirs[i]).c_str());
            }
            futimens(fd, NULL);
        }
        
    }

    delete itrack->second.track;
    itrack->second.track = NULL;

    BufferedFile bf;

    if (!soft) {
        load_track_list(db_id, bf); // lock track list for write
    }

    m_tracks.erase(track_name);

    if (!soft) {
        update_track_list_file(m_tracks, db_id, bf);
    }
     
}

EMRDb::Track2Attrs EMRDb::get_tracks_attrs(const vector<string> &tracks, vector<string> &attrs) {
    Track2Attrs res;
    
    vector<bool> tracks_attrs_loaded(m_rootdirs.size());
    vector<BufferedFile> locks(m_rootdirs.size());

    lock_track_lists(locks, "r+");
    string db_id; 
    int db_idx;

    for (const auto &trackname : tracks) {
        Name2Track::iterator itrack = m_tracks.find(trackname);

        if (itrack == m_tracks.end()) {
            Name2LogicalTrack::iterator itrackLogical = m_logical_tracks.find(trackname);
            if (itrackLogical == m_logical_tracks.end()) {
                verror("Track %s does not exist", trackname.c_str());
            }           
            db_id = m_rootdirs[0]; // logical tracks are allowed only on the global db
            db_idx = 0;
        } else {
            db_id = itrack->second.db_id;
            db_idx = get_db_idx(db_id);
        }

        if (!tracks_attrs_loaded[db_idx]) {
            load_tracks_attrs(db_id, true);
            tracks_attrs_loaded[db_idx] = true;
        }

        auto itrack2attrs = m_track2attrs[db_id].find(trackname);
        if (itrack2attrs != m_track2attrs[db_id].end()) {
            if (attrs.empty()){
                res.emplace(trackname, itrack2attrs->second);
            } else {
                Track2Attrs::iterator ires_track2attrs = res.end();
                for (const auto &attr : attrs) {
                    auto iattr = itrack2attrs->second.find(attr);
                    if (iattr != itrack2attrs->second.end()) {
                        if (ires_track2attrs == res.end()){
                            ires_track2attrs = res.emplace(trackname, TrackAttrs()).first;
                        }
                        ires_track2attrs->second.emplace(iattr->first, iattr->second);
                    }
                }
            }
        }
    }
    return res;
}

void EMRDb::set_track_attr(const char *trackname, const char *attr,
                           const char *val, const bool& update) {
    vector<BufferedFile> locks(m_rootdirs.size());
    lock_track_lists(locks, "r+");

    Name2Track::iterator itrack = m_tracks.find(trackname);

    string db_id; 
    int db_idx;
    string track_attrs_fname;

    if (itrack == m_tracks.end()) {
        Name2LogicalTrack::iterator itrackLogical = m_logical_tracks.find(trackname);
        if (itrackLogical == m_logical_tracks.end()) {
            verror("Track %s does not exist", trackname);
        } 
        db_id = m_rootdirs[0]; // logical tracks are allowed only on the global db
        db_idx = 0;
        track_attrs_fname = logical_track_attrs_filename(trackname);
    } else {
        db_id = itrack->second.db_id;
        db_idx = get_db_idx(db_id);
        track_attrs_fname = track_attrs_filename(db_id, trackname);
    }
    
    for (int i=0; i < (int)m_rootdirs.size(); i++) {
        if (i != db_idx) {
            locks[i].close(); // release lock for the other spaces
        }
    }  

    TrackAttrs track_attrs = EMRTrack::load_attrs(trackname, track_attrs_fname.c_str());

    if (val) {
        track_attrs[attr] = val;
    }
    else {
        track_attrs.erase(attr);
    }    

    EMRTrack::save_attrs(trackname, track_attrs_fname.c_str(), track_attrs);

    load_tracks_attrs(db_id, true);

    if (track_attrs.empty()){
        m_track2attrs[db_id].erase(trackname);
    }
    else {
        m_track2attrs[db_id][trackname] = track_attrs;
    }

    if (update){
        update_tracks_attrs_file(db_id, true);
    }
    
}

void EMRDb::load_tracks_attrs(string db_id, bool locked)
{
    BufferedFile lock;
    int db_idx = get_db_idx(db_id);
    if (!locked){
        lock_track_list(db_id, lock, "r+");            
        if (db_idx == 0){
            BufferedFile lock1;
            lock_logical_track_list(lock1, "r+");
        }
    }

    while (1)
    {
        BufferedFile bf;
        string filename = tracks_attrs_filename(db_id);

        if (bf.open(filename.c_str(), "r")){
            if (errno != ENOENT)
                verror("Failed to open file %s: %s", filename.c_str(),
                       strerror(errno));
            create_tracks_attrs_file(db_id, true);
            continue;
        }

        struct stat fs;
        if (bf.stat(&fs) == -1)
            verror("stat failed on file %s: %s", bf.file_name().c_str(),
                   strerror(errno));

        // tracks attributes in memory are synced with the attributes file on
        // disk
        if (m_tracks_attrs_ts[db_id] == get_file_mtime(fs))
        {
            vdebug("Up-to-date %s tracks attributes are already in memory", db_id.c_str());
            return;
        }

        m_track2attrs[db_id].clear();

        if (bf.file_size())
        {
            int c;
            string buf;
            string attr;
            uint32_t num_attrs;
            enum
            {
                TRACK,
                ATTR,
                VAL
            } token = TRACK;
            Track2Attrs::iterator itrack2attrs;

            while ((c = bf.getc()) != EOF)
            {
                if (c)
                    buf.push_back(c);

                if (!c)
                { // end of string
                    if (token == TRACK)
                    {
                        if (bf.read(&num_attrs, sizeof(num_attrs)) !=
                            sizeof(num_attrs))
                            break;
                        if (!num_attrs)
                            break;
                        itrack2attrs = m_track2attrs[db_id]
                                           .emplace(buf, TrackAttrs())
                                           .first;
                        token = ATTR;
                    }
                    else if (token == ATTR)
                    {
                        attr = buf;
                        token = VAL;
                    }
                    else
                    {
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

            if (c != EOF || num_attrs)
            {
                vwarning("Invalid format of file %s, rebuilding it",
                         bf.file_name().c_str());
                bf.close();
                create_tracks_attrs_file(db_id, true);
                continue;
            }
        }

        m_tracks_attrs_ts[db_id] = get_file_mtime(fs);
        vdebug("Read %lu tracks with attributes",
               m_track2attrs[db_id].size());
        break;
    }
}


void EMRDb::create_tracks_attrs_file(string db_id, bool locked)
{
    BufferedFile lock;
    int db_idx = get_db_idx(db_id);

    if (!locked){
        lock_track_list(db_id, lock, "r+");
        if (db_idx == 0){
            BufferedFile lock1;
            lock_logical_track_list(lock1, "r+");
        }
    }

    EMRProgressReporter progress;    
    if (db_idx == 0){ // logical tracks are allowed only on the global db
        progress.init(m_tracks.size() + m_logical_tracks.size(), 1);
    } else {
        progress.init(m_tracks.size(), 1);
    }
    
    m_track2attrs[db_id].clear();

    vdebug("Scanning tracks in %s space for attributes\n", db_id.c_str());

    for (const auto &name2track_info : m_tracks){

        const string &trackname = name2track_info.first;

        string filename = track_attrs_filename(db_id, trackname);
        EMRTrack::TrackAttrs attrs =
            EMRTrack::load_attrs(trackname.c_str(), filename.c_str());
        if (!attrs.empty())
            m_track2attrs[db_id].emplace(trackname, attrs);
        check_interrupt();
        progress.report(1);
    }

    if (db_idx == 0){
        for (const auto &name2track_info : m_logical_tracks){
            const string &trackname = name2track_info.first;
            string filename = logical_track_attrs_filename(trackname);
            EMRTrack::TrackAttrs attrs =
                EMRTrack::load_attrs(trackname.c_str(), filename.c_str());
            if (!attrs.empty())
                m_track2attrs[db_id].emplace(trackname, attrs);
            check_interrupt();
            progress.report(1);
        }
    }

    progress.report_last();

    vdebug("Found %lu tracks with attributes\n",
           m_track2attrs[db_id].size());
    update_tracks_attrs_file(db_id, true);
}

void EMRDb::update_tracks_attrs_file(string db_id, bool locked) {
    BufferedFile lock;
    if (!locked){
        lock_track_list(db_id, lock, "r+");
        int db_idx = get_db_idx(db_id);
        if (db_idx == 0){
            BufferedFile lock1;
            lock_logical_track_list(lock1, "r+");
        }
    }

    BufferedFile bf;
    string filename = tracks_attrs_filename(db_id);

    vdebug("Creating %s with attributes from %lu tracks", filename.c_str(), m_track2attrs[db_id].size());

    if (bf.open(filename.c_str(), "w")){
        verror("Failed to open file %s: %s", filename.c_str(), strerror(errno));
    }

    for (auto const &track2attr : m_track2attrs[db_id]) {

        bf.write(track2attr.first.c_str(), track2attr.first.size() + 1); // track name
        
        uint32_t num_attrs = (uint32_t)track2attr.second.size(); // number of attributes

        bf.write(&num_attrs, sizeof(num_attrs));

        for (const auto &attr : track2attr.second) {
            bf.write(attr.first.c_str(), attr.first.size() + 1);   // attribute
            bf.write(attr.second.c_str(), attr.second.size() + 1); // value
        }
    }

    if (bf.error()){
        verror("Error while writing file %s: %s\n", bf.file_name().c_str(), strerror(errno));
    }
}
