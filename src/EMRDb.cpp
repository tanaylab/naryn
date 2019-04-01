#include <dirent.h>
#include <fcntl.h>
#include <limits.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "EMRDb.h"
#include "EMRProgressReporter.h"
#include "EMRTrack.h"

const string  EMRDb::TRACK_FILE_EXT(".nrtrack");
const char   *EMRDb::CACHE_FILENAME = ".tracks";
const char   *EMRDb::IDS_FILENAME = ".ids";
const int     EMRDb::IDS_SIGNATURE = 0xCACA0;

EMRDb *g_db = NULL;

void EMRDb::clear()
{
	for (Name2Track::iterator itrack = m_tracks.begin(); itrack != m_tracks.end(); ++itrack)
		delete itrack->second.track;
    m_tracks.clear();
    m_track_names.clear();
    m_global_track_names.clear();
    m_user_track_names.clear();
    clear_ids_subset();
    clear_ids();

    m_grootdir = "";
    m_urootdir = "";
}

EMRTrack *EMRDb::track(const string &track)
{
	Name2Track::iterator itrack = m_tracks.find(track);
    if (itrack == m_tracks.end())
        return NULL;
    if (!itrack->second.track)
        itrack->second.track = EMRTrack::unserialize(track.c_str(), itrack->second.filename.c_str());
    return itrack->second.track;
}

const EMRDb::TrackInfo *EMRDb::track_info(const string &track)
{
    Name2Track::iterator itrack = m_tracks.find(track);
    return itrack == m_tracks.end() ? NULL : &itrack->second;
}

void EMRDb::check_track_name(const string &track)
{
    if (track.empty() || track[0] == '.' ||
        (track.length() >= TRACK_FILE_EXT.length() && !track.compare(track.length() - TRACK_FILE_EXT.length(), TRACK_FILE_EXT.length(), TRACK_FILE_EXT)))
        verror("Invalid track name: \"%s\"", track.c_str());
}

void EMRDb::load(const char *grootdir, const char *urootdir, bool load_on_demand)
{
	DIR *dir = NULL;
    const char *dirnames[] = { grootdir, urootdir };

	try {
        clear();

        bool load_tracks[] = { true, true };
        char filename[PATH_MAX + 100];
        vector<string> filenames[2];

        if (load_on_demand) {     // read the list of tracks from a cached file
            for (int is_user_dir = 0; is_user_dir < 2; ++is_user_dir) {
                if (!dirnames[is_user_dir])
                    continue;

                BufferedFile bf;
                sprintf(filename, "%s/%s", dirnames[is_user_dir], CACHE_FILENAME);

                if (bf.open(filename, "r")) {
                    if (errno != ENOENT)
                        vwarning("Failed to open file %s: %s", filename, strerror(errno));
                    continue;
                }

                int c;
                int pos = 0;
                char track_name[PATH_MAX];
                vector<string> &track_names = is_user_dir ? m_user_track_names : m_global_track_names;
                unordered_map<string, time_t> read_tracks;

                while ((c = bf.getc()) != EOF) {
                    track_name[pos++] = c;

                    if ((c && pos >= (int)sizeof(track_name))) {
                        vwarning("Invalid format of file %s, rewriting it", filename);
                        break;
                    } else if (!c) { // end of track name
                        time_t timestamp;

                        if (bf.read(&timestamp, sizeof(timestamp)) != sizeof(timestamp) ||
                            !read_tracks.emplace(track_name, timestamp).second)
                        {
                            vwarning("Invalid format of file %s, rewriting it", filename);
                            break;
                        }
                        pos = 0;
                    }
                }

                if (c == EOF) {
                    if (bf.error())
                        vwarning("Failed to read file %s: %s", filename, strerror(errno));
                    else {
                        track_names.reserve(read_tracks.size());
                        m_track_names.reserve(m_track_names.size() + read_tracks.size());
                        for (const auto &itrack : read_tracks) {
                            const string &name = itrack.first;
                            time_t timestamp = itrack.second;

                            if (m_tracks.find(name) != m_tracks.end())
                                verror("Track %s appears both in global and user directories", name.c_str());

                            snprintf(filename, sizeof(filename), "%s/%s%s", dirnames[is_user_dir], name.c_str(), TRACK_FILE_EXT.c_str());
                            m_tracks.insert(pair<string, TrackInfo>(name, TrackInfo(NULL, filename, timestamp, !is_user_dir)));
                            m_track_names.push_back(name);
                            track_names.push_back(name);
                        }

                        load_tracks[is_user_dir] = false;
                    }
                }
            }
        }

        for (int is_user_dir = 0; is_user_dir < 2; ++is_user_dir) {
            if (!is_user_dir)
                m_grootdir = dirnames[is_user_dir];
            else if (urootdir)
                m_urootdir = dirnames[is_user_dir];
            else {
                m_urootdir = "";
                continue;
            }

            if (!load_tracks[is_user_dir])
                continue;

            dir = opendir(dirnames[is_user_dir]);
    		struct dirent *dirp;

    		if (!dir)
    			verror("Failed to open directory %s: %s", dirnames[is_user_dir], strerror(errno));

    		while ((dirp = readdir(dir))) {
    			struct stat s;
    			int len = strlen(dirp->d_name);

    			sprintf(filename, "%s/%s", dirnames[is_user_dir], dirp->d_name);
    			if (stat(filename, &s))
    				verror("Failed to stat file %s: %s", filename, strerror(errno));

    			// is it a normal file having file extension of a track?
    			if (S_ISREG(s.st_mode) && (size_t)len > TRACK_FILE_EXT.size() &&
    				!strncmp(dirp->d_name + len - TRACK_FILE_EXT.size(), TRACK_FILE_EXT.c_str(), TRACK_FILE_EXT.size()))
    				filenames[is_user_dir].push_back(dirp->d_name);

                check_interrupt();
            }

    		closedir(dir);
    		dir = NULL;
        }

		EMRProgressReporter progress;

        if (load_tracks[0] || load_tracks[1])
            progress.init(load_tracks[0] * filenames[0].size() + load_tracks[1] * filenames[1].size(), 1);

        for (int is_user_dir = 0; is_user_dir < 2; ++is_user_dir) {
            if (!dirnames[is_user_dir] || !load_tracks[is_user_dir])
                continue;

            vector<string> &track_names = is_user_dir ? m_user_track_names : m_global_track_names;

    		for (vector<string>::const_iterator ifilename = filenames[is_user_dir].begin(); ifilename != filenames[is_user_dir].end(); ++ifilename) {
                string track_name(*ifilename, 0, ifilename->size() - TRACK_FILE_EXT.size());
                sprintf(filename, "%s/%s", dirnames[is_user_dir], ifilename->c_str());
                if (m_tracks.find(track_name) != m_tracks.end())
                    verror("Track %s appears both in global and user directories", track_name.c_str());

                EMRTrack *track = EMRTrack::unserialize(track_name.c_str(), filename);
                m_tracks.emplace(track_name, TrackInfo(track, filename, track->timestamp(), !is_user_dir));
    			m_track_names.push_back(track_name);
                track_names.push_back(track_name);

                check_interrupt();
    			progress.report(1);
    		}

            update_track_cache(is_user_dir);
        }

        // ignore errors while loading ids: dob track might be missing
        try {
            if (!load_on_demand)
                load_ids();
        } catch (...) {}

        if (load_tracks[0] || load_tracks[1])
            progress.report_last();
	} catch (...) {
		if (dir) 
			closedir(dir);
        clear();
		throw;
	}
}

void EMRDb::load_track(const char *track_name, bool is_global)
{
    string filename = (is_global ? g_db->grootdir() : g_db->urootdir()) + string("/") + track_name + TRACK_FILE_EXT;

    unordered_map<string, TrackInfo>::iterator itrack = m_tracks.find(track_name);
    if (itrack == m_tracks.end()) {
        m_track_names.push_back(track_name);
        if (is_global)
            m_global_track_names.push_back(track_name);
        else
            m_user_track_names.push_back(track_name);
    } else {
        delete itrack->second.track;
        m_tracks.erase(itrack);
    }

    EMRTrack *track = EMRTrack::unserialize(track_name, filename.c_str());
    m_tracks.insert(pair<string, TrackInfo>(track_name, TrackInfo(track, filename.c_str(), track->timestamp(), is_global)));
    update_track_cache(!is_global);
}

void EMRDb::unload_track(const char *track_name)
{
    Name2Track::iterator itrack = m_tracks.find(track_name);

    if (itrack == m_tracks.end())
        return;

    bool is_global = itrack->second.is_global;

    if (itrack->second.is_global) {
        vector<string>::iterator itr = find(m_global_track_names.begin(), m_global_track_names.end(), track_name);
        if (itr != m_global_track_names.end())
            m_global_track_names.erase(itr);
    } else {
        vector<string>::iterator itr = find(m_user_track_names.begin(), m_user_track_names.end(), track_name);
        if (itr != m_user_track_names.end())
            m_user_track_names.erase(itr);
    }

    vector<string>::iterator itr = find(m_track_names.begin(), m_track_names.end(), track_name);
    if (itr != m_track_names.end())
        m_track_names.erase(itr);

    delete itrack->second.track;
    m_tracks.erase(itrack);
    update_track_cache(!is_global);
}

void EMRDb::clear_ids_subset()
{
    m_ids_subset.clear();
    m_ids_subset_fraction = 1;
    m_ids_subset_complementary = false;
}

void EMRDb::ids_subset(vector<unsigned> &ids, const char *src, double fraction, bool complementary)
{
    if (fraction < 0 || fraction > 1)
        verror("Invalid value of fraction, must be in [0,1] range.");

    if ((fraction == 1 && complementary) || (fraction == 0 && !complementary))
        verror("The subset is empty. Please choose a different fraction value.");

    size_t subset_size = (size_t)(ids.size() * fraction + .5);

    if ((!subset_size && !complementary) || (subset_size == ids.size() && complementary))
        verror("The subset is empty. Please choose a different fraction value.");

    clear_ids_subset();
    m_ids_subset_src = src;
    m_ids_subset_fraction = fraction;
    m_ids_subset_complementary = complementary;

    for (size_t i = 0; i < subset_size; ++i) {
        size_t idx = (size_t)(unif_rand() * (ids.size() - subset_size));

        if (!complementary)
            m_ids_subset.insert(ids[idx]);

        swap(ids[idx], ids[ids.size() - i - 1]);
    }

    if (complementary) {
        for (vector<unsigned>::const_iterator iid = ids.begin(); iid != ids.end() - subset_size; ++iid)
            m_ids_subset.insert(*iid);
    }
}

void EMRDb::update_track_cache(bool is_user_dir)
{
    string filename = (is_user_dir ? m_urootdir : m_grootdir) + "/" + CACHE_FILENAME;
    char tmp_filename[PATH_MAX];

    snprintf(tmp_filename, sizeof(tmp_filename), "%sXXXXXX", filename.c_str());
    int fd = mkstemp(tmp_filename);

    if (fd == -1) {
        vwarning("Failed to create file %s: %s", tmp_filename, strerror(errno));
        return;
    }

    for (const auto &name2track : m_tracks) {
        if (name2track.second.is_global == !is_user_dir &&
            (write(fd, name2track.first.c_str(), name2track.first.size() + 1) == -1 ||
             write(fd, &name2track.second.timestamp, sizeof(name2track.second.timestamp)) == -1))
        {
            close(fd);
            unlink(tmp_filename);
            verror("Failed to write file %s: %s", tmp_filename, strerror(errno));
        }
    }

    fchmod(fd, 0666);
    close(fd);
    if (rename(tmp_filename, filename.c_str()) == -1) {
        unlink(tmp_filename);
        verror("Failed to rename file %s to %s:\n%s", tmp_filename, filename.c_str(), strerror(errno));
    }
}

void EMRDb::clear_ids()
{
    if (m_shmem_ids != MAP_FAILED)
        munmap(m_shmem_ids, m_shmem_ids_size);
    m_shmem_ids = MAP_FAILED;

    m_ids_session_ts = 0;
    m_ids = NULL;
    m_num_ids = 0;
    m_id2idx.clear();
}

void EMRDb::load_ids()
{
    int fd = -1;

    try {
        string filename = m_grootdir + "/" + IDS_FILENAME;
        struct stat sb;

        while (1) {
            clear_ids();

            if ((fd = open(filename.c_str(), O_RDONLY, 0)) == -1) {
                if (errno != ENOENT)
                    verror("Opening file %s: %s", filename.c_str(), strerror(errno));
                create_ids_file();
                continue;
            }

            struct flock fl;
            memset(&fl, 0, sizeof(fl));
            fl.l_type = F_RDLCK;

            while (fcntl(fd, F_SETLKW, &fl) == -1) {
                if (errno != EINTR)
                    verror("Locking file %s: %s", filename.c_str(), strerror(errno));
            }

            if (fstat(fd, &sb) == -1)
                verror("stat failed on file %s: %s", filename.c_str(), strerror(errno));

            m_shmem_ids_size = sb.st_size;

            if (!m_shmem_ids_size) {
                vwarning("File %s is empty, rebuilding it", filename.c_str());
                create_ids_file();
                continue;
            }

            if ((m_shmem_ids = mmap(NULL, m_shmem_ids_size, PROT_READ, MAP_PRIVATE | MAP_POPULATE, fd, 0)) == MAP_FAILED)
                verror("mmap failed on file %s: %s", filename, strerror(errno));

            close(fd);
            fd = -1;

            time_t timestamp;

            if (m_shmem_ids_size < sizeof(int) + sizeof(timestamp) ||
                (m_shmem_ids_size - sizeof(int) - sizeof(timestamp)) % sizeof(unsigned) ||
                *(int *)m_shmem_ids != IDS_SIGNATURE)
            {
                vwarning("Invalid format of %s file, rebuilding it (%d)", filename.c_str());
                create_ids_file();
                continue;
            }
            memcpy(&timestamp, (char *)m_shmem_ids + sizeof(int), sizeof(timestamp));

            struct stat fs;
            if (stat((m_grootdir + "/dob" + TRACK_FILE_EXT).c_str(), &fs) == -1) {
                if (errno == ENOENT)
                    verror("Failed to retrieve ids: 'dob' track is missing");
                verror("Failed to stat 'dob' track: %s", strerror(errno));
            }

            if (timestamp != fs.st_mtim.tv_sec) {
                // remove an outdated version of dob track from the memory
                // (it is there if the session has already accessed dob track in the past)
                Name2Track::iterator itrack = m_tracks.find("dob");
                if (itrack != m_tracks.end() && itrack->second.track && fs.st_mtim.tv_sec != itrack->second.track->timestamp()) {
                    delete itrack->second.track;
                    itrack->second.track = NULL;
                }

                vdebug("'dob' track had been updated, rebuilding %s file", filename.c_str());
                create_ids_file();
                continue;
            }

            m_ids = (unsigned *)((char *)m_shmem_ids + sizeof(int) + sizeof(timestamp));
            m_num_ids = (m_shmem_ids_size - sizeof(int) - sizeof(timestamp)) / sizeof(unsigned);
            m_ids_session_ts = g_session_id;

            for (size_t i = 0; i < m_num_ids; ++i)
                m_id2idx[m_ids[i]] = i;

            break;
        }
    }
    catch (...) {
        if (fd != -1)
            close(fd);
        clear_ids();
        throw;
    }
}

void EMRDb::create_ids_file()
{
    int fd = -1;

    try {
        string filename = m_grootdir + "/" + IDS_FILENAME;
        fd = creat(filename.c_str(), S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH); // rwrwrw

        struct flock fl;
        memset(&fl, 0, sizeof(fl));
        fl.l_type = F_WRLCK;
    	while (fcntl(fd, F_SETLKW, &fl) == -1) {
    		if (errno != EINTR)
    			verror("Locking file %s: %s", filename.c_str(), strerror(errno));
    	}

        Name2Track::iterator itrack = m_tracks.find("dob");
        if (itrack == m_tracks.end())
             verror("Cannot retrieve ids: 'dob' track is missing");

        if (!itrack->second.is_global)
            verror("Cannot retrieve ids: 'dob' track is not in the global space");

        EMRTrack *dob = track("dob");
        time_t timestamp = dob->timestamp();
        vector<unsigned> ids;
        dob->ids(ids);

        if (write(fd, &IDS_SIGNATURE, sizeof(IDS_SIGNATURE)) != sizeof(IDS_SIGNATURE) ||
            write(fd, &timestamp, sizeof(timestamp)) != sizeof(timestamp) ||
            write(fd, &ids.front(), sizeof(unsigned) * ids.size()) != (int64_t)(sizeof(unsigned) * ids.size()))
            verror("Failed to write file %s: %s", filename.c_str(), strerror(errno));
    } catch (...) {
        close(fd);  // closing fd will also release the lock (see: fcntl, F_SETLKW)
        throw;
    }
    close(fd);
}
