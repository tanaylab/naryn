#include <dirent.h>
#include <limits.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>

#include "naryn.h"
#include "EMRDb.h"
#include "EMRProgressReporter.h"
#include "EMRTrack.h"

const string  EMRDb::TRACK_FILE_EXT(".nrtrack");
const char   *EMRDb::CACHE_FILENAME = ".naryn-cache";

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

    m_grootdir = "";
    m_urootdir = "";
    m_minid = numeric_limits<unsigned>::max();
    m_maxid = 0;
    m_mintime = EMRTimeStamp::MAX_HOUR;
    m_maxtime = 0;

    for (int i = 0; i < 2; ++i) {
        m_dir_minid[i] = numeric_limits<unsigned>::max();
        m_dir_maxid[i] = 0;
        m_dir_mintime[i] = EMRTimeStamp::MAX_HOUR;
        m_dir_maxtime[i] = 0;
    }
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
            unsigned dir_minid;
            unsigned dir_maxid;
            unsigned dir_mintime;
            unsigned dir_maxtime;

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

                bf.read(&dir_minid, sizeof(dir_minid));
                bf.read(&dir_maxid, sizeof(dir_maxid));
                bf.read(&dir_mintime, sizeof(dir_mintime));
                bf.read(&dir_maxtime, sizeof(dir_maxtime));

                if (bf.eof()) {
                    vwarning("Invalid format of file %s, rewriting it", filename);
                    continue;
                }

                int c;
                int pos = 0;
                char track_name[PATH_MAX];
                vector<string> &track_names = is_user_dir ? m_user_track_names : m_global_track_names;
                unordered_set<string> _track_names;

                while ((c = bf.getc()) != EOF) {
                    track_name[pos++] = c;

                    if ((c && pos >= (int)sizeof(track_name)) || (!c && !_track_names.insert(string(track_name)).second)) {
                        vwarning("Invalid format of file %s, rewriting it", filename);
                        break;
                    } else if (!c)
                        pos = 0;
                }

                if (c == EOF) {
                    if (bf.error())
                        vwarning("Failed to read file %s: %s", filename, strerror(errno));
                    else {
                        track_names.reserve(_track_names.size());
                        m_track_names.reserve(m_track_names.size() + _track_names.size());
                        for (unordered_set<string>::const_iterator itrack_name = _track_names.begin(); itrack_name != _track_names.end(); ++itrack_name) {
                            if (m_tracks.find(*itrack_name) != m_tracks.end())
                                verror("Track %s appears both in global and user directories", itrack_name->c_str());

                            snprintf(filename, sizeof(filename), "%s/%s%s", dirnames[is_user_dir], itrack_name->c_str(), TRACK_FILE_EXT.c_str());
                            m_tracks.insert(pair<string, TrackInfo>(*itrack_name, TrackInfo(NULL, filename, !is_user_dir)));
                            m_track_names.push_back(*itrack_name);
                            track_names.push_back(*itrack_name);
                        }

                        m_dir_minid[is_user_dir] = dir_minid;
                        m_dir_maxid[is_user_dir] = dir_maxid;
                        m_dir_mintime[is_user_dir] = dir_mintime;
                        m_dir_maxtime[is_user_dir] = dir_maxtime;

                        m_minid = min(m_minid, dir_minid);
                        m_maxid = max(m_maxid, dir_maxid);
                        m_mintime = min(m_mintime, dir_mintime);
                        m_maxtime = max(m_maxtime, dir_maxtime);

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
                m_tracks.insert(pair<string, TrackInfo>(track_name, TrackInfo(track, filename, !is_user_dir)));
    			m_track_names.push_back(track_name);
                track_names.push_back(track_name);
                m_dir_minid[is_user_dir] = min(m_dir_minid[is_user_dir], track->minid());
                m_dir_maxid[is_user_dir] = max(m_dir_maxid[is_user_dir], track->maxid());
                m_dir_mintime[is_user_dir] = min(m_dir_mintime[is_user_dir], track->mintime());
                m_dir_maxtime[is_user_dir] = max(m_dir_maxtime[is_user_dir], track->maxtime());

                check_interrupt();
    			progress.report(1);
    		}

            m_minid = min(m_minid, m_dir_minid[is_user_dir]);
            m_maxid = max(m_maxid, m_dir_maxid[is_user_dir]);
            m_mintime = min(m_mintime, m_dir_mintime[is_user_dir]);
            m_maxtime = max(m_maxtime, m_dir_maxtime[is_user_dir]);

            update_track_cache(is_user_dir);
         }

        if (m_minid > m_maxid)
            m_minid = m_maxid = 0;
        if (m_mintime > m_maxtime)
            m_mintime = m_maxtime = 0;

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
    m_tracks.insert(pair<string, TrackInfo>(track_name, TrackInfo(track, filename.c_str(), is_global)));
    m_dir_minid[!is_global] = min(m_dir_minid[!is_global], track->minid());
    m_dir_maxid[!is_global] = max(m_dir_maxid[!is_global], track->maxid());
    m_dir_mintime[!is_global] = min(m_dir_mintime[!is_global], track->mintime());
    m_dir_maxtime[!is_global] = max(m_dir_maxtime[!is_global], track->maxtime());
    m_minid = min(m_minid, track->minid());
    m_maxid = max(m_maxid, track->maxid());
    m_mintime = min(m_mintime, track->mintime());
    m_maxtime = max(m_maxtime, track->maxtime());

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

    m_dir_minid[!is_global] = numeric_limits<unsigned>::max();
    m_dir_maxid[!is_global] = 0;
    m_dir_mintime[!is_global] = EMRTimeStamp::MAX_HOUR;
    m_dir_maxtime[!is_global] = 0;

    for (itrack = m_tracks.begin(); itrack != m_tracks.end(); ++itrack) {
        if (itrack->second.is_global == is_global) {
            track(itrack->first);    // causes the track to be loaded if it is not loaded yet
            m_dir_minid[!is_global] = min(m_dir_minid[!is_global], itrack->second.track->minid());
            m_dir_maxid[!is_global] = max(m_dir_maxid[!is_global], itrack->second.track->maxid());
            m_dir_mintime[!is_global] = min(m_dir_mintime[!is_global], itrack->second.track->mintime());
            m_dir_maxtime[!is_global] = max(m_dir_maxtime[!is_global], itrack->second.track->maxtime());
        }
    }

    m_minid = min(m_dir_minid[0], m_dir_minid[1]);
    m_maxid = max(m_dir_maxid[0], m_dir_maxid[1]);
    m_mintime = min(m_dir_mintime[0], m_dir_mintime[1]);
    m_maxtime = max(m_dir_maxtime[0], m_dir_maxtime[1]);

    if (m_minid > m_maxid)
        m_minid = m_maxid = 0;
    if (m_mintime > m_maxtime)
        m_mintime = m_maxtime = 0;

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
    BufferedFile bf;

    if (bf.open(filename.c_str(), "w"))
        vwarning("Failed to create file %s: %s", filename.c_str(), strerror(errno));

    bf.write(&m_dir_minid[is_user_dir], sizeof(m_dir_minid[is_user_dir]));
    bf.write(&m_dir_maxid[is_user_dir], sizeof(m_dir_maxid[is_user_dir]));
    bf.write(&m_dir_mintime[is_user_dir], sizeof(m_dir_mintime[is_user_dir]));
    bf.write(&m_dir_maxtime[is_user_dir], sizeof(m_dir_maxtime[is_user_dir]));

    vector<string> &track_names = is_user_dir ? m_user_track_names : m_global_track_names;
    for (vector<string>::const_iterator itrack_name = track_names.begin(); itrack_name != track_names.end(); ++itrack_name)
        bf.write(itrack_name->c_str(), itrack_name->size() + 1);

    if (bf.error())
        vwarning("Failed to write to %s: %s", filename.c_str(), strerror(errno));
}

