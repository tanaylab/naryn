#include <stdlib.h>
#include <unistd.h>

#include "EMRDb.h"
#include "EMRTrack.h"
#include "naryn.h"
#include "NRPoint.h"
#include "strutil.h"

extern "C" {

SEXP emr_import(SEXP _track, SEXP _space, SEXP _categorical, SEXP _src, SEXP _add, SEXP _envir)
{
    try {
        Naryn naryn(_envir);

        if (!isString(_track) || Rf_length(_track) != 1)
            verror("'track' argument must be a string");

        bool do_add = asLogical(_add);
        string trackname = { CHAR(asChar(_track)) };
        string track_filename;
        bool categorical;
        bool is_global;
        EMRTrackData<float> data;

        if (do_add) {
            EMRTrackData<double>::DataRecs data_recs;
            EMRTrack *track = g_db->track(trackname);
            const EMRDb::TrackInfo *track_info = g_db->track_info(trackname);

            if (!track)
                verror("Track %s not found", trackname.c_str());

            track_filename = track_info->filename;
            categorical = track->is_categorical();
            is_global = track_info->is_global;
            track->data_recs(data_recs);

            for (EMRTrackData<double>::DataRecs::const_iterator irec = data_recs.begin(); irec != data_recs.end(); ++irec)
                data.add_data(irec->id, irec->timestamp, (float)irec->val);
        } else {
            if (!isLogical(_categorical) || Rf_length(_categorical) != 1 || asLogical(_categorical) == NA_LOGICAL)
                verror("'categorical' argument must be logical");

            categorical = asLogical(_categorical);

            if (!isString(_space) || Rf_length(_space) != 1)
                verror("'space' argument must be a string");

            string space = { CHAR(asChar(_space)) };

            if (space == "global")
                is_global = true;
            else {
                if (space == "user") {
                    if (g_db->urootdir().empty())
                        verror("User space root directory is not set");
                    is_global = false;
                } else
                    verror("Invalid value of 'space' argument");
            }

            if (g_db->track(trackname))
                verror("Track %s already exists", trackname.c_str());

            EMRDb::check_track_name(trackname);

            track_filename = (is_global ? g_db->grootdir() : g_db->urootdir()) + string("/") + trackname + EMRDb::TRACK_FILE_EXT;

            if (access(track_filename.c_str(), F_OK) != -1)
                verror("File %s already exists", track_filename.c_str());
        }

        if (isString(_src)) {
            if (Rf_length(_src) != 1)
                verror("Invalid format of 'src' argument");

            const char *filename = CHAR(asChar(_src));

            BufferedFile bfile;
            vector<string> fields;
            int lineno = 0;
            char *endptr;
            int id;
            int hour;
            int ref;
            float val;

            if (bfile.open(filename, "r"))
                verror("Failed to open file %s for reading: %s", filename, strerror(errno));

            while (1) {
                lineno += split_line_by_space_chars(bfile, fields, NRPoint::NUM_PVAL_COLS);

                if (fields.size() != NRPoint::NUM_PVAL_COLS) {
                    if (bfile.eof())
                        break;

                    if (bfile.error())
                        verror("Reading file %s: %s", filename, strerror(errno));

                    if (fields.size() >= 1 && fields[0].size() && fields[0][0] == '#')     // skip comments (lines that begin with #)
                        continue;

                    verror("%s, line %d: file has invalid format", filename, lineno);
                }

                id = strtol(fields[NRPoint::ID].c_str(), &endptr, 10);
                if (*endptr || id < 0)
                    verror("%s, line %d: invalid id", filename, lineno);

                hour = strtol(fields[NRPoint::TIME].c_str(), &endptr, 10);
                if (*endptr || hour < 0 || hour > EMRTimeStamp::MAX_HOUR)
                    verror("%s, line %d: invalid time", filename, lineno);

                ref = strtol(fields[NRPoint::REF].c_str(), &endptr, 10);
                if (*endptr || ref < 0 && ref != -1 || ref > EMRTimeStamp::MAX_REFCOUNT)
                    verror("%s, line %d: invalid reference", filename, lineno);

                val = strtod(fields[NRPoint::VALUE].c_str(), &endptr);
                if (*endptr)
                    verror("%s, line %d: invalid data format", filename, lineno);

                data.add_data(id, EMRTimeStamp((EMRTimeStamp::Hour)hour, (EMRTimeStamp::Refcount)ref), val);
            }
        } else
            NRPoint::convert_rpoints_vals(_src, data, "'src': ");

        if (access(track_filename.c_str(), F_OK) != -1) {
            string tmp_filename = track_filename + ".tmp";
            EMRTrack::serialize(tmp_filename.c_str(), categorical ? EMRTrack::IS_CATEGORICAL : 0, data);
            unlink(track_filename.c_str());
            rename(tmp_filename.c_str(), track_filename.c_str());
        } else
            EMRTrack::serialize(track_filename.c_str(), categorical, data);
        g_db->load_track(trackname.c_str(), is_global);
    } catch (TGLException &e) {
        rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

    rreturn(R_NilValue);
}

}
