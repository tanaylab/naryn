#include <stdlib.h>
#include <unistd.h>

#include "EMRDb.h"
#include "EMRTrack.h"
#include "naryn.h"
#include "strutil.h"

extern "C" {

SEXP emr_import(SEXP _track, SEXP _space, SEXP _categorical, SEXP _src, SEXP _add, SEXP _envir)
{
    try {
        enum { ID, TIME, REF, VALUE, NUM_COLS };

        static const char *COL_NAMES[NUM_COLS] = { "id", "time", "ref", "value" };

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
                lineno += split_line_by_space_chars(bfile, fields, NUM_COLS);

                if (fields.size() != NUM_COLS) {
                    if (bfile.eof())
                        break;

                    if (bfile.error())
                        verror("Reading file %s: %s", filename, strerror(errno));

                    if (fields.size() >= 1 && fields[0].size() && fields[0][0] == '#')     // skip comments (lines that begin with #)
                        continue;

                    verror("%s, line %d: file has invalid format", filename, lineno);
                }

                id = strtol(fields[ID].c_str(), &endptr, 10);
                if (*endptr || id < 0)
                    verror("%s, line %d: invalid id", filename, lineno);

                hour = strtol(fields[TIME].c_str(), &endptr, 10);
                if (*endptr || hour < 0 || hour > EMRTimeStamp::MAX_HOUR)
                    verror("%s, line %d: invalid time", filename, lineno);

                ref = strtol(fields[REF].c_str(), &endptr, 10);
                if (*endptr || ref < 0 && ref != -1 || ref > EMRTimeStamp::MAX_REFCOUNT)
                    verror("%s, line %d: invalid reference", filename, lineno);

                val = strtod(fields[VALUE].c_str(), &endptr);
                if (*endptr)
                    verror("%s, line %d: invalid data format", filename, lineno);

                data.add_data(id, EMRTimeStamp((EMRTimeStamp::Hour)hour, (EMRTimeStamp::Refcount)ref), val);
            }
        } else {
            if (TYPEOF(_src) == PROMSXP) {
                if (PRENV(_src) == R_NilValue)
                    _src = PRVALUE(_src);
                else
                    _src = eval_in_R(PRCODE(_src), PRENV(_src));
            }

            SEXP colnames = getAttrib(_src, R_NamesSymbol);

            if (!isVector(_src) || !isString(colnames) || Rf_length(colnames) < NUM_COLS - 1)
                verror("Invalid format of 'src' argument");

            bool ref_used = Rf_length(colnames) > REF && !strcmp(CHAR(STRING_ELT(colnames, REF)), COL_NAMES[REF]);
            SEXP rcol[NUM_COLS];

            for (unsigned i = 0, rcolidx = 0; i < NUM_COLS; i++) {
                if (i == REF && !ref_used) {
                    rcol[REF] = R_NilValue;
                    continue;
                }

                rcol[i] = VECTOR_ELT(_src, rcolidx);

                if (strcmp(CHAR(STRING_ELT(colnames, rcolidx)), COL_NAMES[i]) || !isReal(rcol[i]) && !isInteger(rcol[i]) ||
                    rcolidx && Rf_length(VECTOR_ELT(_src, rcolidx - 1)) != Rf_length(rcol[i]))
                    verror("Invalid format of 'src' argument");

                ++rcolidx;
            }

            unsigned num_points = (unsigned)Rf_length(rcol[ID]);

            for (unsigned i = 0; i < num_points; ++i) {
                unsigned id = (unsigned)(isReal(rcol[ID]) ? REAL(rcol[ID])[i] : INTEGER(rcol[ID])[i]);
                EMRTimeStamp::Hour hour = (EMRTimeStamp::Hour)(isReal(rcol[TIME]) ? REAL(rcol[TIME])[i] : INTEGER(rcol[TIME])[i]);
                EMRTimeStamp::Refcount ref = ref_used ? (EMRTimeStamp::Refcount)(isReal(rcol[REF]) ? REAL(rcol[REF])[i] : INTEGER(rcol[REF])[i]) : EMRTimeStamp::NA_REFCOUNT;
                float val = (float)(isReal(rcol[VALUE]) ? REAL(rcol[VALUE])[i] : INTEGER(rcol[VALUE])[i]);

                data.add_data(id, EMRTimeStamp(hour, ref), val);
            }
        }

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
