#include <dirent.h>
#include <limits.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <iostream>

#include "EMRDb.h"
#include "EMRProgressReporter.h"
#include "EMRTimesIterator.h"
#include "EMRTrack.h"
#include "EMRTrackIterator.h"
#include "naryn.h"
#include "NRIteratorFilter.h"
#include "NRTimeInterval.h"
#include "strutil.h"

#include <R_ext/BLAS.h>
#include <R_ext/Parse.h>

typedef unordered_map<int, EMRTrackData<float> *> Datasets;

extern "C" {

SEXP test_parse_expr(SEXP r_expr, SEXP envir) {
    try {
        Naryn naryn(envir);
        string expr = CHAR(STRING_ELT(r_expr, 0));
        vector<string> vars;
        get_expression_vars(expr, vars);        
        for (size_t i = 0; i < vars.size(); ++i) {            
            vdebug(vars[i].c_str());
        }
        return R_NilValue;
    } catch (TGLException &e) {
        rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

    return R_NilValue;
}

SEXP logical_track_vtrack(SEXP _track, SEXP envir){
    try {
        Naryn naryn(envir);        
        const char *trackname = CHAR(STRING_ELT(_track, 0));        
        const EMRLogicalTrack *ltrack = g_db->logical_track(trackname);
        
        if (!ltrack) verror("Track %s does not exist", trackname);
        
        return (ltrack->vtrack());

    } catch (TGLException &e) {
        rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

    return R_NilValue;
}

SEXP netta_bug(SEXP envir) {
    try {
        Naryn naryn(envir);

        const int NUM_TRACKS = 2;
        EMRTrackData<float> data[NUM_TRACKS];

        data[0].add(5918, EMRTimeStamp(1194898, 227), 1);
        data[0].add(5918, EMRTimeStamp(1197420, 228), 1);
        data[0].add(5920, EMRTimeStamp(1223183, 229), 11);
        data[0].add(5920, EMRTimeStamp(1296408, 230), 10);
        data[0].add(5921, EMRTimeStamp(1223183, 2), 11);
        data[0].add(5921, EMRTimeStamp(1296408, 3), 10);

        data[1].add(5918, EMRTimeStamp(1194898, 227), 1);
        data[1].add(5918, EMRTimeStamp(1197420, 228), 1);
        data[1].add(5920, EMRTimeStamp(1223183, 229), 11);
        data[1].add(5920, EMRTimeStamp(1296408, 230), 10);
        data[1].add(7000, EMRTimeStamp(1223183, 2), 11);
        data[1].add(7000, EMRTimeStamp(1296408, 3), 10);
        data[1].add(8000, EMRTimeStamp(1223183, 0), 11);
        data[1].add(8000, EMRTimeStamp(1296408, 1), 10);

        string filename[NUM_TRACKS];

        filename[0] = g_db->grootdir() + "/netta1" + EMRDb::TRACK_FILE_EXT;
        filename[1] = g_db->grootdir() + "/netta2" + EMRDb::TRACK_FILE_EXT;

        for (int i = 0; i < NUM_TRACKS; ++i) {
            EMRTrack::serialize(filename[i].c_str(), true, data[i]);
            REprintf("Track %s created...\n", filename[i].c_str());
        }
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	return R_NilValue;
}

SEXP nrtest_track_create(SEXP envir)
{
	try {
		Naryn naryn(envir);

        const int NUM_TRACKS = 4;
		EMRTrackData<double> data[NUM_TRACKS];

		data[0].add(25, EMRTimeStamp(1, 0), 10);
        data[0].add(25, EMRTimeStamp(2, 0), 20);
        data[0].add(25, EMRTimeStamp(2, 2), 22);
        data[0].add(25, EMRTimeStamp(2, 8), 28);
        data[0].add(25, EMRTimeStamp(2, 4), 24);
        data[0].add(25, EMRTimeStamp(2, 6), 26);
        data[0].add(25, EMRTimeStamp(3, 4), 34);
        data[0].add(25, EMRTimeStamp(6, 0), 60);
        data[0].add(25, EMRTimeStamp(6, 2), 62);
        data[0].add(25, EMRTimeStamp(8, 1), 80);
        data[0].add(25, EMRTimeStamp(8, 4), 84);
        data[0].add(25, EMRTimeStamp(9, 2), 92);
        data[0].add(25, EMRTimeStamp(9, 4), 94);
        data[0].add(25, EMRTimeStamp(10, 4), 104);
        data[0].add(25, EMRTimeStamp(12, 4), 124);
        data[0].add(27, EMRTimeStamp(23, 4), 234);
		data[0].add(33, EMRTimeStamp(23, 4), 234);
        data[0].add(33, EMRTimeStamp(50, 0), 500);
        data[0].add(5, EMRTimeStamp(1, 3), 13);
        data[0].add(10, EMRTimeStamp(1, 3), 13);
        data[0].add(40, EMRTimeStamp(1, 3), 13);

        data[1].add(25, EMRTimeStamp(1, 0), 10);
        data[1].add(25, EMRTimeStamp(2, 0), 20);
        data[1].add(25, EMRTimeStamp(2, 2), 22);
        data[1].add(25, EMRTimeStamp(2, 8), 28);
        data[1].add(25, EMRTimeStamp(2, 5), 24);
        data[1].add(25, EMRTimeStamp(2, 6), 26);
        data[1].add(25, EMRTimeStamp(3, 4), 34);
        data[1].add(25, EMRTimeStamp(6, 0), 60);
        data[1].add(25, EMRTimeStamp(6, 2), 62);
        data[1].add(25, EMRTimeStamp(8, 1), 80);
        data[1].add(25, EMRTimeStamp(8, 4), 84);
        data[1].add(25, EMRTimeStamp(9, 2), 92);
        data[1].add(25, EMRTimeStamp(9, 4), 94);
        data[1].add(25, EMRTimeStamp(10, 4), 104);
        data[1].add(25, EMRTimeStamp(12, 4), 124);
        data[1].add(27, EMRTimeStamp(23, 4), 234);
        data[1].add(27, EMRTimeStamp(50, 0), 500);
        data[1].add(24, EMRTimeStamp(1, 3), 13);
        data[1].add(22, EMRTimeStamp(1, 3), 13);
        data[1].add(28, EMRTimeStamp(1, 3), 13);

        data[2].add(24, EMRTimeStamp(1, 0), 2);
        data[2].add(24, EMRTimeStamp(3, 0), 3);
        data[2].add(24, EMRTimeStamp(3, 2), 2);
        data[2].add(25, EMRTimeStamp(2, 2), 1);
        data[2].add(25, EMRTimeStamp(2, 6), 4);
        data[2].add(25, EMRTimeStamp(6, 0), 1);
        data[2].add(25, EMRTimeStamp(6, 2), 0);
        data[2].add(27, EMRTimeStamp(20, 0), 4);
        data[2].add(27, EMRTimeStamp(23, 4), 3);
        data[2].add(27, EMRTimeStamp(25, 1), 2);
        data[2].add(28, EMRTimeStamp(1, 3), 0);

        data[3].add(24, EMRTimeStamp(1, 0), 5);
        data[3].add(24, EMRTimeStamp(3, 0), 2);
        data[3].add(24, EMRTimeStamp(3, 2), 1);
        data[3].add(25, EMRTimeStamp(2, 2), 0);
        data[3].add(25, EMRTimeStamp(2, 6), 4);
        data[3].add(25, EMRTimeStamp(6, 0), 3);
        data[3].add(25, EMRTimeStamp(6, 2), 2);
        data[3].add(27, EMRTimeStamp(20, 0), 1);
        data[3].add(27, EMRTimeStamp(23, 4), 0);
        data[3].add(27, EMRTimeStamp(25, 1), 2);
        data[3].add(28, EMRTimeStamp(1, 3), 4);

		string filename[NUM_TRACKS];
        bool is_categorical[NUM_TRACKS] = { false, false, true, true };

		filename[0] = g_db->grootdir() + "/sparse_track" + EMRDb::TRACK_FILE_EXT;
		filename[1] = g_db->grootdir() + "/dense_track" + EMRDb::TRACK_FILE_EXT;
        filename[2] = g_db->grootdir() + "/categorical_track" + EMRDb::TRACK_FILE_EXT;
        filename[3] = g_db->grootdir() + "/categorical_track2" + EMRDb::TRACK_FILE_EXT;

		for (int i = 0; i < NUM_TRACKS; ++i) {
			EMRTrack::serialize(filename[i].c_str(), is_categorical[i], data[i]);
			REprintf("Track %s created...\n", filename[i].c_str());
		}
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	return R_NilValue;
}

SEXP nrtest_regressiondb_create(SEXP envir)
{
	try {
		Naryn naryn(envir);

        if (g_db->urootdir().empty())
            verror("User space dir is not set");

        const int NUM_TRACKS = 11;

        size_t num_vals[NUM_TRACKS] = { 100000L, 500000L, 2500000L, 100L, 500L, 2500L, 400L, 100000L, 2500000L, 100L, 200L };
        bool is_categorical[NUM_TRACKS] = { false, false, false, false, false, false, true, true, true, false, true };
        bool is_global[NUM_TRACKS] = { true, true, true, true, true, true, true, true, true, false, false };
        size_t max_patients = 1000L;
        size_t max_time = 10000L;
        size_t max_val = 1000L;
        size_t max_val_categorical = 10L;

        for (int itrack = 0; itrack < NUM_TRACKS; itrack++) {
            EMRTrackData<float> data;

            for (size_t ival = 0; ival < num_vals[itrack]; ++ival) {
                float val = is_categorical[itrack] ? (unsigned)(unif_rand() * max_val_categorical) : (unsigned)(unif_rand() * max_val);
                unsigned id = (unsigned)(unif_rand() * max_patients);
                unsigned hour = (unsigned)(unif_rand() * max_time);
                
                for (int ref = 0; ref < EMRTimeStamp::MAX_REFCOUNT; ++ref) {
                    try {
                        data.add(id, EMRTimeStamp(hour, ref), val);
                        break;
                    } catch (...) {}
                }
            }

            char filename[1000];

            sprintf(filename, "%s/track%d%s", is_global[itrack] ? g_db->grootdir().c_str() : g_db->urootdir().c_str(), itrack, EMRDb::TRACK_FILE_EXT.c_str());
            EMRTrack::TrackType track_type = EMRTrack::serialize(filename, is_categorical[itrack], data);
            REprintf("Track %s created (%s)...\n", filename, EMRTrack::TRACK_TYPE_NAMES[track_type]);

            // if dense track is created create another one in sparse format
            if (track_type == EMRTrack::DENSE) {
                // sparse is created when density falls beyond the limit
                unsigned id = (unsigned)(max_patients / EMRTrack::DENSE_TRACK_MIN_DENSITY) + 10;
                float val = (unsigned)(unif_rand() * max_val);
                unsigned hour = (unsigned)(unif_rand() * max_time);

                data.add(id, EMRTimeStamp(hour, 0), val);
                sprintf(filename, "%s/track%d_sparse%s", g_db->grootdir().c_str(), itrack, EMRDb::TRACK_FILE_EXT.c_str());
                EMRTrack::TrackType track_type = EMRTrack::serialize(filename, is_categorical[itrack], data);
                REprintf("Track %s created (%s)...\n", filename, EMRTrack::TRACK_TYPE_NAMES[track_type]);
            }
        }
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	return R_NilValue;
}

SEXP nrtest_vtrack(SEXP _track, SEXP envir)
{
	try {
		Naryn naryn(envir);

        const char *trackname = CHAR(STRING_ELT(_track, 0));
        EMRTrack *t = g_db->track(trackname);
        if (!t)
            verror("Track %s does not exist", trackname);

        EMRTrack::DataFetcher df;
        EMRInterval interv(0, 0, 0, 0);

        REprintf("Track loaded %s... Type: %s, Data: %s\n", t->name(), EMRTrack::TRACK_TYPE_NAMES[t->track_type()], EMRTrack::DATA_TYPE_NAMES[t->data_type()]);

        EMRTrackData<double> data_recs;
        int f = -1;
        while (1) {
            char buf[1000];
            double percentile;

            REprintf("Enter function name or an interval as \"pid stime etime ref\" or print or func or quit: ");
            if (scanf("%s", buf) != 1)
                continue;

            if (!strcmp(buf, "quit"))
                break;

            if (!strcmp(buf, "print")) {
                t->data_recs(data_recs);
                REprintf("Num patients: %ld\n", data_recs.data.size());
                for (EMRTrackData<double>::DataRecs::const_iterator ipr = data_recs.data.begin(); ipr != data_recs.data.end(); ++ipr)
                    REprintf("Patient %d, time %s, val %g\n", ipr->id, ipr->timestamp.tostr().c_str(), ipr->val);
                continue;
            }

            if (!strcmp(buf, "func")) {
                for (int i = 0; i < EMRTrack::NUM_FUNCS; ++i)
                    REprintf("%s ", EMRTrack::FUNC_INFOS[i].name);
                REprintf("\n\n");
                continue;
            }

            int i;
            for (i = 0; i < EMRTrack::NUM_FUNCS; ++i) {
                if (!strcmp(EMRTrack::FUNC_INFOS[i].name, buf)) {
                    f = i;
                    if (f == EMRTrack::QUANTILE) {
                        REprintf("Enter percentile: ");
                        if (scanf("%lf", &percentile) != 1)
                            continue;
                    }
                    REprintf("Function %s was defined\n", EMRTrack::FUNC_INFOS[f].name);
                    break;
                }
            }

            if (i < EMRTrack::NUM_FUNCS) {
                df.init(t, false, unordered_set<double>());
                df.register_function((EMRTrack::Func)f);
                interv.init(0, 0, 0, 0);
            } else if (f == -1)
                REprintf("Function must be defined first\n");
            else {
                char *endptr;
                int pid, stime, etime, ref;
                pid = strtol(buf, &endptr, 10);

                if (*endptr) {
                    REprintf("Invalid command\n");
                    continue;
                }

                if (scanf("%d %d %d", &stime, &etime, &ref) != 3)
                    continue;

                if (pid < (int)interv.id) {
                    REprintf("New pid < old pid\n");
                    continue;
                }
                if (pid == (int)interv.id && stime < interv.stime) {
                    REprintf("New stime < old stime\n");
                    continue;
                }
                if (stime > etime) {
                    REprintf("stime > etime\n");
                    continue;
                }
                interv.init(pid, stime, etime, ref);
                df.set_vals(interv);

                if ((EMRTrack::Func)f == EMRTrack::QUANTILE)
                    REprintf("Res: %g\n", df.quantile(percentile));
                else
                    REprintf("Res: %g\n", df.val());
            }
        }
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	return R_NilValue;
}

SEXP nrtest_iterator(SEXP envir)
{
	try {
		Naryn naryn(envir);

		EMRTrackData<double> data[2];

		data[0].add(25, EMRTimeStamp(1, 0), 10);
        data[0].add(25, EMRTimeStamp(2, 0), 20);
        data[0].add(25, EMRTimeStamp(2, 2), 22);
        data[0].add(25, EMRTimeStamp(2, 8), 28);
        data[0].add(25, EMRTimeStamp(2, 4), 24);
        data[0].add(25, EMRTimeStamp(2, 6), 26);
        data[0].add(25, EMRTimeStamp(3, 4), 34);
        data[0].add(25, EMRTimeStamp(6, 0), 60);
        data[0].add(25, EMRTimeStamp(6, 2), 62);
        data[0].add(25, EMRTimeStamp(8, 1), 80);
        data[0].add(25, EMRTimeStamp(8, 4), 84);
        data[0].add(25, EMRTimeStamp(9, 2), 92);
        data[0].add(25, EMRTimeStamp(9, 4), 94);
        data[0].add(25, EMRTimeStamp(10, 4), 104);
        data[0].add(25, EMRTimeStamp(12, 4), 124);
		data[0].add(33, EMRTimeStamp(23, 4), 234);
        data[0].add(33, EMRTimeStamp(50, 0), 500);
        data[0].add(5, EMRTimeStamp(1, 3), 13);
        data[0].add(10, EMRTimeStamp(1, 3), 13);
        data[0].add(40, EMRTimeStamp(1, 3), 13);

        data[1].add(25, EMRTimeStamp(1, 0), 10);
        data[1].add(25, EMRTimeStamp(2, 0), 20);
        data[1].add(25, EMRTimeStamp(2, 2), 22);
        data[1].add(25, EMRTimeStamp(2, 8), 28);
        data[1].add(25, EMRTimeStamp(2, 4), 24);
        data[1].add(25, EMRTimeStamp(2, 6), 26);
        data[1].add(25, EMRTimeStamp(3, 4), 34);
        data[1].add(25, EMRTimeStamp(6, 0), 60);
        data[1].add(25, EMRTimeStamp(6, 2), 62);
        data[1].add(25, EMRTimeStamp(8, 1), 80);
        data[1].add(25, EMRTimeStamp(8, 4), 84);
        data[1].add(25, EMRTimeStamp(9, 2), 92);
        data[1].add(25, EMRTimeStamp(9, 4), 94);
        data[1].add(25, EMRTimeStamp(10, 4), 104);
        data[1].add(25, EMRTimeStamp(12, 4), 124);
        data[1].add(27, EMRTimeStamp(23, 4), 234);
        data[1].add(27, EMRTimeStamp(50, 0), 500);
        data[1].add(24, EMRTimeStamp(1, 3), 13);
        data[1].add(22, EMRTimeStamp(1, 3), 13);
        data[1].add(28, EMRTimeStamp(1, 3), 13);

		string filename[2];
        string trackname[2];

        trackname[0] = "sparse_track";
        trackname[1] = "dense_track";

        filename[0] = g_db->grootdir() + "/" + trackname[0] + EMRDb::TRACK_FILE_EXT;
        filename[1] = g_db->grootdir() + "/" + trackname[1] + EMRDb::TRACK_FILE_EXT;

        EMRTrack *t[2];

		for (int i = 0; i < 2; ++i) {
			EMRTrack::serialize(filename[i].c_str(), false, data[i]);
			REprintf("Track %s created...\n", filename[i].c_str());
			t[i] = EMRTrack::unserialize(trackname[i].c_str(), filename[i].c_str());
		}

		EMRTrackData<double> data_recs;
        EMRTrackIterator itr;

        int idx = -1;
        while (1) {
            // char buf[1000];
            // double percentile;

            REprintf("Enter index: ");
            if (scanf("%d", &idx) != 1)
                continue;

			if (idx < 0 || idx >= 2)
				break;

			t[idx]->data_recs(data_recs);
			REprintf("Num patients: %ld\n", data_recs.data.size());
			for (EMRTrackData<double>::DataRecs::const_iterator ipr = data_recs.data.begin(); ipr != data_recs.data.end(); ++ipr)
				REprintf("Patient %d, time %s, val %g\n", ipr->id, ipr->timestamp.tostr().c_str(), ipr->val);

			REprintf("Enter time scope and keep_ref (0/1): ");
			int stime, etime, keep_ref;
			if (scanf("%d%d%d", &stime, &etime, &keep_ref) != 3)
                continue;

            itr.init(t[idx], keep_ref, stime, etime, unordered_set<double>());
			for (itr.begin(); !itr.isend(); itr.next()) {
				REprintf("pid: %d, timestamp: %s\n", itr.point().id, itr.point().timestamp.tostr().c_str());
			}
        }
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	return R_NilValue;
}

SEXP nrtrack(SEXP _track, SEXP _envir)
{
	try {
		Naryn naryn(_envir);

		const char *trackname = CHAR(STRING_ELT(_track, 0));
		EMRTrack *track = g_db->track(trackname);
		EMRTrackData<double> data_recs;

		if (!track) 
			verror("Track %s not found", trackname);

		track->data_recs(data_recs);
		for (EMRTrackData<double>::DataRecs::const_iterator ipr = data_recs.data.begin(); ipr != data_recs.data.end(); ++ipr)
			REprintf("Patient %d, time %s, val %g\n", ipr->id, ipr->timestamp.tostr().c_str(), ipr->val);
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	return R_NilValue;
}

SEXP nrimport_clalit(SEXP _dirname, SEXP _envir)
{
	// DIR *dir = NULL;
	// Datasets datasets;

	// try {
	// 	Naryn naryn(_envir);

	// 	const char *dirname = CHAR(STRING_ELT(_dirname, 0));
	// 	const string FEXTENSION(".csv");

	// 	dir = opendir(dirname);
	// 	struct dirent *dirp;

	// 	if (!dir)
	// 		verror("Failed to open directory %s: %s", dirname, strerror(errno));

	// 	srand48(0);

	// 	while ((dirp = readdir(dir))) {
	// 		char filename[PATH_MAX + 100];
	// 		struct stat s;
	// 		int len = strlen(dirp->d_name);

	// 		sprintf(filename, "%s/%s", dirname, dirp->d_name);
	// 		if (stat(filename, &s))
	// 			verror("Failed to stat file %s: %s", filename, strerror(errno));

	// 		// is it a normal file having the supported extension?
	// 		if (S_ISREG(s.st_mode) && len > (int)FEXTENSION.size() && !strncmp(dirp->d_name + len - (int)FEXTENSION.size(), FEXTENSION.c_str(), FEXTENSION.size())) {
	// 			enum { PATIENTID, TESTCODE, DATE, RESULT, NUM_FIELDS };

	// 			BufferedFile bfile;
	// 			vector<string> fields;
	// 			int lineno = 0;

	// 			REprintf("Reading %s\n", filename);
	// 			if (bfile.open(filename, "r"))
	// 				verror("Failed to open file %s for reading: %s", filename, strerror(errno));

	// 			while (1) {
	// 				char *endptr;
	// 				unsigned patientid;
	// 				unsigned testcode;
	// 				EMRTimeStamp timestamp;
	// 				float res;

	// 				check_interrupt();

	// 				lineno += split_line(bfile, fields, ',', NUM_FIELDS);

	// 				if (fields.size() != NUM_FIELDS) {
	// 					if (bfile.eof()) 
	// 						break;

	// 					if (bfile.error()) 
	// 						verror("Reading file %s: %s", filename, strerror(errno));

	// 					verror("File %s has invalid format (0)", filename);
	// 				}

	// 				patientid = (unsigned)strtod(fields[PATIENTID].c_str(), &endptr);
	// 				if (*endptr) {
	// 					// if it's a header => skip it
	// 					if (lineno == 1) 
	// 						continue;
	// 					verror("File %s has invalid format (line %d)", filename, lineno);
	// 				}

	// 				testcode = strtol(fields[TESTCODE].c_str(), &endptr, 10);
	// 				if (*endptr) 
	// 					verror("File %s has invalid format (2)", filename);

	// 				res = strtod(fields[RESULT].c_str(), &endptr);
	// 				if (*endptr) 
	// 					verror("File %s has invalid format (3)", filename);

	// 				for (unsigned char refcount = 0; ; ++refcount) {
	// 					// for now put random timestamp
	// 					timestamp.init((unsigned)(unif_rand() * 1000000), refcount);

	// 					if (datasets.find(testcode) == datasets.end())
	// 						datasets[testcode] = new EMRTrackData<float>();
	// 					try {
	// 						datasets[testcode]->add(patientid, timestamp, res);
	// 						break;
	// 					} catch (...) {
	// 						// if exception is thrown => record already exists => increase reference count
	// 					}
	// 				}
	// 			}
	// 		}
	// 	}

	// 	closedir(dir);

	// 	for (Datasets::iterator idataset = datasets.begin(); idataset != datasets.end(); ++idataset) {
	// 		char filename[PATH_MAX + 100];

	// 		REprintf("Writing track %d\n", idataset->first);
	// 		sprintf(filename, "%s/t%d%s", g_db->grootdir().c_str(), idataset->first, EMRDb::TRACK_FILE_EXT.c_str());
	// 		EMRTrack::serialize(filename, false, *idataset->second);
	// 	}

	// 	for (Datasets::iterator idataset = datasets.begin(); idataset != datasets.end(); ++idataset) 
	// 		delete idataset->second;
	// } catch (TGLException &e) {
	// 	for (Datasets::iterator idataset = datasets.begin(); idataset != datasets.end(); ++idataset) 
	// 		delete idataset->second;
	// 	rerror("%s", e.msg());
    // } catch (const bad_alloc &e) {
    //     rerror("Out of memory");
    // }

	// return R_NilValue;
}

// void print_tree(SEXP tree, int depth)
// {
//     bool is_op = true;
//     bool is_function = false;

//     while (1) {
//         SEXP data = CAR(tree);
//         if (isLanguage(data)) {
//             print_tree(data, depth + 1);
//         } else {
//             const char *str = CHAR(asChar(data));
//             if (is_op && strcmp(str, "&") && strcmp(str, "|") && strcmp(str, "!") && strcmp(str, "(")) {
//                 REprintf("FUNCTION\n");
//                 is_function = true;

//                 for (int i = 0; i < depth; ++i)
//                     REprintf("  ");
//                 REprintf("%s\n", str);

//                 while (1) {
//                     tree = CDR(tree);
//                     if (isNull(tree))
//                         break;
//                     data = CAR(tree);
//                     SEXP res = eval_in_R(data, g_naryn->env());
//                     if (isReal(res)) {
//                         for (int i = 0; i < Rf_length(res); ++i) {
//                             for (int j = 0; j < depth; ++j)
//                                 REprintf("  ");
//                             REprintf("REAL %g\n", REAL(res)[i]);
//                         }
//                     } else if (isLogical(res)) {
//                         for (int i = 0; i < Rf_length(res); ++i) {
//                             for (int j = 0; j < depth; ++j)
//                                 REprintf("  ");
//                             REprintf("LOGICAL %d\n", LOGICAL(res)[i]);
//                         }
//                     } else
//                         verror("Error in eval");
//                 }
//             } else {
//                 for (int i = 0; i < depth; ++i)
//                     REprintf("  ");
//                 REprintf("%s\n", str);
//             }
//             is_op = false;
//         }
//         tree = CDR(tree);
//         if (isNull(tree))
//             break;
//     }
// }

// do.call(f, list(substitute(a+2)))
// string to expression: eval(parse(text="substitute(a+2)"))

// SEXP nrtest_substitute(SEXP _expr, SEXP _envir)
// {
// 	try {
// 		Naryn naryn(_envir);

//         if (isLanguage(_expr))
//             print_tree(_expr, 0);
//         else
//             REprintf("PLAIN %s\n", CHAR(asChar(_expr)));
// 	} catch (TGLException &e) {
// 		rerror("%s", e.msg());
//     } catch (const bad_alloc &e) {
//         rerror("Out of memory");
//     }

// 	return R_NilValue;
// }

SEXP nrfilter(SEXP _expr, SEXP _stime, SEXP _etime, SEXP _envir)
{
	try {
		Naryn naryn(_envir);

        NRIteratorFilter filter;
        filter.init(_expr, asInteger(_stime), asInteger(_etime));
        filter.debug_print();

        while (1) {
            REprintf("Enter iterator point (id, time, ref) or quit: ");
            char buf[1000];
            if (!fgets(buf, sizeof(buf), stdin))
                continue;
            int id;
            int time;
            int ref;
            int retv;
            buf[strlen(buf) - 1] = '\0';
            retv = sscanf(buf, "%d %d %d", &id, &time, &ref);
            if (retv == 2 || retv == 3) {
                if (retv == 2)
                    ref = -1;

                REprintf("Filtering....\n");
                EMRPoint point(id, EMRTimeStamp(time, (unsigned char)ref));
                if (filter.is_passed(point))
                    REprintf("PASSED\n");
                else
                    REprintf("NOT PASSED, next point: %s\n", filter.jumpto().tostr().c_str());
            } else {
                if (!strcmp(buf, "quit"))
                    break;
            }
        }
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	return R_NilValue;
}

SEXP nrtest_time_iterator(SEXP _times, SEXP _stime, SEXP _etime, SEXP _keepref, SEXP _envir)
{
	try {
		Naryn naryn(_envir);

        unsigned stime = asInteger(_stime);
        unsigned etime = asInteger(_etime);
        EMRTimeIntervals intervs;
        NRTimeIntervals::convert_rtime_intervals(_times, &intervs);

        EMRTimesIterator itr;

        itr.init(intervs, asLogical(_keepref), stime, etime);
        itr.begin();

        while (1) {
            REprintf("Current point %s\n", itr.point().tostr().c_str());
            REprintf("Enter iterator point (id, time, ref) or quit: ");
            char buf[1000];
            if (!fgets(buf, sizeof(buf), stdin))
                continue;
            int id;
            int time;
            int ref;
            int retv;
            buf[strlen(buf) - 1] = '\0';
            retv = sscanf(buf, "%d %d %d", &id, &time, &ref);
            if (retv == 2 || retv == 3) {
                if (retv == 2)
                    ref = -1;

                itr.next(EMRPoint(id, EMRTimeStamp(time, -1)));
                if (itr.isend()) {
                    REprintf("End\n");
                    break;
                }
            } else {
                if (!strcmp(buf, "quit"))
                    break;
            }
        }
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	return R_NilValue;
}

SEXP emr_test_pipe(SEXP _num_processes, SEXP _timeout, SEXP _envir)
{
	try {
		Naryn naryn(_envir);

        int num_processes = asInteger(_num_processes);

        Naryn::prepare4multitasking();
        for (int i = 0; i < num_processes; ++i) {
            if (!g_naryn->launch_process()) { // kid process
                if (g_naryn->debug()){
                    SEXP rvar = GetOption(install("emr_child_run_delay"), R_NilValue);
                    if (isReal(rvar) || isInteger(rvar))
                        sleep(asInteger(rvar));
                }

                char buf[1000];

                while (1)
                    Naryn::write_multitask_fifo(buf, sizeof(buf));
            }
            vdebug("Launched child process %d/%d\n", i + 1, num_processes);
        }

        char buf[100];
        size_t bytes_read = 0;

        vdebug("Starting read test\n");
        size_t timeout = asInteger(_timeout);
        Naryn::set_alarm(timeout * 1000);
        while (1) {
            if (Naryn::read_multitask_fifo(buf, sizeof(buf)) == EOF)
                verror("Broken pipe");

            bytes_read += sizeof(buf);

            if (Naryn::alarm_fired())
                break;

            check_interrupt();
        }

        vdebug("End\n");
        REprintf("Received: %ld bytes, rate: %ld bytes / sec\n", bytes_read, bytes_read / timeout);
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	rreturn(R_NilValue);
}

SEXP emr_test_eval(SEXP _expr, SEXP _n, SEXP _envir)
{
	try {
		Naryn naryn(_envir);

        if (!isString(_expr) || Rf_length(_expr) != 1)
            verror("'expr' argument must be a string");

        if ((!isInteger(_n) && !isReal(_n)) || Rf_length(_n) != 1)
            verror("'n' argument must be an integer value");

        // const char *expr_str = { CHAR(asChar(_expr)) };
        int n = asInteger(_n);

        SEXP aaa, bbb;
        rprotect(aaa = RSaneAllocVector(REALSXP, 1));
        rprotect(bbb = RSaneAllocVector(REALSXP, 1));
        defineVar(install("aaa"), aaa, g_naryn->env());
        defineVar(install("bbb"), bbb, g_naryn->env());

        // parse R expression
        ParseStatus status;
        SEXP parsed_expr;
        rprotect(parsed_expr = R_ParseVector(_expr, -1, &status, R_NilValue));
        if (status != PARSE_OK)
            verror("R parsing failed");
        SEXP eval_expr = VECTOR_ELT(parsed_expr, 0);
        for (int i = 0; i < n; ++i) {
            REAL(aaa)[0] = i;
            REAL(bbb)[0] = i + 1;
            SEXP res = eval_in_R(eval_expr, g_naryn->env());
            // int lres = REAL(res)[0];
//            REprintf("res: %g\n", REAL(res)[0]);
            runprotect(res);
        }
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }

	rreturn(R_NilValue);
}

}

