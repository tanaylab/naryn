#include "BinsManager.h"
#include "NRDb.h"
#include "naryn.h"

BinsManager::BinsManager(SEXP _breaks, SEXP _include_lowest, const vector<string> *exprs, const NRTrackExpressionVars *expr_vars)
{
	if (!isVector(_breaks))
		TGLError<BinsManager>("Breaks argument must be a vector");

	if (!isLogical(_include_lowest) || Rf_length(_include_lowest) != 1)
		TGLError<BinsManager>("include.lowest argument is not logical");

	unsigned num_breaks_sets = Rf_length(_breaks);

	m_include_lowest = LOGICAL(_include_lowest)[0];
	m_bin_finders.reserve(num_breaks_sets);
	m_track_mult.resize(num_breaks_sets);
    m_tracks.resize(num_breaks_sets, NULL);
	m_totalbins = 1;

	for (unsigned i = 0; i < num_breaks_sets; ++i) {
		SEXP breaks = VECTOR_ELT(_breaks, i);

		if (expr_vars && !isNull(breaks) && !isReal(breaks) && !isInteger(breaks) || !expr_vars && !isReal(breaks) && !isInteger(breaks))
			TGLError<BinsManager>("breaks[%d] is not numeric", i + 1);

		m_bin_finders.push_back(BinFinder());

		if (isInteger(breaks)) {
			vector<double> double_breaks(Rf_length(breaks));

			for (int i = 0; i < Rf_length(breaks); i++)
				double_breaks[i] = INTEGER(breaks)[i];
			m_bin_finders.back().init(double_breaks, m_include_lowest);
		} else if (isReal(breaks))
			m_bin_finders.back().init(REAL(breaks), Rf_length(breaks), m_include_lowest);
        else {  // breaks is NULL
            const NRTrackExpressionVars::TrackVar *var = expr_vars->var((*exprs)[i].c_str());

            if (!var)
                TGLError<BinsManager>("breaks[%d]: breaks can be NULL only when the track expression is a track or a virtual track", i + 1);

            NRTrack *track = g_db->track(var->imanager->name.c_str());

            if (!track->is_categorial())
                TGLError<BinsManager>("breaks[%d]: breaks can be NULL only when the underyling track is categorial", i + 1);

            if (var->imanager->data_fetcher.func() != NRTrack::VALUE &&
                var->imanager->data_fetcher.func() != NRTrack::FREQUENT &&
                var->imanager->data_fetcher.func() != NRTrack::SAMPLE &&
                var->imanager->data_fetcher.func() != NRTrack::EARLIEST &&
                var->imanager->data_fetcher.func() != NRTrack::LATEST &&
                var->imanager->data_fetcher.func() != NRTrack::CLOSEST)
            {
                TGLError<BinsManager>("breaks[%d]: breaks cannot be NULL when virtual track function is %s",
                                      i + 1, NRTrack::FUNC_INFOS[var->imanager->data_fetcher.func()].name);
            }

            vector<double> vals;

            track->unique_vals(vals);
            vals.insert(vals.begin(), track->minval() - 0.5);
            m_bin_finders.back().init(vals, false);
            m_tracks[i] = track;
        }

		m_totalbins *= m_bin_finders.back().get_numbins();
		m_track_mult[i] = !i ? 1 : m_track_mult[i - 1] * m_bin_finders[i - 1].get_numbins();
	}
}

void BinsManager::set_dims(SEXP dim, SEXP dimnames, SEXP breaks_set) const
{
	for (unsigned i = 0; i < get_num_bin_finders(); i++) {
        SEXP dimname;
        SEXP breaks;

        if (m_tracks[i]) {
            vector<double> vals;
            m_tracks[i]->unique_vals(vals);
            INTEGER(dim)[i] = vals.size();
            rprotect(dimname = allocVector(STRSXP, vals.size()));
            rprotect(breaks = allocVector(INTSXP, vals.size()));

            for (vector<double>::const_iterator ival = vals.begin(); ival < vals.end(); ++ival) {
                char buf[100];
                sprintf(buf, "%d", (int)*ival);
                SET_STRING_ELT(dimname, ival - vals.begin(), mkChar(buf));
                INTEGER(breaks)[ival - vals.begin()] = (int)*ival;
            }
        } else {
    		const BinFinder &bin_finder = m_bin_finders[i];
    		int numbins = bin_finder.get_numbins();
    		INTEGER(dim)[i] = numbins;
    		rprotect(dimname = allocVector(STRSXP, numbins));
            rprotect(breaks = allocVector(REALSXP, numbins + 1));

    		for (int j = 0; j < numbins; j++) {
    			char buf[100];
    			sprintf(buf, "%c%g,%g]", j || !m_include_lowest ? '(' : '[', bin_finder.get_breaks()[j], bin_finder.get_breaks()[j + 1]);
    			SET_STRING_ELT(dimname, j, mkChar(buf));
                REAL(breaks)[j] = bin_finder.get_breaks()[j];
    		}
            REAL(breaks)[numbins] = bin_finder.get_breaks()[numbins];
        }
		SET_VECTOR_ELT(dimnames, i, dimname);
        SET_VECTOR_ELT(breaks_set, i, breaks);
	}
}

