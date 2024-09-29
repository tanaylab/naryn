#include "EMRDb.h"
#include "BinsManager.h"
#include "naryn.h"

BinsManager::BinsManager(SEXP _breaks, SEXP _include_lowest, SEXP _right, const vector<string> *exprs, const NRTrackExpressionVars *expr_vars)
{
	if (!Rf_isVector(_breaks)){
		TGLError<BinsManager>("'breaks' argument must be a vector");
    }

	if (!Rf_isLogical(_include_lowest) || Rf_length(_include_lowest) != 1){
		TGLError<BinsManager>("'include.lowest' argument is not logical");
    }

    if (!Rf_isLogical(_right) || Rf_length(_right) != 1){
        TGLError<BinsManager>("'right' argument is not logical");
    }

	unsigned num_breaks_sets = Rf_length(_breaks);

	m_include_lowest = Rf_asLogical(_include_lowest);
    m_right = Rf_asLogical(_right);
	m_bin_finders.reserve(num_breaks_sets);
	m_track_mult.resize(num_breaks_sets);
    m_tracks.resize(num_breaks_sets, NULL);
	m_totalbins = 1;

	for (unsigned i = 0; i < num_breaks_sets; ++i) {
		SEXP breaks = VECTOR_ELT(_breaks, i);

		if ((expr_vars && !Rf_isNull(breaks) && !Rf_isReal(breaks) && !Rf_isInteger(breaks)) || (!expr_vars && !Rf_isReal(breaks) && !Rf_isInteger(breaks))){
			TGLError<BinsManager>("breaks[%d] is not numeric", i + 1);
        }

		m_bin_finders.push_back(BinFinder());

		if (Rf_isInteger(breaks)) {
			vector<double> double_breaks(Rf_length(breaks));

			for (int i = 0; i < Rf_length(breaks); i++)
				double_breaks[i] = INTEGER(breaks)[i];
			m_bin_finders.back().init(double_breaks, m_include_lowest, m_right);
		} else if (Rf_isReal(breaks))
			m_bin_finders.back().init(REAL(breaks), Rf_length(breaks), m_include_lowest, m_right);
        else {  // breaks is NULL
            const NRTrackExpressionVars::TrackVar *var = expr_vars->var((*exprs)[i].c_str());

            if (!var)
                TGLError<BinsManager>("breaks[%d]: breaks can be NULL only when the track expression is a track or a virtual track", i + 1);

            EMRTrack *track = g_db->track(var->imanager->name.c_str());

            if (!track->is_categorical())
                TGLError<BinsManager>("breaks[%d]: breaks can be NULL only when the underyling track is categorical", i + 1);

            if (var->imanager->data_fetcher.func() != EMRTrack::VALUE &&
                var->imanager->data_fetcher.func() != EMRTrack::FREQUENT &&
                var->imanager->data_fetcher.func() != EMRTrack::SAMPLE &&
                var->imanager->data_fetcher.func() != EMRTrack::SAMPLE_TIME &&
                var->imanager->data_fetcher.func() != EMRTrack::EARLIEST &&
                var->imanager->data_fetcher.func() != EMRTrack::LATEST &&
                var->imanager->data_fetcher.func() != EMRTrack::CLOSEST)
            {
                TGLError<BinsManager>("breaks[%d]: breaks cannot be NULL when virtual track function is %s",
                                      i + 1, EMRTrack::FUNC_INFOS[var->imanager->data_fetcher.func()].name);
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
            rprotect(dimname = RSaneAllocVector(STRSXP, vals.size()));
            rprotect(breaks = RSaneAllocVector(INTSXP, vals.size()));

            for (vector<double>::const_iterator ival = vals.begin(); ival < vals.end(); ++ival) {
                char buf[100];
                snprintf(buf, sizeof(buf), "%d", (int)*ival);
                SET_STRING_ELT(dimname, ival - vals.begin(), Rf_mkChar(buf));
                INTEGER(breaks)[ival - vals.begin()] = (int)*ival;
            }
        } else {
    		const BinFinder &bin_finder = m_bin_finders[i];
    		int numbins = bin_finder.get_numbins();
    		INTEGER(dim)[i] = numbins;
    		rprotect(dimname = RSaneAllocVector(STRSXP, numbins));
            rprotect(breaks = RSaneAllocVector(REALSXP, numbins + 1));

    		for (int j = 0; j < numbins; j++) {
    			char buf[100];
                if (m_right) {
                    snprintf(buf, sizeof(buf), "%c%g,%g]", j || !m_include_lowest ? '(' : '[', bin_finder.get_breaks()[j], bin_finder.get_breaks()[j + 1]);
                } else {
                    snprintf(buf, sizeof(buf), "[%g,%g%c", bin_finder.get_breaks()[j], bin_finder.get_breaks()[j + 1], j != numbins - 1 || !m_include_lowest ? ')' : ']');
                }
    			SET_STRING_ELT(dimname, j, Rf_mkChar(buf));
                REAL(breaks)[j] = bin_finder.get_breaks()[j];
    		}
            REAL(breaks)[numbins] = bin_finder.get_breaks()[numbins];
        }
		SET_VECTOR_ELT(dimnames, i, dimname);
        SET_VECTOR_ELT(breaks_set, i, breaks);
	}
}

