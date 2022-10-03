#include <cmath>

#include "naryn.h"
#include "NRTrackExpressionScanner.h"
#include "StreamSampler.h"
#include "StreamPercentiler.h"

#include <R.h>
#include <Rinternals.h>

#ifdef length
#undef length
#endif
#ifdef error
#undef error
#endif

struct Percentile {
	double    percentile;
	int64_t   index;
	bool      estimation;

	Percentile() {}
	Percentile(double _percentile, int64_t _index) : percentile(_percentile), index(_index) {}

	bool operator<(const Percentile &p) const { return percentile < p.percentile; }
};

bool calc_medians(StreamPercentiler<double> &sp, vector<Percentile> &percentiles, vector<double> &medians)
{
	bool estimated_results = false;

	if (sp.stream_size()) {
		for (vector<Percentile>::iterator ip = percentiles.begin(); ip != percentiles.end(); ++ip) {
			medians[ip->index] = sp.get_percentile(ip->percentile, ip->estimation);
			if (ip->estimation)
				estimated_results = true;
		}

		for (vector<Percentile>::iterator ip = percentiles.begin() + 1; ip != percentiles.end(); ++ip) {
			if (ip->estimation)
				medians[ip->index] = max(medians[ip->index], medians[(ip - 1)->index]);
		}
		for (vector<Percentile>::reverse_iterator ip = percentiles.rbegin() + 1; ip != percentiles.rend(); ++ip) {
			if (ip->estimation)
				medians[ip->index] = min(medians[ip->index], medians[(ip - 1)->index]);
		}
	} else {
		for (vector<Percentile>::iterator ip = percentiles.begin(); ip != percentiles.end(); ++ip)
			medians[ip->index] = numeric_limits<double>::quiet_NaN();
	}
	return estimated_results;
}

extern "C" {

SEXP C_emr_quantiles(SEXP _expr, SEXP _percentiles, SEXP _stime, SEXP _etime, SEXP _iterator_policy, SEXP _keepref, SEXP _filter, SEXP _envir)
{
	try {
        Naryn naryn(_envir);

		if (!isString(_expr) || Rf_length(_expr) != 1)
			verror("Track argument is not a string");

		if (!isReal(_percentiles) || Rf_length(_percentiles) < 1)
			verror("Percentile argument is not a vector of numbers");

		vector<Percentile> percentiles(Rf_length(_percentiles));
		for (int64_t i = 0; i < Rf_length(_percentiles); ++i)
			percentiles[i] = Percentile(REAL(_percentiles)[i], i);
		sort(percentiles.begin(), percentiles.end());

		for (vector<Percentile>::const_iterator ip = percentiles.begin(); ip != percentiles.end(); ++ip) {
			if (ip->percentile < 0 || ip->percentile > 1)
				verror("Percentile (%g) is not in [0, 1] range\n", ip->percentile);
		}

        NRTrackExprScanner scanner;
		StreamPercentiler<double> sp(g_naryn->max_data_size(), g_naryn->quantile_edge_data_size(), g_naryn->quantile_edge_data_size());

        for (scanner.begin(_expr, NRTrackExprScanner::REAL_T, _stime, _etime, _iterator_policy, _keepref, _filter); !scanner.isend(); scanner.next()) {
			float val = scanner.real();

			if (!std::isnan(val))
				sp.add(val, unif_rand);
		}

		vector<double> medians(percentiles.size(), numeric_limits<float>::quiet_NaN());

		if (calc_medians(sp, percentiles, medians))
			vwarning("Data size (%ld) exceeds the limit (%ld).\n"
					"The data was sampled to fit the limit and the resulted quantiles are hence approximate.\n"
					"(The limit can be controlled by gmax.data.size limit)", sp.stream_size(), g_naryn->max_data_size());

		// assemble the answer
		SEXP answer;
		SEXP colnames;

		rprotect(answer = RSaneAllocVector(REALSXP, percentiles.size()));
		rprotect(colnames = RSaneAllocVector(STRSXP, percentiles.size()));

		for (vector<Percentile>::const_iterator ip = percentiles.begin(); ip != percentiles.end(); ++ip) {
			char buf[100];

			REAL(answer)[ip->index] = medians[ip->index];

			sprintf(buf, "%g", ip->percentile);
			SET_STRING_ELT(colnames, ip->index, mkChar(buf));
		}

		setAttrib(answer, R_NamesSymbol, colnames);

		rreturn(answer);
	} catch (TGLException &e) {
		rerror("%s", e.msg());
    } catch (const bad_alloc &e) {
        rerror("Out of memory");
    }
	rreturn(R_NilValue);
}

}
