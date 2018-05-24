#include <cmath>

#include "naryn.h"
#include "NRTrackExpressionScanner.h"

#include <R.h>
#include <Rinternals.h>

#ifdef length
#undef length
#endif

struct IntervalSummary {
	double num_bins;
	double num_non_nan_bins;
	double total;
	double minval;
	double maxval;
	double mean_square_sum;

	double get_mean() const { return total / num_non_nan_bins; }

	double get_stdev() const {
		double mean = get_mean();
		// we are calaculating unbiased standard deviation:
		// sqrt(sum((x-mean)^2) / (N-1)) = sqrt(sum(x^2)/(N-1) - N*(mean^2)/(N-1))
		return sqrt(mean_square_sum / (num_non_nan_bins - 1) - (mean * mean) * (num_non_nan_bins / (num_non_nan_bins - 1)));
	}

	void update(double v) {
		++num_bins;
		if (!std::isnan(v)) {
			num_non_nan_bins++;
			total += v;
			minval = min(minval, v);
			maxval = max(maxval, v);
			mean_square_sum += v * v;
		}
	}

	void merge(const IntervalSummary &obj) {
		num_bins += obj.num_bins;
		num_non_nan_bins += obj.num_non_nan_bins;
		total += obj.total;
		minval = min(minval, obj.minval);
		maxval = max(maxval, obj.maxval);
		mean_square_sum += obj.mean_square_sum;
	}

	IntervalSummary() :
		num_bins(0),
		num_non_nan_bins(0),
		total(0),
		minval(numeric_limits<double>::max()),
		maxval(-numeric_limits<double>::max()),
		mean_square_sum(0) {}
};

enum IntervalSummaryCols { TOTAL_BINS, TOTAL_NAN_BINS, MIN, MAX, SUM, MEAN, STDEV, NUM_COLS };

static const char *IntervalSummaryColNames[NUM_COLS] = { "Total values", "NaN values", "Min", "Max", "Sum", "Mean", "Std dev" };

extern "C" {

SEXP emr_summary(SEXP _expr, SEXP _stime, SEXP _etime, SEXP _iterator_policy, SEXP _keepref, SEXP _filter, SEXP _envir)
{
	try {
        Naryn naryn(_envir);

		if (!isString(_expr) || Rf_length(_expr) != 1)
			verror("The value of 'expr' parameter must be a string");

        IntervalSummary summary;
		NRTrackExprScanner scanner;

		for (scanner.begin(_expr, NRTrackExprScanner::REAL_T, _stime, _etime, _iterator_policy, _keepref, _filter); !scanner.isend(); scanner.next())
            summary.update(scanner.real());

        SEXP answer;
        SEXP colnames;

        rprotect(answer = allocVector(REALSXP, NUM_COLS));
        rprotect(colnames = allocVector(STRSXP, NUM_COLS));

        REAL(answer)[TOTAL_BINS] = summary.num_bins;
        REAL(answer)[TOTAL_NAN_BINS] = summary.num_bins - summary.num_non_nan_bins;
        REAL(answer)[MIN] = summary.num_non_nan_bins ? summary.minval : numeric_limits<double>::quiet_NaN();
        REAL(answer)[MAX] = summary.num_non_nan_bins ? summary.maxval : numeric_limits<double>::quiet_NaN();
        REAL(answer)[SUM] = summary.num_non_nan_bins ? summary.total : numeric_limits<double>::quiet_NaN();
        REAL(answer)[MEAN] = summary.num_non_nan_bins ? summary.get_mean() : numeric_limits<double>::quiet_NaN();
        REAL(answer)[STDEV] = summary.num_non_nan_bins > 1 ? summary.get_stdev() : numeric_limits<double>::quiet_NaN();

        for (int i = 0; i < NUM_COLS; i++)
            SET_STRING_ELT(colnames, i, mkChar(IntervalSummaryColNames[i]));

        setAttrib(answer, R_NamesSymbol, colnames);

        rreturn(answer);
	} catch (TGLException &e) {
		rerror("%s", e.msg());
	}
	rreturn(R_NilValue);
}

}

