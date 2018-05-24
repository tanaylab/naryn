\name{emr_vtrack.create}
\alias{emr_vtrack.create}
\title{
Creates a new virtual track
}
\description{
Creates a new virtual track.
}
\usage{
emr_vtrack.create(vtrack, src, func = NULL, params = NULL,
                  keepref = F, time.shift = NULL, id.map = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vtrack}{virtual track name}
  \item{src}{source (track name)}
  \item{func, params}{see below}
  \item{keepref}{see below}
  \item{time.shift}{time shift and expansion for iterator time}
  \item{id.map}{id mapping}
}
\details{
This function creates a new virtual track named 'vtrack'.

During the evaluation of track expression that contains a virtual track 'vtrack' the iterator point of id-time (ID1, Time, Ref) form is transformed first to an id-time interval: (ID2, Time1, Time2, Ref).

If 'id.map' is 'NULL' then ID1 == ID2, otherwise ID2 is derived from the translation table provided in 'id.map'. This table is a data frame with two first columns named 'id1' and 'id2', where 'id1' is mapped to 'id2'. If 'id.map' contains also a third optional column named 'time.shift' the value V of this column is used to shift the time accordingly, i.e. Time1 = Time2 = Time + V.
 
'time.shift' parameter (not to be confused with 'time.shift' column of 'id.map') can be either a single number X, in which case Time1 = Time2 = Time + X. Alternatively  'time.shift' can be a vector of two numbers, i.e. 'c(X1, X2)', which would result in Time1 = Time + X1, Time2 = Time + X2.

Both 'time.shift' parameter and 'time.shift' column within 'id.map' may be used simultaneously. In this case the time shifts are applied sequentially.

At the next step the values from 'src' that fall into the new id-time interval are collected. If 'keepref' is 'TRUE' the reference of these values must match 'Ref' unless either the reference or 'Ref' are '-1'.

Function 'func' (with 'params') is applied then on the collected values and produces a single value which is considered to be the value of 'vtrack' for the given iterator point.

Use the following table for a reference of all valid functions and parameters combinations.

CATEGORIAL TRACKS

\tabular{lll}{
    FUNC \tab PARAM \tab DESCRIPTION \cr
    value \tab vals/NULL \tab A track value or -1 if there is more than one. \cr
    exists \tab vals \tab 1 if any of the 'vals' exist otherwise 0. \cr
    sample \tab vals/NULL \tab uniformly sample a track value. \cr
    frequent \tab vals/NULL \tab The most frequent track value or -1 if there is more than one value. \cr
    size \tab vals/NULL \tab Number of values. \cr
    earliest \tab vals/NULL \tab Earliest value or -1 if there is more than one. \cr
    latest \tab vals/NULL \tab Latest value or -1 if there is more than one. \cr
    closest \tab vals/NULL \tab Values closest to the middle of the interval or -1 if there is more than one. \cr
    earliest.time \tab vals/NULL \tab Time of the earliest value. \cr
    latest.time \tab vals/NULL \tab Time of the latest value. \cr
    closest.earlier.time \tab vals/NULL \tab Time of the of the earlier of the closest values. \cr
    closest.later.time \tab vals/NULL \tab Time of the of the later of the closest values. \cr
    dt1.earliest \tab vals/NULL \tab Time difference between the earliest value and T1 \cr
    dt1.latest \tab vals/NULL \tab Time difference between the latest value and T1 \cr
    dt2.earliest \tab vals/NULL \tab Time difference between T2 and the earliest value \cr
    dt2.latest \tab vals/NULL \tab Time difference between T2 and the latest value \cr
}

* 'vals' is a vector of values. If not 'NULL' it serves as a filter: the function is applied only to the track values that appear among 'vals'.

QUANTITATIVE TRACKS

\tabular{lll}{
    FUNC \tab PARAM \tab DESCRIPTION \cr
    avg \tab NULL \tab Average of all values. \cr
    min \tab NULL \tab Minimal value. \cr
    max \tab NULL \tab Maximal value. \cr
    sample \tab NULL \tab uniformly sample a track value. \cr
    size \tab NULL \tab Number of values. \cr
    earliest \tab NULL \tab Average of the earliest values. \cr
    latest \tab NULL \tab Average of the latest values. \cr
    closest \tab NULL \tab Average of values closest to the middle of the interval. \cr
    stddev \tab NULL \tab Unbiased standard deviation of the values. \cr
    sum \tab NULL \tab Sum of values. \cr
    quantile \tab Percentile in the range of [0, 1] \tab Quantile of the values. \cr
    percentile.upper \tab NULL \tab Average of upper-bound values percentiles.* \cr
    percentile.upper.min \tab NULL \tab Minimum of upper-bound values percentiles.* \cr
    percentile.upper.max \tab NULL \tab Maximum of upper-bound values percentiles.* \cr
    percentile.lower \tab NULL \tab Average of lower-bound values percentiles.* \cr
    percentile.lower.min \tab NULL \tab Minimum of lower-bound values percentiles.* \cr
    percentile.lower.max \tab NULL \tab Maximum of lower-bound values percentiles.* \cr
    lm.intercept \tab NULL \tab Intercept (aka "alpha") of the simple linear regression (X = time, Y = values)\cr
    lm.slope \tab NULL \tab Slope (aka "beta") of the simple linear regression (X = time, Y = values)\cr
    earliest.time \tab NULL \tab Time of the earliest value. \cr
    latest.time \tab NULL \tab Time of the latest value. \cr
    closest.earlier.time \tab NULL \tab Time of the of the earlier of the closest values. \cr
    closest.later.time \tab NULL \tab Time of the of the later of the closest values. \cr
    dt1.earliest \tab NULL \tab Time difference between the earliest value and T1 \cr
    dt1.latest \tab NULL \tab Time difference between the latest value and T1 \cr
    dt2.earliest \tab NULL \tab Time difference between T2 and the earliest value \cr
    dt2.latest \tab NULL \tab Time difference between T2 and the latest value \cr
}

* Percentile is calculated based on the values of the whole track even if a subset is defined.

Note: 'time.shift' can be used only when 'keepref' is 'FALSE'. Also when 'keepref' is 'TRUE' only 'avg', 'percentile.upper' and 'percentile.lower' can be used in 'func'.

}
\value{
None.
}

\seealso{
\code{\link{emr_vtrack.ls}}, \code{\link{emr_vtrack.exists}}, \code{\link{emr_vtrack.rm}}
}
\examples{
emr_db.init_examples()

emr_vtrack.create("vtrack1", "dense_track", time.shift = 1,
                  func = "max")
emr_vtrack.create("vtrack2", "dense_track",
                  time.shift = c(-5, 10), func = "min")
emr_extract("dense_track", "vtrack1", "vtrack2", keepref = T)
}
\keyword{ ~virtual }
