% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extract.R
\name{emr_extract}
\alias{emr_extract}
\title{Returns evaluated track expression}
\usage{
emr_extract(
  expr,
  tidy = FALSE,
  sort = FALSE,
  names = NULL,
  stime = NULL,
  etime = NULL,
  iterator = NULL,
  keepref = FALSE,
  filter = NULL
)
}
\arguments{
\item{expr}{vector of track expressions}

\item{tidy}{if 'TRUE' result is returned in "tidy"" format}

\item{sort}{if 'TRUE' result is sorted by id, time and reference}

\item{names}{names for the track expressions in the returned value. If
'NULL' names are set to the track expression themselves.}

\item{stime}{start time scope}

\item{etime}{end time scope}

\item{iterator}{track expression iterator. If 'NULL' iterator is determined
implicitly based on track expressions. See also 'iterator' section.}

\item{keepref}{If 'TRUE' references are preserved in the iterator.}

\item{filter}{Iterator filter.}
}
\value{
A set of ID-Time points with additional columns depending on the
value of 'tidy' (see above).
}
\description{
Returns the result of track expressions evaluation for each of the iterator
points.
}
\details{
This function returns the result of track expressions evaluation for each of
the iterator stops.

If 'tidy' is 'TRUE' the returned value is a set of ID-Time points with two
additional columns named 'expr' and 'value'. 'expr' marks the track
expression that produced the value. Rows with NaN values are omitted from
the tidy format.

If 'tidy' is 'FALSE' the returned value is a set of ID-Time points with an
additional column for the values of each of the track expressions.

If 'sort' is 'TRUE' the returned value is sorted by id, time and reference,
otherwise the order is not guaranteed especially for longer runs, when
multitasking might be launched. Sorting requires additional time, so it is
switched off by default.

'names' parameter sets the labels for the track expressions in the return
value. If 'names' is 'NULL' the labels are set to the track expression
themselves.
}
\section{iterator}{


There are a few types of iterators:
\describe{
 \item{Track iterator: }{Track iterator returns the points (including the reference) from the specified track. Track name is specified as a string. If `keepref=FALSE` the reference of each point is set to `-1` \cr
Example: \cr \cr
# Returns the level of glucose one hour after the insulin shot was made \cr
emr_vtrack.create("glucose", "glucose_track", func="avg", time.shift=1) \cr
emr_extract("glucose", iterator="insulin_shot_track") \cr
}
 \item{Id-Time Points Iterator: }{Id-Time points iterator generates points from an *id-time points table*. If `keepref=FALSE` the reference of each point is set to `-1`. \cr
Example: \cr \cr
# Returns the level of glucose one hour after the insulin shot was made \cr
emr_vtrack.create("glucose", "glucose_track", func = "avg", time.shift = 1) \cr
r <- emr_extract("insulin_shot_track") # <-- implicit iterator is used here \cr
emr_extract("glucose", iterator = r) \cr
}
 \item{Ids Iterator: }{Ids iterator generates points with ids taken from an *ids table* and times that run from `stime` to `etime` with a step of 1. If `keepref=TRUE` for each id-time pair the iterator generates 255 points with references running from `0` to `254`. If `keepref=FALSE` only one point is generated for the given id and time, and its reference is set to `-1`.\cr
Example: \cr \cr
stime <- emr_date2time(1, 1, 2016, 0) \cr
etime <- emr_date2time(31, 12, 2016, 23) \cr
emr_extract("glucose", iterator = data.frame(id = c(2, 5)), stime = stime, etime = etime)\cr
}
\item{Time Intervals Iterator: }{*Time intervals iterator* generates points for all the ids that appear in 'patients.dob' track with times taken from a *time intervals table* (see: Appendix). Each time starts at the beginning of the time interval and runs to the end of it with a step of 1. That being said the points that lie outside of `[stime, etime]` range are skipped. \cr
If `keepref=TRUE` for each id-time pair the iterator generates 255 points with references running from `0` to `254`. If `keepref=FALSE` only one point is generated for the given id and time, and its reference is set to `-1`. \cr
Example: \cr
# Returns the level of hangover for all patients the next day after New Year Eve for the years 2015 and 2016 \cr
stime1 <- emr_date2time(1, 1, 2015, 0) \cr
etime1 <- emr_date2time(1, 1, 2015, 23) \cr
stime2 <- emr_date2time(1, 1, 2016, 0) \cr
etime2 <- emr_date2time(1, 1, 2016, 23) \cr
emr_extract("alcohol_level_track", iterator = data.frame( \cr
    stime = c(stime1, stime2), \cr
    etime = c(etime1, etime2) \cr
)) \cr
}
\item{Id-Time Intervals Iterator: }{*Id-Time intervals iterator* generates for each id points that cover `['stime', 'etime']` time range as specified in *id-time intervals table* (see: Appendix). Each time starts at the beginning of the time interval and runs to the end of it with a step of 1. That being said the points that lie outside of `[stime, etime]` range are skipped. \cr
If `keepref=TRUE` for each id-time pair the iterator generates 255 points with references running from `0` to `254`. If `keepref=FALSE` only one point is generated for the given id and time, and its reference is set to `-1`}
\item{Beat Iterator: }{*Beat Iterator* generates a "time beat" at the given period for each id that appear in 'patients.dob' track. The period is given always in hours. \cr
Example: \cr
emr_extract("glucose_track", iterator=10, stime=1000, etime=2000) \cr
This will create a beat iterator with a period of 10 hours starting at `stime` up until `etime` is reached. If, for example, `stime` equals `1000` then the beat iterator will create for each id iterator points at times: 1000, 1010, 1020, ... \cr
If `keepref=TRUE` for each id-time pair the iterator generates 255 points with references running from `0` to `254`. If `keepref=FALSE` only one point is generated for the given id and time, and its reference is set to `-1`.
}
\item{Extended Beat Iterator: }{*Extended beat iterator* is as its name suggests a variation on the beat iterator. It works by the same principle of creating time points with the given period however instead of basing the times count on `stime` it accepts an additional parameter - a track or a *Id-Time Points table* - that instructs what should be the initial time point for each of the ids. The two parameters (period and mapping) should come in a list. Each id is required to appear only once and if a certain id does not appear at all, it is skipped by the iterator. \cr
Anyhow points that lie outside of `[stime, etime]` range are not generated. \cr
Example: \cr
# Returns the maximal weight of patients at one year span starting from their birthdays \cr
emr_vtrack.create("weight", "weight_track", func = "max", time.shift = c(0, year())) \cr
emr_extract("weight", iterator = list(year(), "birthday_track"), stime = 1000, etime = 2000) \cr
}
\item{Periodic Iterator: }{periodic iterator goes over every year/month. You can use it by running  \code{emr_monthly_iterator} or \code{emr_yearly_iterator}. \cr
Example: \cr
iter <- emr_yearly_iterator(emr_date2time(1, 1, 2002), emr_date2time(1, 1, 2017)) \cr
emr_extract("dense_track", iterator = iter, stime = 1, etime = 3) \cr
iter <- emr_monthly_iterator(emr_date2time(1, 1, 2002), n = 15) \cr
emr_extract("dense_track", iterator = iter, stime = 1, etime = 3) \cr
}
\item{Implicit Iterator: }{The iterator is set implicitly if its value remains `NULL` (which is the default). In that case the track expression is analyzed and searched for track names. If all the track variables or virtual track variables point to the same track, this track is used as a source for a track iterator. If more then one track appears in the track expression, an error message is printed out notifying ambiguity.}
}


Revealing Current Iterator Time:
During the evaluation of a track expression one can access a specially defined variable named `EMR_TIME` (Python: `TIME`). This variable contains a vector (`numpy.ndarray` in Python) of current iterator times. The length of the vector matches the length of the track variable (which is a vector too). \cr
Note that some values in `EMR_TIME` might be set 0. Skip those intervals and the values of the track variables at the corresponding indices. \cr
# Returns times of the current iterator as a day of month \cr
emr_extract("emr_time2dayofmonth(EMR_TIME)", iterator = "sparse_track") \cr
}

\examples{

emr_db.init_examples()
emr_extract("dense_track", stime = 1, etime = 3)
}
\seealso{
\code{\link{emr_screen}}
}
\keyword{~extract}
