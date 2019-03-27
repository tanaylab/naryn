\name{emr_summary}
\alias{emr_summary}
\title{
Calculates summary statistics of track expression
}
\description{
Calculates summary statistics of track expression.
}
\usage{
emr_summary(expr, stime = NULL, etime = NULL, iterator = NULL,
            keepref = F, filter = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{expr}{track expression.}
  \item{stime}{start time scope.}
  \item{etime}{end time scope.}
  \item{iterator}{track expression iterator. If 'NULL' iterator is determined implicitly based on track expressions.}
  \item{keepref}{If 'TRUE' references are preserved in the iterator.}
  \item{filter}{Iterator filter.}
}
\details{
This function returns summary statistics of a track expression: total number of values, number of NaN values, min, max, sum, mean and standard deviation of the values.
}
\value{
An array that represents summary statistics.
}

\seealso{
\code{\link{emr_track.info}}
}
\examples{
emr_db.init_examples()
emr_summary("sparse_track")
}
\keyword{ ~summary }
\keyword{ ~statistics }
