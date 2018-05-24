\name{emr_screen}
\alias{emr_screen}
\title{
Finds patient-time pairs that match track expression
}
\description{
Finds all patient-time pairs where track expression is 'TRUE'.
}
\usage{
emr_screen(expr, sort = F, stime = MINTIME, etime = MAXTIME,
           iterator = NULL, keepref = F, filter = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{expr}{logical track expression}
  \item{sort}{if 'TRUE' result is sorted by id, time and reference}
  \item{stime}{start time scope}
  \item{etime}{end time scope}
  \item{iterator}{track expression iterator. If 'NULL' iterator is determined implicitly based on track expression.}
  \item{keepref}{If 'TRUE' references are preserved in the iterator.}
  \item{filter}{Iterator filter.}
}
\details{
This function finds all patient-time pairs where track expression's value is 'TRUE'.

If 'sort' is 'TRUE' the returned value is sorted by id, time and reference, otherwise the order is not guaranteed especially for longer runs, when multitasking might be launched. Sorting requires additional time, so it is switched off by default.
}
\value{
A set of patient-time pairs that match track expression.
}

\seealso{
\code{\link{emr_extract}}
}
\examples{
emr_db.init_examples()
emr_screen("sparse_track == 13 | dense_track < 80",
           iterator = "sparse_track", keepref = T)
}
\keyword{ ~screen }

