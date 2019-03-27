\name{emr_dist}
\alias{emr_dist}
\title{
Calculates distribution of track expressions
}
\description{
Calculates distribution of track expressions' values over the given set of bins. 
}
\usage{
emr_dist([expr, breaks]+, include.lowest = F, right = T,
         stime = NULL, etime = NULL, iterator = NULL, keepref = F,
         filter = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{expr}{track expression}
  \item{breaks}{breaks that determine the bin or 'NULL'}
  \item{include.lowest}{if 'TRUE', the lowest (or highest, for ‘right = FALSE’) value of the range determined by breaks is included}
  \item{right}{if 'TRUE' the intervals are closed on the right (and open on the left), otherwise vice versa.}
  \item{stime}{start time scope}
  \item{etime}{end time scope}
  \item{iterator}{track expression iterator. If 'NULL' iterator is determined implicitly based on track expressions.}
  \item{keepref}{If 'TRUE' references are preserved in the iterator.}
  \item{filter}{Iterator filter.}
}
\details{
This function calculates the distribution of values of the numeric track expressions over the given set of bins.

The range of bins is determined by 'breaks' argument. For example: 'breaks=c(x1, x2, x3, x4)' represents three different intervals (bins): (x1, x2], (x2, x3], (x3, x4].

If the track expression constitutes of a categorical track or a virtual track which source is a categorical track, the 'breaks' is allowed to be 'NULL' meaning that the breaks are derived implicitly from the unique values of the underlying track.

'emr_dist' can work with any number of dimensions. If more than one 'expr'-'breaks' pair is passed, the result is a multidimensional vector, and an individual value can be accessed by [i1,i2,...,iN] notation, where 'i1' is the first track and 'iN' is the last track expression.
}
\value{
N-dimensional vector where N is the number of 'expr'-'breaks' pairs.
}

\seealso{
\code{\link{emr_cor}}, \code{\link{cut}}
}
\examples{
emr_db.init_examples()
emr_dist("sparse_track", c(0, 15, 20, 30, 40, 50), keepref = T)
}
\keyword{ ~distribution }

