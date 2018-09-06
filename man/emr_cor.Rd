\name{emr_cor}
\alias{emr_cor}
\title{
Calculates correlation statistics for pairs of track expressions
}
\description{
Calculates correlation statistics for pairs of track expressions.
}
\usage{
emr_cor([expr, breaks]+, cor.exprs, include.lowest = F, right = T,
        stime = MINTIME, etime = MAXTIME, iterator = NULL,
        keepref = F, filter = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{expr}{track expression.}
  \item{breaks}{breaks that determine the bin or 'NULL'.}
  \item{cor.exprs}{vector of track expressions for which correlation statistics is calculated.}
  \item{include.lowest}{if 'TRUE', the lowest (or highest, for ‘right = FALSE’) value of the range determined by breaks is included.}
  \item{right}{if 'TRUE' the intervals are closed on the right (and open on the left), otherwise vice versa.}
  \item{stime}{start time scope.}
  \item{etime}{end time scope.}
  \item{iterator}{track expression iterator. If 'NULL' iterator is determined implicitly based on track expressions.}
  \item{keepref}{If 'TRUE' references are preserved in the iterator.}
  \item{filter}{Iterator filter.}
}
\details{
This function works in a similar manner to 'emr_dist'. However instead of returning a single counter for each bin 'emr_cor' returns 5 matrices of 'length(cor.exprs) X length(cor.exprs)' size. Each matrix represents the correlation statistics for each pair of track expressions from 'cor.exprs'. Given a 'bin' and a pair of track expressions 'cor.exprs[i]' and 'cor.exprs[j]' the corresponding matrix contains the following information:

$n[bin,i,j]   - number of times when both 'cor.exprs[i]' and 'cor.exprs[j]' exist
$e[bin,i,j]   - expectation (average) of values from 'cor.exprs[i]' when 'cor.exprs[j]' exists
$var[bin,i,j] - variance of values from 'cor.exprs[i]' when 'cor.exprs[j]' exists
$cov[bin,i,j] - covariance of 'cor.exprs[i]' and 'cor.exprs[j]'
$cor[bin,i,j] - correlation of 'cor.exprs[i]' and 'cor.exprs[j]'

Similarly to 'emr_dist' 'emr_cor' can do multi-dimensional binning. Given N dimensional binning the individual data in the matrices can be accessed as: $cor[bin1, ..., binN, i, j].
}
\value{
A list of 5 elements each containing a N-dimensional vector (N is the number of 'expr'-'breaks' pairs). The member of each vector is a specific statistics matrix.
}

\seealso{
\code{\link{emr_dist}}, \code{\link{cut}}, \code{\link{emr_track.unique}}
}
\examples{
emr_db.init_examples()
emr_cor("categorial_track", c(0, 2, 5),
        cor.exprs = c("sparse_track", "1/dense_track"),
        include.lowest = T, iterator = "categorial_track",
        keepref = T)
}
\keyword{ ~correlation }
\keyword{ ~covariance }
\keyword{ ~variance }
