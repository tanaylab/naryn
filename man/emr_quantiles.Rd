\name{emr_quantiles}
\alias{emr_quantiles}
\title{
Calculates quantiles of a track expression
}
\description{
Calculates quantiles of a track expression for the given percentiles.
}
\usage{
emr_quantiles(expr, percentiles = 0.5, stime = MINTIME,
              etime = MAXTIME, iterator = NULL, keepref = F,
              filter = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{expr}{track expression}
  \item{percentiles}{an array of percentiles of quantiles in [0, 1] range}
  \item{stime}{start time scope}
  \item{etime}{end time scope}
  \item{iterator}{track expression iterator. If 'NULL' iterator is determined implicitly based on track expression.}
  \item{keepref}{If 'TRUE' references are preserved in the iterator.}
  \item{filter}{Iterator filter.}
}
\details{
This function calculates quantiles for the given percentiles.

If data size exceeds the limit (see: 'getOption(emr_max.data.size)'), the data is randomly sampled to fit the limit. A warning message is generated. The seed of the pseudo-random generator can be controled through 'grnd.seed' option.
}
\value{
An array that represent quantiles.
}

\seealso{
\code{\link{emr_extract}}
}
\examples{
emr_db.init_examples()
emr_quantiles("sparse_track", c(0.1, 0.6, 0.8))
}
\keyword{ ~quantiles }
\keyword{ ~percentiles }
