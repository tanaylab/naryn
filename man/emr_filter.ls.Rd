\name{emr_filter.ls}
\alias{emr_filter.ls}
\title{
Returns a list of named filters
}
\description{
Returns a list of named filters.
}
\usage{
emr_filter.ls(pattern = "", ignore.case = FALSE, perl = FALSE,
              fixed = FALSE, useBytes = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pattern, ignore.case, perl, fixed, useBytes}{see 'grep'}
}
\details{
This function returns a list of named filters that exist in current R environment that match the pattern (see 'grep'). If called without any arguments all named filters are returned.
}
\value{
An array that contains the names of filters.
}

\seealso{
\code{\link{grep}}, \code{\link{emr_filter.exists}}, \code{\link{emr_filter.create}}, \code{\link{emr_filter.rm}}
}
\examples{
emr_db.init_examples()
emr_filter.create("f1", "dense_track", time.shift = c(2,4))
emr_filter.create("f2", "dense_track", keepref = T)
emr_filter.ls()
emr_filter.ls("*2")
}
\keyword{ ~filter }
\keyword{ ~ls }
