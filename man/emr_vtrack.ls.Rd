\name{emr_vtrack.ls}
\alias{emr_vtrack.ls}
\title{
Returns a list of virtual track names
}
\description{
Returns a list of virtual track names.
}
\usage{
emr_vtrack.ls(pattern = "", ignore.case = FALSE, perl = FALSE,
              fixed = FALSE, useBytes = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pattern, ignore.case, perl, fixed, useBytes}{see 'grep'}
}
\details{
This function returns a list of virtual tracks that exist in current R environment that match the pattern (see 'grep'). If called without any arguments all virtual tracks are returned.
}
\value{
An array that contains the names of virtual tracks.
}

\seealso{
\code{\link{grep}}, \code{\link{emr_vtrack.exists}}, \code{\link{emr_vtrack.create}}, \code{\link{emr_vtrack.rm}}
}
\examples{
emr_db.init_examples()
emr_vtrack.create("vtrack1", "dense_track", func = "max")
emr_vtrack.create("vtrack2", "dense_track", func = "min")
emr_vtrack.ls()
emr_vtrack.ls("*2")
}
\keyword{ ~virtual }
\keyword{ ~ls }
