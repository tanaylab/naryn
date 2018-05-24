\name{emr_vtrack.rm}
\alias{emr_vtrack.rm}
\title{
Deletes a virtual track
}
\description{
Deletes a virtual track.
}
\usage{
emr_vtrack.rm(vtrack)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vtrack}{virtual track name}
}
\details{
This function deletes a virtual track from current R environment.
}
\value{
None.
}

\seealso{
\code{\link{emr_vtrack.create}}, \code{\link{emr_vtrack.ls}}
}
\examples{
emr_db.init_examples()
emr_vtrack.create("vtrack1", "dense_track")
emr_vtrack.create("vtrack2", "dense_track")
emr_vtrack.ls()
emr_vtrack.rm("vtrack1")
emr_vtrack.ls()
}
\keyword{ ~virtual }
