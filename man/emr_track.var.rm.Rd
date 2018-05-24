\name{emr_track.var.rm}
\alias{emr_track.var.rm}
\title{
Deletes a track variable
}
\description{
Deletes a track variable.
}
\usage{
emr_track.var.rm(track, var)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{track}{track name}
  \item{var}{track variable name}
}
\details{
This function deletes a track variable.
}
\value{
None.
}

\seealso{
\code{\link{emr_track.var.get}}, \code{\link{emr_track.var.set}}, \code{\link{emr_track.var.ls}}
}
\examples{
emr_db.init_examples()
emr_track.var.set("sparse_track", "test_var1", 1 : 10)
emr_track.var.set("sparse_track", "test_var2", "v")
emr_track.var.ls("sparse_track")
emr_track.var.rm("sparse_track", "test_var1")
emr_track.var.rm("sparse_track", "test_var2")
emr_track.var.ls("sparse_track")
}
\keyword{ ~variable }

