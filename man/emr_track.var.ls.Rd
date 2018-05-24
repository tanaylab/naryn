\name{emr_track.var.ls}
\alias{emr_track.var.ls}
\title{
Returns a list of track variables for a track
}
\description{
Returns a list of track variables for a track.
}
\usage{
emr_track.var.ls(track, pattern = "", ignore.case = FALSE,
                 perl = FALSE, fixed = FALSE, useBytes = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{track}{track name}
  \item{pattern, ignore.case, perl, fixed, useBytes}{see 'grep'}
}
\details{
This function returns a list of track variables of a track that match the pattern (see 'grep'). If called without any arguments all track variables of a track are returned.
}
\value{
An array that contains the names of track variables.
}

\seealso{
\code{\link{grep}}, \code{\link{emr_track.var.get}}, \code{\link{emr_track.var.set}}, \code{\link{emr_track.var.rm}}
}
\examples{
emr_db.init_examples()
emr_track.var.ls("sparse_track")
emr_track.var.set("sparse_track", "test_var1", 1 : 10)
emr_track.var.set("sparse_track", "test_var2", "v")
emr_track.var.ls("sparse_track")
emr_track.var.ls("sparse_track", pattern = "2")
emr_track.var.rm("sparse_track", "test_var1")
emr_track.var.rm("sparse_track", "test_var2")
}
\keyword{ ~variable }
\keyword{ ~ls }

