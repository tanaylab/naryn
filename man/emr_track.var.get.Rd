\name{emr_track.var.get}
\alias{emr_track.var.get}
\title{
Returns value of a track variable
}
\description{
Returns value of a track variable.
}
\usage{
emr_track.var.get(track, var)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{track}{track name}
  \item{var}{track variable name}
}
\details{
This function returns the value of a track variable. If the variable does not exist an error is reported.
}
\value{
Track variable value.
}

\seealso{
\code{\link{emr_track.var.set}}, \code{\link{emr_track.var.ls}}, \code{\link{emr_track.var.rm}}
}
\examples{
emr_db.init_examples()
emr_track.var.set("sparse_track", "test_var", 1 : 10)
emr_track.var.get("sparse_track", "test_var")
emr_track.var.rm("sparse_track", "test_var")
}
\keyword{ ~variable }

