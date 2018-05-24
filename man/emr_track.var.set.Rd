\name{emr_track.var.set}
\alias{emr_track.var.set}
\title{
Assigns value to a track variable
}
\description{
Assigns value to a track variable.
}
\usage{
emr_track.var.set(track, var, value)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{track}{track name}
  \item{var}{track variable name}
  \item{value}{value}
}
\details{
This function creates a track variable and assigns 'value' to it. If the track variable already exists its value is overwritten.
}
\value{
None.
}

\seealso{
\code{\link{emr_track.var.get}}, \code{\link{emr_track.var.ls}}, \code{\link{emr_track.var.rm}}
}
\examples{
emr_db.init_examples()
emr_track.var.set("sparse_track", "test_var", 1 : 10)
emr_track.var.get("sparse_track", "test_var")
emr_track.var.rm("sparse_track", "test_var")
}
\keyword{ ~variable }

