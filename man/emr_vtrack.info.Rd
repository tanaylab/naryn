\name{emr_vtrack.info}
\alias{emr_vtrack.info}
\title{
Returns the definition of a virtual track
}
\description{
Returns the definition of a virtual track.
}
\usage{
emr_vtrack.info(vtrack)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vtrack}{virtual track name}
}
\details{
This function returns the internal represenation of a virtual track.
}
\value{
Internal representation of a virtual track.
}

\seealso{
\code{\link{emr_vtrack.create}}
}
\examples{
emr_db.init_examples()
emr_vtrack.create("vtrack1", "dense_track", c(5, 10), "max")
emr_vtrack.info("vtrack1")
}
\keyword{ ~virtual }

