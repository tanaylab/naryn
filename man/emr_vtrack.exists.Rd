\name{emr_vtrack.exists}
\alias{emr_vtrack.exists}
\title{
Checks whether the virtual track exists
}
\description{
Checks whether the virtual track exists.
}
\usage{
emr_vtrack.exists(vtrack)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vtrack}{virtual track name}
}
\details{
This function checks whether the virtual track exists.
}
\value{
'TRUE' if the virtual track exists, otherwise 'FALSE'.
}

\seealso{
\code{\link{emr_vtrack.create}}, \code{\link{emr_vtrack.ls}}
}
\examples{
emr_db.init_examples()
emr_vtrack.create("vtrack1", "dense_track", c(5, 10), "max")
emr_vtrack.exists("vtrack1")
}
\keyword{ ~virtual }
\keyword{ ~exists }

