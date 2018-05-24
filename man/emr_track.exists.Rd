\name{emr_track.exists}
\alias{emr_track.exists}
\title{
Checks whether the track exists
}
\description{
Checks whether the track exists.
}
\usage{
emr_track.exists(track)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{track}{track name}
}
\details{
This function checks whether the track exists.
}
\value{
'TRUE' if the tracks exists, otherwise 'FALSE'
}

\seealso{
\code{\link{emr_track.ls}}, \code{\link{emr_track.info}}
}
\examples{
emr_db.init_examples()
emr_track.exists("sparse_track")
}
\keyword{ ~track }
\keyword{ ~exists }

