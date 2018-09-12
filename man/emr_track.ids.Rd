\name{emr_track.ids}
\alias{emr_track.ids}
\title{
Returns track ids
}
\description{
Returns the ids contained by the track.
}
\usage{
emr_track.ids(track)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{track}{track name}
}
\details{
Returns the ids contained by the track.

Note: this function ignores the current subset, i.e. ids of the whole track are returned.
}
\value{
An Ids Table
}

\seealso{
\code{\link{emr_track.unique}}, \code{\link{emr_track.info}}
}
\examples{
emr_db.init_examples()
emr_track.ids("categorical_track")
}
\keyword{ ~track }
\keyword{ ~ids }
