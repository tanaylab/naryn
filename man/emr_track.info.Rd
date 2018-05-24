\name{emr_track.info}
\alias{emr_track.info}
\title{
Returns information about a track
}
\description{
Returns information about a track.
}
\usage{
emr_track.info(track)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{track}{track name}
}
\details{
This function returns information about the track: type, data type, number of vales, number of unique values, minimal / maximal value, minimal / maximal id, minimal / maximal time.

Note: this function ignores the current subset, i.e. it is applied to the whole track.
}
\value{
A list that contains track properties
}

\seealso{
\code{\link{emr_track.ls}}
}
\examples{
emr_db.init_examples()
emr_track.info("sparse_track")
}
\keyword{ ~track }
\keyword{ ~info }
\keyword{ ~property }
