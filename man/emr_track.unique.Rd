\name{emr_track.unique}
\alias{emr_track.unique}
\title{
Returns track values
}
\description{
Returns unique and sorted track values
}
\usage{
emr_track.unique(track)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{track}{track name}
}
\details{
Returns unique and sorted track values. NaN values (if exist in the track) are not returned.

Note: this function ignores the current subset, i.e. the unique values of the whole track are returned.
}
\value{
A vector of values
}

\seealso{
\code{\link{emr_track.ids}}, \code{\link{emr_track.info}}
}
\examples{
emr_db.init_examples()
emr_track.unique("categorical_track")
}
\keyword{ ~track }
\keyword{ ~unique }
