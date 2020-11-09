\name{emr_track.attr.get}
\alias{emr_track.attr.get}
\title{
Returns the value of the track attribute
}
\description{
Returns the value of the track attribute.
}
\usage{
emr_track.attr.get(track, attr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{track}{track name}
  \item{attr}{attribute name}
}
\details{
This function returns the value of a track attribute or 'NULL' if the attribute does not exist.
}
\value{
Track attribute value or 'NULL'.
}

\seealso{
\code{\link{emr_track.attr.export}}, \code{\link{emr_track.attr.set}}
}
\examples{
emr_db.init_examples()
emr_track.attr.set("sparse_track", "test_attr", "value")
emr_track.attr.get("sparse_track", "test_attr")
}
\keyword{ ~attr }
\keyword{ ~attribute }

