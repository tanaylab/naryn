\name{emr_track.attr.rm}
\alias{emr_track.attr.rm}
\title{
Deletes a track attribute
}
\description{
Deletes a track attribute.
}
\usage{
emr_track.attr.rm(track, attr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{track}{track name}
  \item{attr}{attribute name}
}
\details{
This function deletes a track attribute.
}
\value{
None.
}

\seealso{
\code{\link{emr_track.attr.set}}, \code{\link{emr_track.attr.get}}, \code{\link{emr_track.attr.export}}
}
\examples{
emr_db.init_examples()
emr_track.attr.set("sparse_track", "test_attr", "value")
emr_track.attr.export()
emr_track.attr.rm("sparse_track", "test_attr")
emr_track.attr.export()
}
\keyword{ ~attr }
\keyword{ ~attribute }

