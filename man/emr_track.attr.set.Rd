\name{emr_track.attr.set}
\alias{emr_track.attr.set}
\title{
Assigns a value to the track attribute
}
\description{
Assigns a value to the track attribute.
}
\usage{
emr_track.attr.set(track, attr, value)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{track}{track name}
  \item{attr}{attribute name}
  \item{value}{value}
}
\details{
This function creates a track attribute and assigns 'value' to it. If the attribute already exists its value is overwritten.
}
\value{
None.
}

\seealso{
\code{\link{emr_track.attr.get}}, \code{\link{emr_track.attr.rm}}, \code{\link{emr_track.attr.export}}
}
\examples{
emr_db.init_examples()
emr_track.attr.set("sparse_track", "test_attr", "value")
emr_track.attr.get("sparse_track", "test_attr")
}
\keyword{ ~attr }
\keyword{ ~attribute }

