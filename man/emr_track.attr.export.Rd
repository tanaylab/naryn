\name{emr_track.attr.export}
\alias{emr_track.attr.export}
\title{
Returns attributes values of tracks
}
\description{
Returns attributes values of tracks.
}
\usage{
emr_track.attr.export(tracks = NULL, attrs = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{track}{a vector of track names or 'NULL'}
  \item{attr}{a vector of attribute names or 'NULL'}
}
\details{
This function returns a data frame that contains attributes values of one or more tracks. The data frame is constituted of 3 columns named 'track', 'attr' and 'value'.

'track' parameter is optionally used to retrieve only the attributes of the specific track(s). If 'NULL', attributes of all the tracks are returned.

Likewise 'attr' allows to retrieve only specifically named attributes.

If both 'track' and 'attr' are used, the attributes that fulfill both of the conditions are returned.
}
\value{
A data frame containing attributes values of tracks.
}

\seealso{
\code{\link{emr_track.attr.get}}, \code{\link{emr_track.attr.set}}
}
\examples{
emr_db.init_examples()
emr_track.attr.export()
emr_track.attr.set("sparse_track", "gender", "female")
emr_track.attr.set("sparse_track", "tag", "")
emr_track.attr.set("dense_track", "gender", "male")
emr_track.attr.export()
emr_track.attr.export(track = "sparse_track")
emr_track.attr.export(attr = "gender")
emr_track.attr.export(track = "sparse_track", attr = "gender")
}
\keyword{ ~attr }
\keyword{ ~attribute }

