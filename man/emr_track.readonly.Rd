\name{emr_track.readonly}
\alias{emr_track.readonly}
\title{
Gets or sets "read-only" property of a track
}
\description{
Gets or sets "readonly" property of a track.
}
\usage{
emr_track.readonly(track, readonly = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{track}{track name}
  \item{readonly}{if 'NULL', return "readonlyness" of the track, otherwise sets it}
}
\details{
This function gets or sets "read-onlyness" of the track. If 'readonly' is 'NULL' the functions retuns whether the track is R/O. Otherwise it sets "read-onlyness" to the value indicated by 'readonly'.
}
\value{
None.
}

\seealso{
\code{\link{emr_track.create}}, \code{\link{emr_track.mv}}, \code{\link{emr_track.ls}}, \code{\link{emr_track.rm}}
}
\keyword{ ~track }
