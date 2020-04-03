\name{emr_track.mv}
\alias{emr_track.mv}
\title{
Deletes a track
}
\description{
Moves (renames) a track
}
\usage{
emr_track.mv(src, tgt, space = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{src}{source track name}
  \item{tgt}{target track name}
  \item{space}{"global" or "user" or 'NULL'}
}
\details{
This function moves (renames) 'src' track into 'tgt'. If 'space' equals 'NULL', the track remains in the same space. Otherwise it is moved to the specified space.
}
\value{
None.
}

\seealso{
\code{\link{emr_track.create}}, \code{\link{emr_track.rm}}, \code{\link{emr_track.ls}}
}
\keyword{ ~track }
