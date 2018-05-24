\name{emr_track.addto}
\alias{emr_track.addto}
\title{
Adds new records to a track
}
\description{
Adds new records to a track from a TAB-delimited file or a data frame.
}
\usage{
emr_track.addto(track, src)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{track}{the name of the newly created track}
  \item{src}{file name or data-frame containing the track records}
}
\details{
This function adds new records to a track. The records are contained either in a file or a data frame.

If 'src' is a file name, the latter must be constituted of four columns separated by spaces or 'TAB' characters: ID, time, reference and value. The file might contain lines of comments which should start with a '#' character.

Alternatively 'src' can be a data frame consisting of the columns named "id", "time", "ref" and "value". Note: "ref" column in the data frame is optional.
}
\value{
None.
}

\seealso{
\code{\link{emr_track.import}}, \code{\link{emr_track.create}}, \code{\link{emr_db.init}}, \code{\link{emr_track.ls}}
}
\keyword{ ~import }
