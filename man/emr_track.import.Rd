\name{emr_track.import}
\alias{emr_track.import}
\title{
Imports a track from a file or data-frame
}
\description{
Imports a track from a file or data-frame.
}
\usage{
emr_track.import(track, space, categorial, src)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{track}{the name of the newly created track}
  \item{space}{"user" or "global" space}
  \item{categorial}{if 'TRUE' track is marked as categorial}
  \item{src}{file name or data-frame containing the track records}
}
\details{
This function creates a new track from a text file or a data-frame. The location of the track is controlled via 'space' parameter which can be either "user" or to "global".

If 'src' is a file name, the latter must be constituted of four columns separated by spaces or 'TAB' characters: ID, time, reference and value. The file might contain lines of comments which should start with a '#' character.

Alternatively 'src' can be a data frame consisting of the columns named "id", "time", "ref" and "value". Note: "ref" column in the data frame is optional.
}
\value{
None.
}

\seealso{
\code{\link{emr_track.addto}}, \code{\link{emr_track.create}}, \code{\link{emr_db.init}}, \code{\link{emr_track.ls}}
}
\keyword{ ~import }
