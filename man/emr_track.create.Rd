\name{emr_track.create}
\alias{emr_track.create}
\title{
Creates a track from a track expression
}
\description{
Creates a track from a track expression.
}
\usage{
emr_track.create(track, space, categorial, expr, stime = MINTIME,
                 etime = MAXTIME, iterator = NULL, keepref = F,
                 filter = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{track}{the name of the newly created track}
  \item{space}{"user" or "global" space}
  \item{categorial}{if 'TRUE' track is marked as categorial}
  \item{expr}{track expression}
  \item{stime}{start time scope}
  \item{etime}{end time scope}
  \item{iterator}{track expression iterator. If 'NULL' iterator is determined implicitly based on track expressions}
  \item{keepref}{If 'TRUE' references are preserved in the iterator}
  \item{filter}{Iterator filter}
}
\details{
This function creates a new user or global track based on the values from the track expression. The location of the track is controlled via 'space' parameter which can be either "user" or to "global".
}
\value{
None.
}

\seealso{
\code{\link{emr_track.import}}, \code{\link{emr_track.addto}}, \code{\link{emr_track.rm}}, \code{\link{emr_track.ls}}, \code{\link{emr_track.exists}}
}
\keyword{ ~track }
\keyword{ ~create }
