\name{emr_track.rm}
\alias{emr_track.rm}
\title{
Deletes a track
}
\description{
Deletes a track.
}
\usage{
emr_track.rm(track, force = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{track}{track name}
  \item{force}{if 'TRUE', supresses user confirmation of a named track removal}
}
\details{
This function deletes a user track from the database. By default 'gtrack.rm' requires the user to interactively confirm the deletion. Set 'force' to 'TRUE' to suppress the user prompt.
}
\value{
None.
}

\seealso{
\code{\link{emr_track.create}}, \code{\link{emr_track.ls}}
}
\keyword{ ~track }
