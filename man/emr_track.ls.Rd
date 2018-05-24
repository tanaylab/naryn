\name{emr_track.ls}
\alias{emr_track.ls}
\title{
Returns a list of track names
}
\description{
Returns a list of track names in the database.
}
\usage{
emr_track.ls(..., ignore.case = FALSE, perl = FALSE, fixed = FALSE,
             useBytes = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{...}{these arguments are of either form 'pattern' or 'attribute = pattern'}
  \item{ignore.case, perl, fixed, useBytes}{see 'grep'}
}
\details{
This function returns a list of all tracks (global and user) in the database that match the pattern (see 'grep'). If called without any arguments all tracks are returned.
}
\value{
An array that contains the names of tracks that match the supplied patterns.
}

\seealso{
\code{\link{grep}}, \code{\link{emr_db.init}}, \code{\link{emr_track.global.ls}}, \code{\link{emr_track.user.ls}}, \code{\link{emr_track.exists}}
}
\examples{
emr_db.init_examples()

# get all track names
emr_track.ls()

# get track names that match the pattern "den*"
emr_track.ls("den*")
}
\keyword{ ~track }
\keyword{ ~tracks }
\keyword{ ~ls }
