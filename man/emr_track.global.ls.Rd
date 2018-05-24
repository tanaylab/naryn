\name{emr_track.global.ls}
\alias{emr_track.global.ls}
\title{
Returns a list of global track names
}
\description{
Returns a list of global track names.
}
\usage{
emr_track.global.ls(..., ignore.case = FALSE, perl = FALSE,
                    fixed = FALSE, useBytes = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{...}{these arguments are of either form 'pattern' or 'attribute = pattern'}
  \item{ignore.case, perl, fixed, useBytes}{see 'grep'}
}
\details{
This function returns a list of global tracks in the database that match the pattern (see 'grep'). If called without any arguments all global tracks are returned.
}
\value{
An array that contains the names of global tracks that match the supplied patterns.
}

\seealso{
\code{\link{grep}}, \code{\link{emr_db.init}}, \code{\link{emr_track.ls}}, \code{\link{emr_track.user.ls}}
}
\keyword{ ~track }
\keyword{ ~tracks }
\keyword{ ~ls }
