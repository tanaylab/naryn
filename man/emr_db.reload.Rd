\name{emr_db.reload}
\alias{emr_db.reload}
\title{
Reloads database
}
\description{
  Reloads database
}
\usage{
emr_db.reload()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
}
\details{
Rebuilds Naryn database index files. Use this function if you manually add/delete/move/modify track files or if you suspect that the database is corrupted: existing tracks cannot be found, deleted ones continue to appear or a warning message is issued by Naryn itself recommending to run 'emr_db.reload'.
}

\seealso{
\code{\link{emr_db.init}}, \code{\link{emr_track.ls}}, \code{\link{emr_vtrack.ls}}
}
\keyword{ ~db }
