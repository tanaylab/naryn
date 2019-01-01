\name{emr_db.init}
\alias{emr_db.init}
\alias{emr_db.init_examples}
\title{
Initializes connection with Naryn Database
}
\description{
Initializes connection with Naryn Database.
}
\usage{
emr_db.init(global.dir, user.dir = NULL, load.on.demand = T)
emr_db.init_examples()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{global.dir}{root directory of global tracks}
  \item{user.dir}{'NULL' or root directory of user tracks}
  \item{load.on.demand}{see below}
}
\details{
'emr_db.init' initializes the connection with Naryn Database. 'global.dir' marks the root directory of the global tracks. 'user.dir' parameter specifies the directory of the user tracks. Unlike global tracks user tracks may be deleted ('emr_track.rm') or new ones can be created ('emr_track.create'). 'user.dir' can be 'NULL', in which case operations on user tracks will be disabled.

When the package is attached it internally calls 'emr_db.init_examples' which sets 'global.dir' to 'PKGDIR/naryndb/test' and 'user.dir' to NULL ('PKGDIR' is the directory where the package is installed).

If 'load.on.demand' parameter is 'TRUE' the list of tracks is taken from a cached file named '.naryn-cache' and the tracks are loaded into memory on demand. When 'load.on.demand' is 'FALSE' or '.naryn-cache' file is missing or needs to be rebuilt, the directories are rescanned to obtain the current list of tracks and all the tracks are loaded into physical memory.

Note: tracks that are loaded into memory are shared among various processes that run on the same machine.

Upon completion the connection is established with the database and a few global variables are added to the environment. These variables should not be modified by the user!

\tabular{ll}{
    EMR_GROOT \tab Root directory of global tracks\cr
    EMR_UROOT \tab Root directory of user tracks\cr
    MINID \tab Minimal id stored in the database \cr
    MAXID \tab Maximal id stored in the database \cr
    MINTIME \tab Minimal time value stored in the database \cr
    MAXTIME \tab Maximal id stored in the database \cr
}
}
\value{
None.
}

\seealso{
\code{\link{emr_db.reload}}, \code{\link{emr_track.import}}, \code{\link{emr_track.create}}, \code{\link{emr_track.rm}}, \code{\link{emr_track.ls}}, \code{\link{emr_vtrack.ls}}, \code{\link{emr_filter.ls}}
}
\keyword{ ~db }
\keyword{ ~data }
\keyword{ ~database }
