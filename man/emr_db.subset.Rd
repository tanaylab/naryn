\name{emr_db.subset}
\alias{emr_db.subset}
\title{
Defines an ids subset
}
\description{
Defines an ids subset.
}
\usage{
emr_db.subset(src, fraction, complementary, seed = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{src}{track name or ids table or 'NULL'}
  \item{fraction}{fraction of data to be sampled from 'src' in [0,1] range}
  \item{complementary}{'TRUE' for a complementary subset, otherwise 'FALSE'}
  \item{seed}{seed integer value for the random number generator}
}
\details{
'emr_db.subset' creates an ids subset" ("viewport") of data of "fraction * sizeof('src')" size by sampling the ids from 'src'. Once the subset is defined only the ids that are in the subset are used by various functions and iterators. Other ids are ignored.

'src' can be a track name or an ids table. If 'complementary' is 'TRUE' the complementary set of sampled ids is used as a subset.

If 'src' is 'NULL' the current subset is annihilated.
}
\value{
None.
}

\seealso{
\code{\link{emr_db.init}}, \code{\link{emr_db.subset.ids}}
}
\keyword{ ~db }
\keyword{ ~data }
\keyword{ ~database }
\keyword{ ~subset }
