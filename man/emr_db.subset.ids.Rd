\name{emr_db.subset.ids}
\alias{emr_db.subset.ids}
\title{
Returns the ids that constitute the current ids subset
}
\description{
Returns the ids that constitute the current ids subset.
}
\usage{
emr_db.subset.ids()
}
%- maybe also 'usage' for other objects documented here.
\arguments{
}
\details{
'emr_db.subset.ids' returns the ids that constitute the current ids subset. The ids are returned in "ids table" format.

If no ids subset is defined, 'emr_db.subset.ids' returns 'NULL'.
}
\value{
Ids table or 'NULL'
}

\seealso{
\code{\link{emr_db.subset}}
}
\keyword{ ~db }
\keyword{ ~data }
\keyword{ ~database }
\keyword{ ~subset }
