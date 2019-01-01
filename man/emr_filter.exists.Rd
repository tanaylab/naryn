\name{emr_filter.exists}
\alias{emr_filter.exists}
\title{
Checks whether the named filter exists
}
\description{
Checks whether the named filter exists.
}
\usage{
emr_filter.exists(filter)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filter}{filter name}
}
\details{
This function checks whether the named filter exists.
}
\value{
'TRUE', if the named filter exists, otherwise 'FALSE'.
}

\seealso{
\code{\link{emr_filter.create}}, \code{\link{emr_filter.ls}}
}
\examples{
emr_db.init_examples()
emr_filter.create("f1", "dense_track", time.shift = c(2,4))
emr_filter.exists("f1")
}
\keyword{ ~filter }
\keyword{ ~exists }

