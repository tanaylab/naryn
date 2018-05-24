\name{emr_filter.info}
\alias{emr_filter.info}
\title{
Returns the definition of a named filter
}
\description{
Returns the definition of a named filter.
}
\usage{
emr_filter.info(filter)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filter}{filter name}
}
\details{
This function returns the internal represenation of a named filter.
}
\value{
Internal representation of a named filter.
}

\seealso{
\code{\link{emr_filter.create}}
}
\examples{
emr_db.init_examples()
emr_filter.create("f1", "dense_track", time.shift = c(2,4))
emr_filter.info("f1")
}
\keyword{ ~filter }

