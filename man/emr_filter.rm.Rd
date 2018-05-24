\name{emr_filter.rm}
\alias{emr_filter.rm}
\title{
Deletes a named filter
}
\description{
Deletes a named filter.
}
\usage{
emr_filter.rm(filter)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filter}{virtual track name}
}
\details{
This function deletes a named filter from current R environment.
}
\value{
None.
}

\seealso{
\code{\link{emr_filter.create}}, \code{\link{emr_filter.ls}}
}
\examples{
emr_db.init_examples()
emr_filter.create("f1", "dense_track", time.shift = c(2,4))
emr_filter.create("f2", "dense_track", keepref = T)
emr_filter.ls()
emr_filter.rm("f1")
emr_filter.ls()
}
\keyword{ ~virtual }
