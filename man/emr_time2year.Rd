\name{emr_time2year}
\alias{emr_time2year}
\title{
Converts time from internal format to a year
}
\description{
Converts time from internal format to a year.
}
\usage{
emr_time2year(time)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{time}{vector of times in internal format}
}
\details{
This function converts time from internal format to a year.
}
\value{
Vector of converted times.
}

\seealso{
\code{\link{emr_time2hour}}, \code{\link{emr_time2dayofmonth}}, \code{\link{emr_time2month}}, \code{\link{emr_date2time}}
}
\examples{
emr_db.init_examples()

# 30 January, 1938, 6:00 - birthday of Islam Karimov
t <- emr_date2time(30, 1, 1938, 6)
emr_time2hour(t)
emr_time2dayofmonth(t)
emr_time2month(t)
emr_time2year(t)
}
\keyword{ ~time }
