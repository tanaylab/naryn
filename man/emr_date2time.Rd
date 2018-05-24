\name{emr_date2time}
\alias{emr_date2time}
\title{
Converts date and hour to internal time format
}
\description{
Converts date and hour to internal time format.
}
\usage{
emr_date2time(day, month, year, hour = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{day}{vector of days of month in [1, 31] range}
  \item{month}{vector of months in [1, 12] range}
  \item{year}{vector of years}
  \item{hour}{vector of hours in [0, 23] range}
}
\details{
This function converts date and hour to internal time format. Note: the earliest valid time is 1 March 1867.

Note: if one of the arguments ('day', ...) is a vector, then other arguments must be vectors two of identical size or a scalar. Internally a data frame is built out of all the vectors or scalars before the conversion is applied. Hence rules for data frame creation apply to this function.
}
\value{
Vector of converted times.
}

\seealso{
\code{\link{emr_time2hour}}, \code{\link{emr_time2dayofmonth}}, \code{\link{emr_time2month}}, \code{\link{emr_time2year}}
}
\examples{
emr_db.init_examples()

# 30 January, 1938, 6:00 - birthday of Islam Karimov
t <- emr_date2time(30, 1, 1938, 6)
emr_time2hour(t)
emr_time2dayofmonth(t)
emr_time2month(t)
emr_time2year(t)

# cover all times when Islam Karimov could have been born
# (if we don't know the exact hour!)
t <- emr_date2time(30, 1, 1938, 0:23)
}
\keyword{ ~time }
