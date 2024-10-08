% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/time.R
\name{emr_time2char}
\alias{emr_time2char}
\alias{emr_char2time}
\title{Convert time to character format}
\usage{
emr_time2char(time, show_hour = FALSE)

emr_char2time(char)
}
\arguments{
\item{time}{The time value to be converted.}

\item{show_hour}{Logical value indicating whether to include the hour in the output. Default is FALSE.}

\item{char}{A character string to be converted to EMR time.}
}
\value{
A character string representing the converted time value.
}
\description{
This function converts a given time value to a character format in the form of "%Y-%m-%d" or "%Y-%m-%d %H:%M:%S" depending on the value of the `show_hour` parameter.
}
\examples{
# 30 January, 1938, 6:00 - birthday of Islam Karimov
t1 <- emr_date2time(30, 1, 1938, 6)
# September 2, 2016, 7:00 - death of Islam Karimov
t2 <- emr_date2time(2, 9, 2016, 7)

emr_time2char(c(t1, t2))
emr_time2char(c(t1, t2), show_hour = TRUE)

emr_char2time(emr_time2char(c(t1, t2), show_hour = TRUE))

# Note that when show_hour = FALSE, the hour is set to 0
# and therefore the results would be different from the original time values
emr_char2time(emr_time2char(c(t1, t2)))

}
