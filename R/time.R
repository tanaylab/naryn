#' Converts time from internal format to an hour
#'
#' Converts time from internal format to an hour.
#'
#' This function converts time from internal format to an hour in [0, 23]
#' range.
#'
#' @param time vector of times in internal format
#' @return Vector of converted times. NA values in the vector would be returned as NA's.
#' @seealso \code{\link{emr_time2dayofmonth}}, \code{\link{emr_time2month}},
#' \code{\link{emr_time2year}}, \code{\link{emr_date2time}}
#' @keywords ~time
#' @examples
#'
#' emr_db.init_examples()
#'
#' # 30 January, 1938, 6:00 - birthday of Islam Karimov
#' t <- emr_date2time(30, 1, 1938, 6)
#' emr_time2hour(t)
#' emr_time2dayofmonth(t)
#' emr_time2month(t)
#' emr_time2year(t)
#' @export emr_time2hour
emr_time2hour <- function(time) {
    if (missing(time)) {
        stop("Usage: emr_time2hour(time)", call. = FALSE)
    }

    .emr_call("C_emr_time2hour", time, .emr_env())
}



#' Converts time from internal format to a day of month
#'
#' Converts time from internal format to a day of month.
#'
#' This function converts time from internal format to a day of month in [1,
#' 31] range.
#'
#' @param time vector of times in internal format
#' @return Vector of converted times. NA values in the vector would be returned as NA's.
#' @seealso \code{\link{emr_time2hour}}, \code{\link{emr_time2month}},
#' \code{\link{emr_time2year}}, \code{\link{emr_date2time}}
#' @keywords ~time
#' @examples
#'
#' emr_db.init_examples()
#'
#' # 30 January, 1938, 6:00 - birthday of Islam Karimov
#' t <- emr_date2time(30, 1, 1938, 6)
#' emr_time2hour(t)
#' emr_time2dayofmonth(t)
#' emr_time2month(t)
#' emr_time2year(t)
#' @export emr_time2dayofmonth
emr_time2dayofmonth <- function(time) {
    if (missing(time)) {
        stop("Usage: emr_time2dayofmonth(time)", call. = FALSE)
    }

    .emr_call("C_emr_time2dayofmonth", time, .emr_env())
}



#' Converts time from internal format to a month
#'
#' Converts time from internal format to a month.
#'
#' This function converts time from internal format to a month in [1, 12]
#' range.
#'
#' @param time vector of times in internal format
#' @return Vector of converted times. NA values in the vector would be returned as NA's.
#' @seealso \code{\link{emr_time2hour}}, \code{\link{emr_time2dayofmonth}},
#' \code{\link{emr_time2year}}, \code{\link{emr_date2time}}
#' @keywords ~time
#' @examples
#'
#' emr_db.init_examples()
#'
#' # 30 January, 1938, 6:00 - birthday of Islam Karimov
#' t <- emr_date2time(30, 1, 1938, 6)
#' emr_time2hour(t)
#' emr_time2dayofmonth(t)
#' emr_time2month(t)
#' emr_time2year(t)
#' @export emr_time2month
emr_time2month <- function(time) {
    if (missing(time)) {
        stop("Usage: emr_time2month(time)", call. = FALSE)
    }

    .emr_call("C_emr_time2month", time, .emr_env())
}



#' Converts time from internal format to a year
#'
#' Converts time from internal format to a year.
#'
#' This function converts time from internal format to a year.
#'
#' @param time vector of times in internal format
#' @return Vector of converted times. NA values in the vector would be returned as NA's.
#' @seealso \code{\link{emr_time2hour}}, \code{\link{emr_time2dayofmonth}},
#' \code{\link{emr_time2month}}, \code{\link{emr_date2time}}
#' @keywords ~time
#' @examples
#'
#' emr_db.init_examples()
#'
#' # 30 January, 1938, 6:00 - birthday of Islam Karimov
#' t <- emr_date2time(30, 1, 1938, 6)
#' emr_time2hour(t)
#' emr_time2dayofmonth(t)
#' emr_time2month(t)
#' emr_time2year(t)
#' @export emr_time2year
emr_time2year <- function(time) {
    if (missing(time)) {
        stop("Usage: emr_time2year(time)", call. = FALSE)
    }

    .emr_call("C_emr_time2year", time, .emr_env())
}



#' Converts date and hour to internal time format
#'
#' Converts date and hour to internal time format.
#'
#' This function converts date and hour to internal time format. Note: the
#' earliest valid time is 1 March 1867.
#'
#' Note: if one of the arguments ('day', ...) is a vector, then the other
#' arguments must be vectors two of identical size or scalars. Internally a
#' data frame is built out of all the vectors or scalars before the conversion
#' is applied. Hence rules for data frame creation apply to this function.
#'
#' @param day vector of days of month in [1, 31] range
#' @param month vector of months in [1, 12] range
#' @param year vector of years
#' @param hour vector of hours in [0, 23] range
#' @return Vector of converted times.
#' @seealso \code{\link{emr_time2hour}}, \code{\link{emr_time2dayofmonth}},
#' \code{\link{emr_time2month}}, \code{\link{emr_time2year}}
#' @keywords ~time
#' @examples
#'
#' emr_db.init_examples()
#'
#' # 30 January, 1938, 6:00 - birthday of Islam Karimov
#' t <- emr_date2time(30, 1, 1938, 6)
#' emr_time2hour(t)
#' emr_time2dayofmonth(t)
#' emr_time2month(t)
#' emr_time2year(t)
#'
#' # cover all times when Islam Karimov could have been born
#' # (if we don't know the exact hour!)
#' t <- emr_date2time(30, 1, 1938, 0:23)
#' @export emr_date2time
emr_date2time <- function(day, month, year, hour = 0) {
    if (missing(day) || missing(month) || missing(year)) {
        stop("Usage: emr_date2time(day, month, year, hour = 0)", call. = FALSE)
    }

    .emr_call("C_emr_date2time", data.frame(hour, day, month, year), .emr_env())
}

#' Convert from internal time to year, month, day, hour
#'
#' @param time vector of times in internal format
#'
#' @return a data frame with columns named 'year', 'month', 'day' and 'hour'
#'
#'
#' @examples
#'
#' emr_db.init_examples()
#'
#' # 30 January, 1938, 6:00 - birthday of Islam Karimov
#' t1 <- emr_date2time(30, 1, 1938, 6)
#' # September 2, 2016, 7:00 - death of Islam Karimov
#' t2 <- emr_date2time(2, 9, 2016, 7)
#' emr_time2date(c(t1, t2))
#' @export
emr_time2date <- function(time) {
    data.frame(
        year = emr_time2year(time),
        month = emr_time2month(time),
        day = emr_time2dayofmonth(time),
        hour = emr_time2hour(time)
    )
}

#' Convert EMR time to POSIXct
#'
#' These function converts EMR time to POSIXct format. It takes the EMR time as input and returns the corresponding POSIXct object.
#'
#' @param time The EMR time to be converted.
#' @param show_hour Logical value indicating whether to include the hour in the output. Default is FALSE.
#' @param tz Time zone to be used for the output POSIXct object. Default is "UTC".
#' @param posix A POSIXct object to be converted to EMR time.
#'
#' @return A POSIXct object representing the converted time.
#'
#' @examples
#' # 30 January, 1938, 6:00 - birthday of Islam Karimov
#' t1 <- emr_date2time(30, 1, 1938, 6)
#' # September 2, 2016, 7:00 - death of Islam Karimov
#' t2 <- emr_date2time(2, 9, 2016, 7)
#'
#' emr_time2posix(c(t1, t2))
#' emr_time2posix(c(t1, t2), show_hour = TRUE)
#'
#' emr_posix2time(emr_time2posix(c(t1, t2), show_hour = TRUE))
#'
#' # Note that when show_hour = FALSE, the hour is set to 0
#' # and therefore the results would be different from the original time values
#' emr_posix2time(emr_time2posix(c(t1, t2)))
#'
#' @export
emr_time2posix <- function(time, show_hour = FALSE, tz = "UTC") {
    year <- emr_time2year(time)
    month <- emr_time2month(time)
    day <- emr_time2dayofmonth(time)

    if (show_hour) {
        hour <- emr_time2hour(time)
        return(as.POSIXct(paste(year, month, day, hour, sep = "-"), format = "%Y-%m-%d-%H", tz = tz))
    }

    return(as.POSIXct(paste(year, month, day, sep = "-"), format = "%Y-%m-%d", tz = tz))
}

#' Convert time to character format
#'
#' This function converts a given time value to a character format in the form of "%Y-%m-%d" or "%Y-%m-%d %H:%M:%S" depending on the value of the `show_hour` parameter.
#'
#' @param time The time value to be converted.
#' @param show_hour Logical value indicating whether to include the hour in the output. Default is FALSE.
#' @param char A character string to be converted to EMR time.
#'
#' @return A character string representing the converted time value.
#'
#' @examples
#' # 30 January, 1938, 6:00 - birthday of Islam Karimov
#' t1 <- emr_date2time(30, 1, 1938, 6)
#' # September 2, 2016, 7:00 - death of Islam Karimov
#' t2 <- emr_date2time(2, 9, 2016, 7)
#'
#' emr_time2char(c(t1, t2))
#' emr_time2char(c(t1, t2), show_hour = TRUE)
#'
#' emr_char2time(emr_time2char(c(t1, t2), show_hour = TRUE))
#'
#' # Note that when show_hour = FALSE, the hour is set to 0
#' # and therefore the results would be different from the original time values
#' emr_char2time(emr_time2char(c(t1, t2)))
#'
#' @export
emr_time2char <- function(time, show_hour = FALSE) {
    as.character(emr_time2posix(time, show_hour = show_hour))
}

#' @rdname emr_time2posix
#' @export
emr_posix2time <- function(posix) {
    day <- as.numeric(format(posix, "%d"))
    month <- as.numeric(format(posix, "%m"))
    year <- as.numeric(format(posix, "%Y"))
    hour <- as.numeric(format(posix, "%H"))

    return(emr_date2time(day, month, year, hour))
}

#' @rdname emr_time2char
#' @export
emr_char2time <- function(char) {
    return(emr_posix2time(as.POSIXct(char)))
}

#' Convert time periods to internal time format
#'
#' Convert time periods to internal time format
#'
#' \code{emr_time} converts a generic number of years, months day and hours to the internal
#' naryn machine format (which is hours).
#'
#' \code{year}, \code{years}, \code{month}, \code{months}, \code{week}, \code{weeks},
#' \code{day}, \code{days}, \code{hour}, \code{hours}
#' are other convenience functions to get a time period explicitly.
#'
#'
#' @param days number of days
#' @param months number of months
#' @param years number of years
#' @param hours number of hours
#' @param n number of days/weeks/months/years/hours
#'
#' @return Machine time format (number of hours)
#'
#' @examples
#'
#' emr_time(5) # 5 days
#' emr_time(months = 4) # 4 months
#' emr_time(2, 4, 1) # 1 year, 4 months and 2 days
#'
#' year() # 1 year
#' years(5) # 5 years
#' month() # 1 month
#' months(5) # 5 months
#' day() # 1 day
#' days(9) # 9 days
#' week() # 1 week
#' weeks(2) # 2 weeks
#' hour() # 1 hour
#' hours(5) # 5 hours
#' @export
emr_time <- function(days = 0, months = 0, years = 0, hours = 0) {
    return(years(years) + months(months) + days(days) + hours(hours))
}

#' @export
#' @rdname emr_time
hours <- function(n) {
    return(n)
}

#' @export
#' @rdname emr_time
hour <- function() {
    return(hours(1))
}

#' @export
#' @rdname emr_time
days <- function(n) {
    return(n * 24)
}

#' @export
#' @rdname emr_time
day <- function() {
    return(days(1))
}

#' @export
#' @rdname emr_time
weeks <- function(n) {
    return(n * days(7))
}

#' @export
#' @rdname emr_time
week <- function() {
    return(weeks(1))
}

#' @export
#' @rdname emr_time
day <- function() {
    return(days(1))
}

#' @export
#' @rdname emr_time
months <- function(n) {
    return(n * 30 * 24)
}

#' @export
#' @rdname emr_time
month <- function() {
    return(months(1))
}

#' @export
#' @rdname emr_time
years <- function(n) {
    return(n * 365 * 24)
}

#' @export
#' @rdname emr_time
year <- function() {
    return(years(1))
}


#' Annotates id-time points table
#'
#' Annotates id-time points table by the values given in the second table.
#'
#' This function merges two sorted id-time points tables 'x' and 'y' by
#' matching 'id', 'time' and 'ref' columns. The result is a new id-time points
#' table that has all the additional columns of 'x' and 'y'.
#'
#' Two rows match if 'id' AND 'time' match AND either 'ref' matches OR one of
#' the 'ref' is '-1'.
#'
#' If a row RX from 'x' matches N rows RY1, ..., RYn from 'y', N rows are added
#' to the result: [RX RY1], ..., [RX RYn].
#'
#' If a row RX from 'x' does not match any rows from 'y', a row of [RX NA] form
#' is added to the result (i.e. all the values of columns borrowed from 'y' are
#' set to 'NA').
#'
#' A missing 'ref' column is interpreted as if reference equals '-1'.
#'
#' Both of 'x' and 'y' must be sorted by 'id', 'time' and 'ref' (in this
#' order!). Note however that all the package functions (such as 'emr_extract',
#' ...) return id-time point tables always properly sorted.
#'
#' @param x sorted id-time points table that is expanded
#' @param y sorted id-time points table that is used for annotations
#' @return A data frame with all the columns from 'x' and additional columns
#' from 'y'.
#' @seealso \code{\link{emr_extract}}
#' @keywords ~annotate
#' @examples
#'
#' emr_db.init_examples()
#'
#' r1 <- emr_extract("sparse_track", keepref = TRUE)
#' r2 <- emr_extract("dense_track", keepref = TRUE)
#' r2$dense_track <- r2$dense_track + 1000
#' emr_annotate(r1, r2)
#' @export emr_annotate
emr_annotate <- function(x, y) {
    if (missing(x) || missing(y)) {
        stop("Usage: emr_annotate(x, y)", call. = FALSE)
    }

    .emr_call("C_emr_annotate", x, y, .emr_env())
}
