#' Converts time from internal format to an hour
#' 
#' Converts time from internal format to an hour.
#' 
#' This function converts time from internal format to an hour in [0, 23]
#' range.
#' 
#' @param time vector of times in internal format
#' @return Vector of converted times.
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
#' 
#' @export emr_time2hour
emr_time2hour <- function(time) {
    if (missing(time)) {
          stop("Usage: emr_time2hour(time)", call. = F)
      }

    .emr_call("emr_time2hour", time, new.env(parent = parent.frame()))
}



#' Converts time from internal format to a day of month
#' 
#' Converts time from internal format to a day of month.
#' 
#' This function converts time from internal format to a day of month in [1,
#' 31] range.
#' 
#' @param time vector of times in internal format
#' @return Vector of converted times.
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
#' 
#' @export emr_time2dayofmonth
emr_time2dayofmonth <- function(time) {
    if (missing(time)) {
          stop("Usage: emr_time2dayofmonth(time)", call. = F)
      }

    .emr_call("emr_time2dayofmonth", time, new.env(parent = parent.frame()))
}



#' Converts time from internal format to a month
#' 
#' Converts time from internal format to a month.
#' 
#' This function converts time from internal format to a month in [1, 12]
#' range.
#' 
#' @param time vector of times in internal format
#' @return Vector of converted times.
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
#' 
#' @export emr_time2month
emr_time2month <- function(time) {
    if (missing(time)) {
          stop("Usage: emr_time2month(time)", call. = F)
      }

    .emr_call("emr_time2month", time, new.env(parent = parent.frame()))
}



#' Converts time from internal format to a year
#' 
#' Converts time from internal format to a year.
#' 
#' This function converts time from internal format to a year.
#' 
#' @param time vector of times in internal format
#' @return Vector of converted times.
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
#' 
#' @export emr_time2year
emr_time2year <- function(time) {
    if (missing(time)) {
          stop("Usage: emr_time2year(time)", call. = F)
      }

    .emr_call("emr_time2year", time, new.env(parent = parent.frame()))
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
#' 
#' @export emr_date2time
emr_date2time <- function(day, month, year, hour = 0) {
    if (missing(day) || missing(month) || missing(year)) {
          stop("Usage: emr_date2time(day, month, year, hour = 0)", call. = F)
      }

    .emr_call("emr_date2time", data.frame(hour, day, month, year), new.env(parent = parent.frame()))
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
#' r1<-emr_extract("sparse_track", keepref=T)
#' r2<-emr_extract("dense_track", keepref=T)
#' r2$dense_track <- r2$dense_track + 1000
#' emr_annotate(r1, r2)
#' 
#' @export emr_annotate
emr_annotate <- function(x, y) {
    if (missing(x) || missing(y)) {
          stop("Usage: emr_annotate(x, y)", call. = F)
      }

    .emr_call("emr_annotate", x, y, new.env(parent = parent.frame()))
}
