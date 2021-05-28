
emr_time2hour <- function(time) {
    if (missing(time)) {
          stop("Usage: emr_time2hour(time)", call. = F)
      }

    .emr_call("emr_time2hour", time, new.env(parent = parent.frame()))
}

emr_time2dayofmonth <- function(time) {
    if (missing(time)) {
          stop("Usage: emr_time2dayofmonth(time)", call. = F)
      }

    .emr_call("emr_time2dayofmonth", time, new.env(parent = parent.frame()))
}

emr_time2month <- function(time) {
    if (missing(time)) {
          stop("Usage: emr_time2month(time)", call. = F)
      }

    .emr_call("emr_time2month", time, new.env(parent = parent.frame()))
}

emr_time2year <- function(time) {
    if (missing(time)) {
          stop("Usage: emr_time2year(time)", call. = F)
      }

    .emr_call("emr_time2year", time, new.env(parent = parent.frame()))
}

emr_date2time <- function(day, month, year, hour = 0) {
    if (missing(day) || missing(month) || missing(year)) {
          stop("Usage: emr_date2time(day, month, year, hour = 0)", call. = F)
      }

    .emr_call("emr_date2time", data.frame(hour, day, month, year), new.env(parent = parent.frame()))
}

emr_annotate <- function(x, y) {
    if (missing(x) || missing(y)) {
          stop("Usage: emr_annotate(x, y)", call. = F)
      }

    .emr_call("emr_annotate", x, y, new.env(parent = parent.frame()))
}
