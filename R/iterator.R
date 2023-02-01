.emr_time_to_date_obj <- function(time) {
    d <- emr_time2date(time)
    return(as.Date(
        glue::glue(
            "{y}-{m}-{d}",
            y = d$year,
            m = d$month,
            d = d$day
        ),
        format = "%Y-%m-%d"
    ))
}

iterator_by_period <- function(stime, etime, n, period) {
    if (is.null(etime) && is.null(n)) {
        stop("Please set either 'etime' or 'n' parameters")
    }

    start_date <- .emr_time_to_date_obj(stime)

    if (!is.null(n)) {
        dates <- seq(start_date, by = period, length.out = n + 1)
    }

    if (!is.null(etime)) {
        end_date <- .emr_time_to_date_obj(etime)
        dates_etime <- seq(start_date, end_date, by = period)
        if (!is.null(n)) {
            end_date <- min(utils::tail(dates, 1), utils::tail(dates_etime, 1))
            dates <- seq(start_date, end_date, by = period)
        } else {
            dates <- dates_etime
        }
    }

    times <- strsplit(as.character(dates), split = "-") %>%
        purrr::map_dbl(~
            emr_date2time(
                year = as.numeric(.x[1]),
                month = as.numeric(.x[2]),
                day = as.numeric(.x[3]),
                hour = emr_time2date(stime)$hour
            ))

    iterator <- data.frame(stime = times, etime = times)

    return(iterator)
}


#' Create an iterator that goes every year/month
#'
#' Create an iterator that goes every year/month, from \code{stime}.
#' If \code{etime} is set, the iterator would go every year/month until the last point which is <= \code{etime}.
#' If \code{month} or \code{years} is set, the iterator would be set for every year/month
#' \code{n}times.
#' If both parameters are set, the iterator would go from \code{etime} until the early between \code{n}
#' times and \code{etime}.
#'
#'
#' @param stime the date of the first point in machine format (use \code{emr_date2time})
#' @param etime end of time scope (can be \code{NULL} if \code{months} parameter is set)
#' @param n number of months / years
#'
#' @return an id time data frame that can be used as an iterator
#'
#' @examples
#' iter <- emr_monthly_iterator(emr_date2time(1, 1, 2002), emr_date2time(1, 1, 2017))
#' # note that the examples database doesn't include actual dates, so the results are empty
#' emr_extract("dense_track", iterator = iter, stime = 1, etime = 3)
#'
#' iter <- emr_monthly_iterator(emr_date2time(1, 1, 2002), n = 15)
#' emr_extract("dense_track", iterator = iter, stime = 1, etime = 3)
#'
#' @export
emr_monthly_iterator <- function(stime, etime = NULL, n = NULL) {
    return(iterator_by_period(stime, etime, n, "month"))
}


#' @export
#' @rdname emr_monthly_iterator
emr_yearly_iterator <- function(stime, etime = NULL, n = NULL) {
    return(iterator_by_period(stime, etime, n, "years"))
}
