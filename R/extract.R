#' Calculates correlation statistics for pairs of track expressions
#'
#' Calculates correlation statistics for pairs of track expressions.
#'
#' This function works in a similar manner to 'emr_dist'. However instead of
#' returning a single counter for each bin 'emr_cor' returns 5 matrices of
#' 'length(cor.exprs) X length(cor.exprs)' size. Each matrix represents the
#' correlation statistics for each pair of track expressions from 'cor.exprs'.
#' Given a 'bin' and a pair of track expressions 'cor.exprs[i]' and
#' 'cor.exprs[j]' the corresponding matrix contains the following information:
#'
#' $n[bin,i,j] - number of times when both 'cor.exprs[i]' and 'cor.exprs[j]'
#' exist $e[bin,i,j] - expectation (average) of values from 'cor.exprs[i]' when
#' 'cor.exprs[j]' exists $var[bin,i,j] - variance of values from 'cor.exprs[i]'
#' when 'cor.exprs[j]' exists $cov[bin,i,j] - covariance of 'cor.exprs[i]' and
#' 'cor.exprs[j]' $cor[bin,i,j] - correlation of 'cor.exprs[i]' and
#' 'cor.exprs[j]'
#'
#' Similarly to 'emr_dist' 'emr_cor' can do multi-dimensional binning. Given N
#' dimensional binning the individual data in the matrices can be accessed as:
#' $cor[bin1, ..., binN, i, j].
#'
#' If \code{dataframe = TRUE} the return value is a data frame with a column for each track expression, additional columns i,j with pairs of \code{cor_exprs}
#' and another 5 columns: 'n', 'e', 'var', 'cov', 'cor' with the same values
#' as the matrices described above.
#'
#' @param expr track expression.
#' @param breaks breaks that determine the bin or 'NULL'.
#' @param cor.exprs vector of track expressions for which correlation
#' statistics is calculated.
#' @param include.lowest if 'TRUE', the lowest (or highest, for ‘right =
#' FALSE’) value of the range determined by breaks is included.
#' @param right if 'TRUE' the intervals are closed on the right (and open on
#' the left), otherwise vice versa.
#' @param stime start time scope.
#' @param etime end time scope.
#' @param iterator track expression iterator. If 'NULL' iterator is determined
#' implicitly based on track expressions.
#' @param keepref If 'TRUE' references are preserved in the iterator.
#' @param filter Iterator filter.
#' @return A list of 5 elements each containing a N-dimensional vector (N is
#' the number of 'expr'-'breaks' pairs). The member of each vector is a
#' specific statistics matrix. If \code{dataframe == TRUE} - a data frame with a column for each track expression, additional columns i,j with pairs of \code{cor_exprs} and another 5 columns: 'n', 'e', 'var', 'cov', 'cor', see description.
#' @param dataframe return a data frame instead of an N-dimensional vector.
#' @param names names for track expressions in the returned dataframe (only relevant when \code{dataframe == TRUE})
#' @seealso \code{\link{emr_dist}}, \code{\link{cut}},
#' \code{\link{emr_track.unique}}
#' @keywords ~correlation ~covariance ~variance
#' @examples
#'
#' emr_db.init_examples()
#' emr_cor("categorical_track", c(0, 2, 5),
#'     cor.exprs = c("sparse_track", "1/dense_track"),
#'     include.lowest = T, iterator = "categorical_track",
#'     keepref = T
#' )
#' emr_cor("categorical_track", c(0, 2, 5),
#'     cor.exprs = c("sparse_track", "1/dense_track"),
#'     include.lowest = T, iterator = "categorical_track",
#'     keepref = T,
#'     dataframe = TRUE
#' )
#' @export emr_cor
emr_cor <- function(..., cor.exprs = NULL, include.lowest = FALSE, right = TRUE, stime = NULL, etime = NULL, iterator = NULL, keepref = F, filter = NULL, dataframe = FALSE, names = NULL) {
    args <- list(...)
    if (length(args) < 2 || (length(args) %% 2 != 0 && (length(args) - 1) %% 2 != 0) || is.null(cor.exprs)) {
        stop("Usage: emr_cor([factor.expr, breaks]+, cor.exprs, include.lowest = F, right = T, stime = NULL, etime = NULL, iterator = NULL, keepref = F, filter = NULL)", call. = F)
    }
    .emr_checkroot()

    exprs <- c()
    breaks <- list()

    for (i in (0:(length(args) / 2 - 1))) {
        exprs <- append(exprs, args[[i * 2 + 1]])
        breaks[length(breaks) + 1] <- list(args[[i * 2 + 2]])
    }

    first_exprs <- exprs
    exprs <- append(exprs, cor.exprs)

    if (is.null(iterator)) {
        iterator <- expand_null_iterator(first_exprs)
    }

    if (is.character(iterator) && emr_track.is_logical(iterator)) {
        ltrack <- emr_logical_track.info(iterator)
        iterator <- ltrack$source
        filter <- create_logical_track_filter(ltrack, filter)
    }

    res <- .emr_call("emr_covariance", exprs, breaks, include.lowest, right, stime, etime, iterator, keepref, .emr_filter(filter), new.env(parent = parent.frame()))

    if (dataframe) {
        names <- names %||% first_exprs
        res <- purrr::imap(res, ~ {
            .x <- as.data.frame.table(.x)
            colnames(.x) <- c(names, "i", "j", .y)
            return(.x)
        })
        res <- purrr::reduce(res, dplyr::left_join, by = c(names, "i", "j"))
    }

    res
}



#' Calculates distribution of track expressions
#'
#' Calculates distribution of track expressions' values over the given set of
#' bins.
#'
#' This function calculates the distribution of values of the numeric track
#' expressions over the given set of bins.
#'
#' The range of bins is determined by 'breaks' argument. For example:
#' 'breaks=c(x1, x2, x3, x4)' represents three different intervals (bins): (x1,
#' x2], (x2, x3], (x3, x4].
#'
#' If the track expression constitutes of a categorical track or a virtual
#' track which source is a categorical track, the 'breaks' is allowed to be
#' 'NULL' meaning that the breaks are derived implicitly from the unique values
#' of the underlying track.
#'
#' 'emr_dist' can work with any number of dimensions. If more than one
#' 'expr'-'breaks' pair is passed, the result is a multidimensional vector, and
#' an individual value can be accessed by [i1,i2,...,iN] notation, where 'i1'
#' is the first track and 'iN' is the last track expression.
#'
#' @param expr track expression
#' @param breaks breaks that determine the bin or 'NULL'
#' @param include.lowest if 'TRUE', the lowest (or highest, for ‘right =
#' FALSE’) value of the range determined by breaks is included
#' @param right if 'TRUE' the intervals are closed on the right (and open on
#' the left), otherwise vice versa.
#' @param stime start time scope
#' @param etime end time scope
#' @param iterator track expression iterator. If 'NULL' iterator is determined
#' implicitly based on track expressions.
#' @param keepref If 'TRUE' references are preserved in the iterator.
#' @param filter Iterator filter.
#' @param dataframe return a data frame instead of an N-dimensional vector.
#' @param names names for track expressions in the returned dataframe (only relevant when \code{dataframe == TRUE})
#'
#' @return N-dimensional vector where N is the number of 'expr'-'breaks' pairs. If \code{dataframe == TRUE} - a data frame with a column for each track expression and an additional column 'n' with counts.
#' @seealso \code{\link{emr_cor}}, \code{\link{cut}}
#' @keywords ~distribution
#' @examples
#'
#' emr_db.init_examples()
#' emr_dist("sparse_track", c(0, 15, 20, 30, 40, 50), keepref = T)
#' emr_dist("sparse_track", c(0, 15, 20, 30, 40, 50), keepref = T, dataframe = TRUE)
#' @export emr_dist
emr_dist <- function(..., include.lowest = FALSE, right = TRUE, stime = NULL, etime = NULL, iterator = NULL, keepref = FALSE, filter = NULL, dataframe = FALSE, names = NULL) {
    args <- list(...)
    if (length(args) < 2 || (length(args) %% 2 != 0 && (length(args) - 1) %% 2 != 0)) {
        stop("Usage: emr_dist([expr, breaks]+, include.lowest = F, right = T, stime = NULL, etime = NULL, iterator = NULL, keepref = F, filter = NULL)", call. = F)
    }
    .emr_checkroot()

    exprs <- c()
    breaks <- list()

    for (i in (0:(length(args) / 2 - 1))) {
        exprs <- append(exprs, args[[i * 2 + 1]])
        breaks[length(breaks) + 1] <- list(args[[i * 2 + 2]])
    }

    if (is.null(iterator)) {
        iterator <- expand_null_iterator(exprs)
    }

    if (is.character(iterator) && emr_track.is_logical(iterator)) {
        ltrack <- emr_logical_track.info(iterator)
        iterator <- ltrack$source
        filter <- create_logical_track_filter(ltrack, filter)
    }

    res <- .emr_call("emr_dist", exprs, breaks, include.lowest, right, stime, etime, iterator, keepref, .emr_filter(filter), new.env(parent = parent.frame()))

    if (dataframe) {
        res <- as.data.frame.table(res)
        names <- names %||% exprs
        colnames(res) <- c(names, "n")
    }

    return(res)
}



#' Returns evaluated track expression
#'
#' Returns the result of track expressions evaluation for each of the iterator
#' points.
#'
#' This function returns the result of track expressions evaluation for each of
#' the iterator stops.
#'
#' If 'tidy' is 'TRUE' the returned value is a set of ID-Time points with two
#' additional columns named 'expr' and 'value'. 'expr' marks the track
#' expression that produced the value. Rows with NaN values are omitted from
#' the tidy format.
#'
#' If 'tidy' is 'FALSE' the returned value is a set of ID-Time points with an
#' additional column for the values of each of the track expressions.
#'
#' If 'sort' is 'TRUE' the returned value is sorted by id, time and reference,
#' otherwise the order is not guaranteed especially for longer runs, when
#' multitasking might be launched. Sorting requires additional time, so it is
#' switched off by default.
#'
#' 'names' parameter sets the labels for the track expressions in the return
#' value. If 'names' is 'NULL' the labels are set to the track expression
#' themselves.
#'
#' @param expr vector of track expressions
#' @param tidy if 'TRUE' result is returned in "tidy"" format
#' @param sort if 'TRUE' result is sorted by id, time and reference
#' @param names names for the track expressions in the returned value. If
#' 'NULL' names are set to the track expression themselves.
#' @param stime start time scope
#' @param etime end time scope
#' @param iterator track expression iterator. If 'NULL' iterator is determined
#' implicitly based on track expressions.
#' @param keepref If 'TRUE' references are preserved in the iterator.
#' @param filter Iterator filter.
#' @return A set of ID-Time points with additional columns depending on the
#' value of 'tidy' (see above).
#' @seealso \code{\link{emr_screen}}
#' @keywords ~extract
#' @examples
#'
#' emr_db.init_examples()
#' emr_extract("dense_track", stime = 1, etime = 3)
#' @export emr_extract
emr_extract <- function(expr, tidy = F, sort = F, names = NULL, stime = NULL, etime = NULL, iterator = NULL, keepref = F, filter = NULL) {
    if (missing(expr)) {
        stop("Usage: emr_extract(expr, tidy = F, sort = F, names = NULL, tidy = F, stime = NULL, etime = NULL, iterator = NULL, keepref = F, filter = NULL)", call. = F)
    }
    .emr_checkroot()

    if (is.null(iterator)) {
        iterator <- expand_null_iterator(expr)
    }

    if (is.character(iterator) && emr_track.is_logical(iterator)) {
        ltrack <- emr_logical_track.info(iterator)
        iterator <- ltrack$source
        filter <- create_logical_track_filter(ltrack, filter)
    }

    .emr_call("emr_extract", expr, names, tidy, sort, stime, etime, iterator, keepref, .emr_filter(filter), new.env(parent = parent.frame()))
}



#' Returns ids coverage per track
#'
#' Returns ids coverage per track.
#'
#' This function accepts a set of ids and a vector of categorical tracks. For
#' each track it calculates how many ids appear in the track. Each id is
#' counted only once.
#'
#' Ids can originate from a track or be provided within Ids Table.
#'
#' Note: The internal iterator that runs over each track is defined with
#' 'keepref=T'.
#'
#' @param ids track name or Ids Table
#' @param tracks a vector of track names
#' @param stime start time scope
#' @param etime end time scope
#' @param filter iterator filter
#' @return A vector containing the ids count for each track.
#' @seealso \code{\link{emr_ids_vals_coverage}}, \code{\link{emr_track.ids}},
#' \code{\link{emr_dist}}
#' @keywords ~coverage
#' @examples
#'
#' emr_db.init_examples()
#' emr_ids_coverage(data.frame(id = c(15, 24, 27)), "categorical_track")
#' @export emr_ids_coverage
emr_ids_coverage <- function(ids, tracks, stime = NULL, etime = NULL, filter = NULL) {
    if (missing(ids) || missing(tracks)) {
          stop("Usage: emr_ids_coverage(ids, tracks, stime = NULL, etime = NULL, filter = NULL)", call. = F)
      }
    .emr_checkroot()

    if (is.null(stime) && is.null(etime) && is.null(filter)) {
          .emr_call("emr_ids_dist", ids, tracks, new.env(parent = parent.frame()))
      } else {
        if (is.null(filter)) {
              filter <- "..emr_tmp_filter"
          } else {
              filter <- paste0("(", filter, ")", "& ..emr_tmp_filter")
          }

        if (is.character(ids)) { # ids is a name of the track
              assign("..emr_tmp_filter", emr_track.ids(ids), envir = .GlobalEnv)
          } else {
              assign("..emr_tmp_filter", data.frame(id = unique(ids$id)), envir = .GlobalEnv)
          }

        tryCatch(
            {
                .emr_call("emr_ids_dist_with_iterator", ids, tracks, stime, etime, .emr_filter(filter), new.env(parent = parent.frame()))
            },
            finally = {
                rm("..emr_tmp_filter", envir = .GlobalEnv)
            }
        )
    }
}



#' Returns ids coverage per value track
#'
#' Returns ids coverage per value track.
#'
#' This function accepts a set of ids and a vector of categorical tracks. For
#' each track value it calculates how many ids share this value. Each id is
#' counted only once. A data frame with 3 columns 'track', 'val' and 'count' is
#' returned.
#'
#' Ids can originate from a track or be provided within Ids Table.
#'
#' Note: The internal iterator that runs over each track is defined with
#' 'keepref=T'.
#'
#' @param ids track name or Ids Table
#' @param tracks a vector of track names
#' @param stime start time scope
#' @param etime end time scope
#' @param filter iterator filter
#' @return A data frame containing the number of ids for each track value.
#' @seealso \code{\link{emr_ids_coverage}}, \code{\link{emr_track.ids}},
#' \code{\link{emr_dist}}
#' @keywords ~coverage
#' @examples
#'
#' emr_db.init_examples()
#' emr_ids_vals_coverage(data.frame(id = c(15, 24, 27)), "categorical_track")
#' @export emr_ids_vals_coverage
emr_ids_vals_coverage <- function(ids, tracks, stime = NULL, etime = NULL, filter = NULL) {
    if (missing(ids) || missing(tracks)) {
          stop("Usage: emr_ids_vals_coverage(ids, tracks, stime = NULL, etime = NULL, filter = NULL)", call. = F)
      }
    .emr_checkroot()

    if (is.null(filter)) {
          filter <- "..emr_tmp_filter"
      } else {
          filter <- paste0("(", filter, ")", "& ..emr_tmp_filter")
      }

    if (is.character(ids)) { # ids is a name of the track
          assign("..emr_tmp_filter", emr_track.ids(ids), envir = .GlobalEnv)
      } else {
          assign("..emr_tmp_filter", data.frame(id = unique(ids$id)), envir = .GlobalEnv)
      }

    tryCatch(
        {
            .emr_call("emr_ids_vals_dist", ids, tracks, stime, etime, .emr_filter(filter), new.env(parent = parent.frame()))
        },
        finally = {
            rm("..emr_tmp_filter", envir = .GlobalEnv)
        }
    )
}



#' Calculates quantiles of a track expression
#'
#' Calculates quantiles of a track expression for the given percentiles.
#'
#' This function calculates quantiles for the given percentiles.
#'
#' If data size exceeds the limit (see: 'getOption(emr_max.data.size)'), the
#' data is randomly sampled to fit the limit. A warning message is generated
#' then.
#'
#' @param expr track expression
#' @param percentiles an array of percentiles of quantiles in [0, 1] range
#' @param stime start time scope
#' @param etime end time scope
#' @param iterator track expression iterator. If 'NULL' iterator is determined
#' implicitly based on track expression.
#' @param keepref If 'TRUE' references are preserved in the iterator.
#' @param filter Iterator filter.
#' @return An array that represent quantiles.
#' @seealso \code{\link{emr_extract}}
#' @keywords ~quantiles ~percentiles
#' @examples
#'
#' emr_db.init_examples()
#' emr_quantiles("sparse_track", c(0.1, 0.6, 0.8))
#' @export emr_quantiles
emr_quantiles <- function(expr, percentiles = 0.5, stime = NULL, etime = NULL, iterator = NULL, keepref = F, filter = NULL) {
    if (missing(expr)) {
        stop("Usage: emr_quantiles(expr, percentiles = 0.5, stime = NULL, etime = NULL, iterator = NULL, keepref = F, filter = NULL)", call. = F)
    }
    .emr_checkroot()

    if (is.null(iterator)) {
        iterator <- expand_null_iterator(expr)
    }

    if (is.character(iterator) && emr_track.is_logical(iterator)) {
        ltrack <- emr_logical_track.info(iterator)
        iterator <- ltrack$source
        filter <- create_logical_track_filter(ltrack, filter)
    }

    .emr_call("emr_quantiles", expr, percentiles, stime, etime, iterator, keepref, .emr_filter(filter), new.env(parent = parent.frame()))
}



#' Finds Id-Time points that match track expression
#'
#' Finds all patient-time pairs where track expression is 'TRUE'.
#'
#' This function finds all Id-Time points where track expression's value is
#' 'TRUE'.
#'
#' If 'sort' is 'TRUE' the returned value is sorted by id, time and reference,
#' otherwise the order is not guaranteed especially for longer runs, when
#' multitasking might be launched. Sorting requires additional time, so it is
#' switched off by default.
#'
#' @param expr logical track expression
#' @param sort if 'TRUE' result is sorted by id, time and reference
#' @param stime start time scope
#' @param etime end time scope
#' @param iterator track expression iterator. If 'NULL' iterator is determined
#' implicitly based on track expression.
#' @param keepref If 'TRUE' references are preserved in the iterator.
#' @param filter Iterator filter.
#' @return A set of Id-Time points that match track expression.
#' @seealso \code{\link{emr_extract}}
#' @keywords ~screen
#' @examples
#'
#' emr_db.init_examples()
#' emr_screen("sparse_track == 13 | dense_track < 80",
#'     iterator = "sparse_track", keepref = T
#' )
#' @export emr_screen
emr_screen <- function(expr, sort = F, stime = NULL, etime = NULL, iterator = NULL, keepref = F, filter = NULL) {
    if (missing(expr)) {
        stop("Usage: emr_screen(expr, sort = F, stime = NULL, etime = NULL, iterator = NULL, keepref = F, filter = NULL)", call. = F)
    }
    .emr_checkroot()

    if (is.null(iterator)) {
        iterator <- expand_null_iterator(expr)
    }

    if (is.character(iterator) && emr_track.is_logical(iterator)) {
        ltrack <- emr_logical_track.info(iterator)
        iterator <- ltrack$source
        filter <- create_logical_track_filter(ltrack, filter)
    }

    .emr_call("emr_screen", expr, sort, stime, etime, iterator, keepref, .emr_filter(filter), new.env(parent = parent.frame()))
}



#' Calculates summary statistics of track expression
#'
#' Calculates summary statistics of track expression.
#'
#' This function returns summary statistics of a track expression: total number
#' of values, number of NaN values, min, max, sum, mean and standard deviation
#' of the values.
#'
#' @param expr track expression.
#' @param stime start time scope.
#' @param etime end time scope.
#' @param iterator track expression iterator. If 'NULL' iterator is determined
#' implicitly based on track expressions.
#' @param keepref If 'TRUE' references are preserved in the iterator.
#' @param filter Iterator filter.
#' @return An array that represents summary statistics.
#' @seealso \code{\link{emr_track.info}}
#' @keywords ~summary ~statistics
#' @examples
#'
#' emr_db.init_examples()
#' emr_summary("sparse_track")
#' @export emr_summary
emr_summary <- function(expr, stime = NULL, etime = NULL, iterator = NULL, keepref = F, filter = NULL) {
    if (missing(expr)) {
        stop("Usage: emr_summary(expr, stime = NULL, etime = NULL, iterator = NULL, keepref = F, filter = NULL)", call. = F)
    }
    .emr_checkroot()

    if (is.null(iterator)) {
        iterator <- expand_null_iterator(expr)
    }

    if (is.character(iterator) && emr_track.is_logical(iterator)) {
        ltrack <- emr_logical_track.info(iterator)
        iterator <- ltrack$source
        filter <- create_logical_track_filter(ltrack, filter)
    }

    .emr_call("emr_summary", expr, stime, etime, iterator, keepref, .emr_filter(filter), new.env(parent = parent.frame()))
}
