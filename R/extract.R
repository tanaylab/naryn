
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
#' @inheritSection emr_extract iterator
#'
#' @param ... pairs of [factor.expr, breaks], where \code{factor.expr} is the  track expression and breaks are the breaks that determine the bin or 'NULL'.
#' @param cor.exprs vector of track expressions for which correlation
#' statistics is calculated.
#' @param include.lowest if 'TRUE', the lowest (or highest, for 'right = FALSE') value of the range determined by breaks is included.
#' @param right if 'TRUE' the intervals are closed on the right (and open on
#' the left), otherwise vice versa.
#' @param stime start time scope.
#' @param etime end time scope.
#' @param iterator track expression iterator. If 'NULL' iterator is determined
#' implicitly based on track expressions. See also 'iterator' section.
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
#'     include.lowest = TRUE, iterator = "categorical_track",
#'     keepref = TRUE
#' )
#' emr_cor("categorical_track", c(0, 2, 5),
#'     cor.exprs = c("sparse_track", "1/dense_track"),
#'     include.lowest = TRUE, iterator = "categorical_track",
#'     keepref = TRUE,
#'     dataframe = TRUE
#' )
#' @export emr_cor
emr_cor <- function(..., cor.exprs = NULL, include.lowest = FALSE, right = TRUE, stime = NULL, etime = NULL, iterator = NULL, keepref = FALSE, filter = NULL, dataframe = FALSE, names = NULL) {
    args <- list(...)
    if (length(args) < 2 || (length(args) %% 2 != 0 && (length(args) - 1) %% 2 != 0) || is.null(cor.exprs)) {
        stop("Usage: emr_cor([factor.expr, breaks]+, cor.exprs, include.lowest = FALSE, right = TRUE, stime = NULL, etime = NULL, iterator = NULL, keepref = FALSE, filter = NULL)", call. = FALSE)
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

    orig_filters <- .emr_gen_vtrack_filters(filter, iterator, keepref, stime, etime)
    on.exit(.emr_recreate_vtrack_filters(orig_filters))

    res <- .emr_call("emr_covariance", exprs, breaks, include.lowest, right, stime, etime, iterator, keepref, filter, new.env(parent = parent.frame()))

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
#' @inheritSection emr_extract iterator
#'
#' @param ... pairs of [expr, breaks], where \code{expr} is the  track expression and breaks are the breaks that determine the bin or 'NULL'.
#' @param include.lowest if 'TRUE', the lowest (or highest, for 'right = FALSE') value of the range determined by breaks is included
#' @param right if 'TRUE' the intervals are closed on the right (and open on
#' the left), otherwise vice versa.
#' @param stime start time scope
#' @param etime end time scope
#' @param iterator track expression iterator. If 'NULL' iterator is determined
#' implicitly based on track expressions. See also 'iterator' section.
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
#' emr_dist("sparse_track", c(0, 15, 20, 30, 40, 50), keepref = TRUE)
#' emr_dist("sparse_track", c(0, 15, 20, 30, 40, 50), keepref = TRUE, dataframe = TRUE)
#' @export emr_dist
emr_dist <- function(..., include.lowest = FALSE, right = TRUE, stime = NULL, etime = NULL, iterator = NULL, keepref = FALSE, filter = NULL, dataframe = FALSE, names = NULL) {
    args <- list(...)
    if (length(args) < 2 || (length(args) %% 2 != 0 && (length(args) - 1) %% 2 != 0)) {
        stop("Usage: emr_dist([expr, breaks]+, include.lowest = FALSE, right = TRUE, stime = NULL, etime = NULL, iterator = NULL, keepref = FALSE, filter = NULL)", call. = FALSE)
    }
    .emr_checkroot()

    exprs <- c()
    breaks <- list()

    for (i in (0:(length(args) / 2 - 1))) {
        exprs <- append(exprs, args[[i * 2 + 1]])
        breaks[length(breaks) + 1] <- list(args[[i * 2 + 2]])
    }

    orig_filters <- .emr_gen_vtrack_filters(filter, iterator, keepref, stime, etime)
    on.exit(.emr_recreate_vtrack_filters(orig_filters))

    res <- .emr_call("emr_dist", exprs, breaks, include.lowest, right, stime, etime, iterator, keepref, filter, new.env(parent = parent.frame()))

    if (dataframe) {
        res <- as.data.frame.table(res)
        names <- names %||% exprs
        colnames(res) <- c(names, "n")
    }

    return(res)
}

.emr_parse_exprs <- function(expr){
    res <- c()
    if (!is.null(expr)) {
        res <- all.vars(as.list(parse(text = expr))[[1]])
    }
    return(res)
}

#' The function overrides the filters which are applied on vtracks,
#' It uses the queries iterator to extract the vtrack expression and
#' creates a new operator filter based on the extract result.
#' The function returns the original information of filters passed, 
#' the original filters can be later sent to .emr_recreate_vtrack_filters, 
#' as the name suggests, the function recreates the original filters.
#' 
#' @noRd
.emr_gen_vtrack_filters <- function(filter, iterator, keepref, stime, etime) {
    parsed_filters <- .emr_parse_exprs(filter)
    
    vtrack_filters <- purrr::keep(parsed_filters, ~{ emr_filter.exists(.x) && 
                                                     is.character(emr_filter.info(.x)$src) && 
                                                     emr_vtrack.exists(emr_filter.info(.x)$src)
                                                    })
                                                    
    orig_vt_filters <- purrr::map(vtrack_filters, ~{info <- emr_filter.info(.x); info$filter <- .x; return(info)})                                                
    vtrack_filter_names_value <- purrr::keep(vtrack_filters, ~{ !is.null(emr_filter.info(.x)$val) }) 
    vtrack_filter_names_no_value <- purrr::keep(vtrack_filters, ~{ is.null(emr_filter.info(.x)$val) })

    vtracks <- purrr::map_chr(vtrack_filter_names_value, ~{ emr_filter.info(.x)$src })

    if (length(vtracks) > 0 && is.null(iterator)) {
        stop("NULL iterator is not allowed when there are filters on vtracks") 
    }

    if (length(vtracks) > 0) {
        vtrack_filters <- emr_extract(vtracks, iterator = iterator, keepref = keepref, stime = stime, etime = etime)
    }
    
    purrr::walk2(vtrack_filter_names_value, vtracks, ~{
        orig_filter <- emr_filter.info(.x)
        # If we get here use_values is TRUE since otherwise we wouldn't have to extract
        emr_filter.create(filter=.x, 
                          src=vtrack_filters %>% dplyr::select(id, time, ref, value=!!.y) %>% na.omit(), 
                          time.shift=orig_filter$time_shift, 
                          val=orig_filter$val, 
                          expiration=orig_filter$expiration, 
                          operator=orig_filter$operator,
                          use_values=TRUE
                    )
    })

    purrr::walk(vtrack_filter_names_no_value, ~{
        orig_filter <- emr_filter.info(.x)
        vtrack <- emr_vtrack.info(orig_filter$src)
        emr_filter.create(.x, 
                          src=vtrack$src, 
                          time.shift=orig_filter$time_shift, 
                          expiration=orig_filter$expiration)
    })

    return(orig_vt_filters)
}

#' The function receives the output of .emr_gen_vtrack_filters
#' and reverts the filters to there old, original form.
#' 
#' @noRd
.emr_recreate_vtrack_filters <- function(orig_filters) {
    purrr::walk(orig_filters, ~{
        emr_filter.create(
        filter = .x$filter,
        src = .x$src,
        time.shift = .x$time_shift,
        val = .x$val,
        expiration = .x$expiration,
        operator = .x$operator,
        use_values = .x$use_values
    )})
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
#' @section iterator:
#'
#' There are a few types of iterators:
#' \itemize{
#'  \item{Track iterator: }{Track iterator returns the points (including the reference) from the specified track. Track name is specified as a string. If `keepref=FALSE` the reference of each point is set to `-1` \cr
#' Example: \cr \cr
#' # Returns the level of glucose one hour after the insulin shot was made \cr
#' emr_vtrack.create("glucose", "glucose_track", func="avg", time.shift=1) \cr
#' emr_extract("glucose", iterator="insulin_shot_track") \cr
#' }
#'  \item{Id-Time Points Iterator: }{Id-Time points iterator generates points from an *id-time points table*. If `keepref=FALSE` the reference of each point is set to `-1`. \cr
#' Example: \cr \cr
#' # Returns the level of glucose one hour after the insulin shot was made \cr
#' emr_vtrack.create("glucose", "glucose_track", func = "avg", time.shift = 1) \cr
#' r <- emr_extract("insulin_shot_track") # <-- implicit iterator is used here \cr
#' emr_extract("glucose", iterator = r) \cr
#' }
#'  \item{Ids Iterator: }{Ids iterator generates points with ids taken from an *ids table* and times that run from `stime` to `etime` with a step of 1. If `keepref=TRUE` for each id-time pair the iterator generates 255 points with references running from `0` to `254`. If `keepref=FALSE` only one point is generated for the given id and time, and its reference is set to `-1`.\cr
#' Example: \cr \cr
#' stime <- emr_date2time(1, 1, 2016, 0) \cr
#' etime <- emr_date2time(31, 12, 2016, 23) \cr
#' emr_extract("glucose", iterator = data.frame(id = c(2, 5)), stime = stime, etime = etime)\cr
#' }
#' \item{Time Intervals Iterator: }{*Time intervals iterator* generates points for all the ids that appear in 'patients.dob' track with times taken from a *time intervals table* (see: Appendix). Each time starts at the beginning of the time interval and runs to the end of it with a step of 1. That being said the points that lie outside of `[stime, etime]` range are skipped. \cr
#' If `keepref=TRUE` for each id-time pair the iterator generates 255 points with references running from `0` to `254`. If `keepref=FALSE` only one point is generated for the given id and time, and its reference is set to `-1`. \cr
#' Example: \cr
#' # Returns the level of hangover for all patients the next day after New Year Eve for the years 2015 and 2016 \cr
#' stime1 <- emr_date2time(1, 1, 2015, 0) \cr
#' etime1 <- emr_date2time(1, 1, 2015, 23) \cr
#' stime2 <- emr_date2time(1, 1, 2016, 0) \cr
#' etime2 <- emr_date2time(1, 1, 2016, 23) \cr
#' emr_extract("alcohol_level_track", iterator = data.frame( \cr
#'     stime = c(stime1, stime2), \cr
#'     etime = c(etime1, etime2) \cr
#' )) \cr
#' }
#' \item{Id-Time Intervals Iterator: }{*Id-Time intervals iterator* generates for each id points that cover `['stime', 'etime']` time range as specified in *id-time intervals table* (see: Appendix). Each time starts at the beginning of the time interval and runs to the end of it with a step of 1. That being said the points that lie outside of `[stime, etime]` range are skipped. \cr
#' If `keepref=TRUE` for each id-time pair the iterator generates 255 points with references running from `0` to `254`. If `keepref=FALSE` only one point is generated for the given id and time, and its reference is set to `-1`}
#' \item{Beat Iterator: }{*Beat Iterator* generates a "time beat" at the given period for each id that appear in 'patients.dob' track. The period is given always in hours. \cr
#' Example: \cr
#' emr_extract("glucose_track", iterator=10, stime=1000, etime=2000) \cr
#' This will create a beat iterator with a period of 10 hours starting at `stime` up until `etime` is reached. If, for example, `stime` equals `1000` then the beat iterator will create for each id iterator points at times: 1000, 1010, 1020, ... \cr
#' If `keepref=TRUE` for each id-time pair the iterator generates 255 points with references running from `0` to `254`. If `keepref=FALSE` only one point is generated for the given id and time, and its reference is set to `-1`.
#' }
#' \item{Extended Beat Iterator: }{*Extended beat iterator* is as its name suggests a variation on the beat iterator. It works by the same principle of creating time points with the given period however instead of basing the times count on `stime` it accepts an additional parameter - a track or a *Id-Time Points table* - that instructs what should be the initial time point for each of the ids. The two parameters (period and mapping) should come in a list. Each id is required to appear only once and if a certain id does not appear at all, it is skipped by the iterator. \cr
#' Anyhow points that lie outside of `[stime, etime]` range are not generated. \cr
#' Example: \cr
#' # Returns the maximal weight of patients at one year span starting from their birthdays \cr
#' emr_vtrack.create("weight", "weight_track", func = "max", time.shift = c(0, year())) \cr
#' emr_extract("weight", iterator = list(year(), "birthday_track"), stime = 1000, etime = 2000) \cr
#' }
#' \item{Periodic Iterator: }{periodic iterator goes over every year/month. You can use it by running  \code{emr_monthly_iterator} or \code{emr_yearly_iterator}. \cr
#' Example: \cr
#' iter <- emr_yearly_iterator(emr_date2time(1, 1, 2002), emr_date2time(1, 1, 2017)) \cr
#' emr_extract("dense_track", iterator = iter, stime = 1, etime = 3) \cr
#' iter <- emr_monthly_iterator(emr_date2time(1, 1, 2002), n = 15) \cr
#' emr_extract("dense_track", iterator = iter, stime = 1, etime = 3) \cr
#' }
#' \item{Implicit Iterator: }{The iterator is set implicitly if its value remains `NULL` (which is the default). In that case the track expression is analyzed and searched for track names. If all the track variables or virtual track variables point to the same track, this track is used as a source for a track iterator. If more then one track appears in the track expression, an error message is printed out notifying ambiguity.}
#' }
#'
#' Revealing Current Iterator Time:
#' During the evaluation of a track expression one can access a specially defined variable named `EMR_TIME` (Python: `TIME`). This variable contains a vector (`numpy.ndarray` in Python) of current iterator times. The length of the vector matches the length of the track variable (which is a vector too). \cr
#' Note that some values in `EMR_TIME` might be set 0. Skip those intervals and the values of the track variables at the corresponding indices. \cr
#' # Returns times of the current iterator as a day of month \cr
#' emr_extract("emr_time2dayofmonth(EMR_TIME)", iterator = "sparse_track") \cr
#'
#' @param expr vector of track expressions
#' @param tidy if 'TRUE' result is returned in "tidy"" format
#' @param sort if 'TRUE' result is sorted by id, time and reference
#' @param names names for the track expressions in the returned value. If
#' 'NULL' names are set to the track expression themselves.
#' @param stime start time scope
#' @param etime end time scope
#' @param iterator track expression iterator. If 'NULL' iterator is determined
#' implicitly based on track expressions. See also 'iterator' section.
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
emr_extract <- function(expr, tidy = FALSE, sort = FALSE, names = NULL, stime = NULL, etime = NULL, iterator = NULL, keepref = FALSE, filter = NULL) {
    if (missing(expr)) {
        stop("Usage: emr_extract(expr, tidy = FALSE, sort = FALSE, names = NULL, tidy = FALSE, stime = NULL, etime = NULL, iterator = NULL, keepref = FALSE, filter = NULL)", call. = FALSE)
    }
    .emr_checkroot()

    orig_filters <- .emr_gen_vtrack_filters(filter, iterator, keepref, stime, etime)
    on.exit(.emr_recreate_vtrack_filters(orig_filters))
    
    .emr_call("emr_extract", expr, names, tidy, sort, stime, etime, iterator, keepref, filter, new.env(parent = parent.frame()))
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
#' 'keepref=TRUE'.
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
        stop("Usage: emr_ids_coverage(ids, tracks, stime = NULL, etime = NULL, filter = NULL)", call. = FALSE)
    }
    .emr_checkroot()

    orig_tracks <- tracks
    res_logical <- list()
    res <- list()
    orig_filters <- .emr_gen_vtrack_filters(filter, iterator, keepref, stime, etime)
    on.exit(.emr_recreate_vtrack_filters(orig_filters))

    for (track in tracks) {
        if (emr_track.logical.exists(track)) {
            ltrack <- emr_track.logical.info(track)
            res_logical[[track]] <-
                emr_ids_coverage(
                    ids,
                    ltrack$source,
                    filter = create_logical_track_filter(track, filter), stime = stime,
                    etime = etime
                )[[1]]
        }
    }

    tracks <- tracks[!(tracks %in% names(res_logical))]

    if (length(tracks) > 0) {
        if (is.null(stime) && is.null(etime) && is.null(filter)) {
            res <- .emr_call("emr_ids_dist", ids, tracks, new.env(parent = parent.frame()))
        } else {
            if (is.null(filter)) {
                filter <- "..emr_tmp_filter"
            } else {
                filter <- paste0("(", filter, ")", " & ..emr_tmp_filter")
            }

            if (is.character(ids)) { # ids is a name of the track
                track_ids <- emr_track.ids(ids)
                assign("..emr_tmp_filter", track_ids, envir = .GlobalEnv)
                if (emr_track.logical.exists(ids)) {
                    ids <- track_ids
                }
            } else {
                assign("..emr_tmp_filter", data.frame(id = unique(ids$id)), envir = .GlobalEnv)
            }

            tryCatch(
                {
                    res <- .emr_call("emr_ids_dist_with_iterator", ids, tracks, stime, etime, filter, new.env(parent = parent.frame()))
                },
                finally = {
                    rm("..emr_tmp_filter", envir = .GlobalEnv)
                }
            )
        }
    }

    res <- c(res, res_logical)

    res <- res[orig_tracks]
    res <- unlist(res)
    return(res)
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
#' 'keepref=TRUE'.
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
        stop("Usage: emr_ids_vals_coverage(ids, tracks, stime = NULL, etime = NULL, filter = NULL)", call. = FALSE)
    }
    .emr_checkroot()

    orig_filters <- .emr_gen_vtrack_filters(filter, iterator, keepref, stime, etime)
    on.exit(.emr_recreate_vtrack_filters(orig_filters))

    logical_tracks <- tracks[purrr::map_lgl(tracks, emr_track.logical.exists)]
    physical_tracks <- tracks[!(tracks %in% logical_tracks)]

    res_logical <- data.frame(track = character(), val = numeric(), count = numeric())
    res_physical <- data.frame(track = character(), val = numeric(), count = numeric())

    if (length(logical_tracks) > 0) {
        res_logical <- purrr::map_dfr(logical_tracks, function(track) {
            ltrack <- emr_track.logical.info(track)
            res <- emr_ids_vals_coverage(
                ids,
                ltrack$source,
                filter = create_logical_track_filter(track, filter), stime = stime,
                etime = etime
            )
            res$track <- track
            res <- res %>% dplyr::filter(val %in% ltrack$values)
            return(res)
        })
    }

    if (length(physical_tracks) > 0) {
        if (is.null(filter)) {
            filter <- "..emr_tmp_filter"
        } else {
            filter <- paste0("(", filter, ")", "& ..emr_tmp_filter")
        }

        if (is.character(ids)) { # ids is a name of the track
            track_ids <- emr_track.ids(ids)
            assign("..emr_tmp_filter", track_ids, envir = .GlobalEnv)
            if (emr_track.logical.exists(ids)) {
                ids <- track_ids
            }
        } else {
            assign("..emr_tmp_filter", data.frame(id = unique(ids$id)), envir = .GlobalEnv)
        }

        tryCatch(
            {
                res_physical <- .emr_call("emr_ids_vals_dist", ids, physical_tracks, stime, etime, filter, new.env(parent = parent.frame()))
            },
            finally = {
                rm("..emr_tmp_filter", envir = .GlobalEnv)
            }
        )
    }

    res <- rbind(res_physical, res_logical)
    res <- res %>%
        dplyr::mutate(track = factor(track, levels = tracks)) %>%
        dplyr::arrange(track)
    return(res)
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
#' @inheritSection emr_extract iterator
#'
#' @param expr track expression
#' @param percentiles an array of percentiles of quantiles in [0, 1] range
#' @param stime start time scope
#' @param etime end time scope
#' @param iterator track expression iterator. If 'NULL' iterator is determined
#' implicitly based on track expression. See also 'iterator' section.
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
emr_quantiles <- function(expr, percentiles = 0.5, stime = NULL, etime = NULL, iterator = NULL, keepref = FALSE, filter = NULL) {
    if (missing(expr)) {
        stop("Usage: emr_quantiles(expr, percentiles = 0.5, stime = NULL, etime = NULL, iterator = NULL, keepref = FALSE, filter = NULL)", call. = FALSE)
    }
    .emr_checkroot()

    orig_filters <- .emr_gen_vtrack_filters(filter, iterator, keepref, stime, etime)
    on.exit(.emr_recreate_vtrack_filters(orig_filters))

    .emr_call("emr_quantiles", expr, percentiles, stime, etime, iterator, keepref, filter, new.env(parent = parent.frame()))
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
#' @inheritSection emr_extract iterator
#'
#' @param expr logical track expression
#' @param sort if 'TRUE' result is sorted by id, time and reference
#' @param stime start time scope
#' @param etime end time scope
#' @param iterator track expression iterator. If 'NULL' iterator is determined
#' implicitly based on track expression. See also 'iterator' section.
#' @param keepref If 'TRUE' references are preserved in the iterator.
#' @param filter Iterator filter.
#' @return A set of Id-Time points that match track expression.
#' @seealso \code{\link{emr_extract}}
#' @keywords ~screen
#' @examples
#'
#' emr_db.init_examples()
#' emr_screen("sparse_track == 13 | dense_track < 80",
#'     iterator = "sparse_track", keepref = TRUE
#' )
#' @export emr_screen
emr_screen <- function(expr, sort = FALSE, stime = NULL, etime = NULL, iterator = NULL, keepref = FALSE, filter = NULL) {
    if (missing(expr)) {
        stop("Usage: emr_screen(expr, sort = FALSE, stime = NULL, etime = NULL, iterator = NULL, keepref = FALSE, filter = NULL)", call. = FALSE)
    }
    .emr_checkroot()

    orig_filters <- .emr_gen_vtrack_filters(filter, iterator, keepref, stime, etime)
    on.exit(.emr_recreate_vtrack_filters(orig_filters))

    .emr_call("emr_screen", expr, sort, stime, etime, iterator, keepref, filter, new.env(parent = parent.frame()))
}



#' Calculates summary statistics of track expression
#'
#' Calculates summary statistics of track expression.
#'
#' This function returns summary statistics of a track expression: total number
#' of values, number of NaN values, min, max, sum, mean and standard deviation
#' of the values.
#'
#' @inheritSection emr_extract iterator
#'
#' @param expr track expression.
#' @param stime start time scope.
#' @param etime end time scope.
#' @param iterator track expression iterator. If 'NULL' iterator is determined
#' implicitly based on track expressions. See also 'iterator' section.
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
emr_summary <- function(expr, stime = NULL, etime = NULL, iterator = NULL, keepref = FALSE, filter = NULL) {
    if (missing(expr)) {
        stop("Usage: emr_summary(expr, stime = NULL, etime = NULL, iterator = NULL, keepref = FALSE, filter = NULL)", call. = FALSE)
    }
    .emr_checkroot()

    orig_filters <- .emr_gen_vtrack_filters(filter, iterator, keepref, stime, etime)
    on.exit(.emr_recreate_vtrack_filters(orig_filters))

    .emr_call("emr_summary", expr, stime, etime, iterator, keepref, filter, new.env(parent = parent.frame()))
}
