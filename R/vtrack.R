#' Get virtual track parameters given a string
#'
#' @param vtrackstr name of the virtual track
#' @param adjust_logical when the source is logical track - adjust the parameters to imitate a physical track
#'
#' @export
#' @noRd
.emr_vtrack.get <- function(vtrackstr, adjust_logical = TRUE) {
    if (!emr_vtrack.exists(vtrackstr)) {
        stop(sprintf("Virtual track %s does not exist", vtrackstr), call. = FALSE)
    }

    vtrack <- get("EMR_VTRACKS", envir = .GlobalEnv)[[vtrackstr]]

    if (adjust_logical) {
        if (!is.null(vtrack$logical)) {
            vtrack$src <- vtrack$logical$src
            vtrack$params <- vtrack$logical$params
        }
        vtrack$logical <- NULL
    }
    vtrack
}

#' Adjusts the params for a vtrack on a logical track
#'
#' Explanation:
#' The params for a vtrack on a logical track
#' are the intersection between the params
#' requested and the values of the logical
#' track, we choose the intersection in order
#' to eliminate params which are not included
#' in the logical track values but might be
#' included in the source of the logical track.
#' This may cause the return of unwanted data in
#' some keepref related situations.
#' When the intersection is empty, we set the
#' params to NA in order to immitate a case where
#' the param chosen is outside the scope of the
#' track's values. When the source is numeric,
#' the logical track serves as an alias, and params
#' should be set to NULL.
#' @noRd
.emr_vtrack_calc_logical_params <- function(src, params) {
    ltrack_info <- emr_track.logical.info(src)
    is_categorical <- emr_track.info(src)$categorical

    if (!is_categorical) {
        return(params)
    }

    if (is.null(ltrack_info$values)) {
        return(NULL)
    }

    if (is.null(params)) {
        params <- ltrack_info$values
    }

    params <- params[params %in% ltrack_info$values]

    if (length(params) == 0) {
        params <- NA
    }

    return(params)
}



#' Creates a new virtual track
#'
#' Creates a new virtual track.
#'
#' This function creates a new virtual track named 'vtrack'.
#'
#' During the evaluation of track expression that contains a virtual track
#' 'vtrack' the iterator point of id-time (ID1, Time, Ref) form is transformed
#' first to an id-time interval: (ID2, Time1, Time2, Ref).
#'
#' If 'id.map' is 'NULL' then ID1 == ID2, otherwise ID2 is derived from the
#' translation table provided in 'id.map'. This table is a data frame with two
#' first columns named 'id1' and 'id2', where 'id1' is mapped to 'id2'. If
#' 'id.map' contains also a third optional column named 'time.shift' the value
#' V of this column is used to shift the time accordingly, i.e. Time1 = Time2 =
#' Time + V.
#'
#' 'time.shift' parameter (not to be confused with 'time.shift' column of
#' 'id.map') can be either a single number X, in which case Time1 = Time2 =
#' Time + X. Alternatively 'time.shift' can be a vector of two numbers, i.e.
#' 'c(X1, X2)', which would result in Time1 = Time + X1, Time2 = Time + X2.
#'
#' Both 'time.shift' parameter and 'time.shift' column within 'id.map' may be
#' used simultaneously. In this case the time shifts are applied sequentially.
#'
#' At the next step values from the data source 'src' that fall into the new
#' id-time interval and pass the 'filter' are collected. 'src' may be either a
#' track name or a list of two members: ID-Time Values table (see "User
#' Manual") and a logical. If the logical is 'TRUE', the data in the table is
#' treated as categorical, otherwise as quantitative.
#'
#' If 'keepref' is 'TRUE' the reference of these values must match 'ref' unless
#' either the reference or 'ref' are '-1'.
#'
#' Function 'func' (with 'params') is applied then on the collected values and
#' produces a single value which is considered to be the value of 'vtrack' for
#' the given iterator point. If 'NULL' is used as a value for 'func', 'func' is
#' set then implicitly to 'value', if the data source is categorical, or 'avg',
#' if the data source is quantitative.
#'
#' Use the following table for a reference of all valid functions and
#' parameters combinations.
#'
#' CATEGORICAL DATA SOURCE
#'
#' \tabular{lll}{ FUNC \tab PARAM \tab DESCRIPTION \cr value \tab vals/NULL
#' \tab A source value or -1 if there is more than one. \cr exists \tab vals
#' \tab 1 if any of the 'vals' exist otherwise 0. \cr sample \tab NULL \tab
#' Uniformly sampled source value. \cr sample.time \tab NULL \tab Time of the
#' uniformly sampled source value. \cr frequent \tab vals/NULL \tab The most
#' frequent source value or -1 if there is more than one value. \cr size \tab
#' vals/NULL \tab Number of values. \cr earliest \tab vals/NULL \tab Earliest
#' value or -1 if there is more than one. \cr latest \tab vals/NULL \tab Latest
#' value or -1 if there is more than one. \cr closest \tab vals/NULL \tab
#' Values closest to the middle of the interval or -1 if there is more than
#' one. \cr earliest.time \tab vals/NULL \tab Time of the earliest value. \cr
#' latest.time \tab vals/NULL \tab Time of the latest value. \cr
#' closest.earlier.time \tab vals/NULL \tab Time of the of the earlier of the
#' closest values. \cr closest.later.time \tab vals/NULL \tab Time of the of
#' the later of the closest values. \cr dt1.earliest \tab vals/NULL \tab Time
#' difference between the earliest value and T1 \cr dt1.latest \tab vals/NULL
#' \tab Time difference between the latest value and T1 \cr dt2.earliest \tab
#' vals/NULL \tab Time difference between T2 and the earliest value \cr
#' dt2.latest \tab vals/NULL \tab Time difference between T2 and the latest
#' value \cr }
#'
#' * 'vals' is a vector of values. If not 'NULL' it serves as a filter: the
#' function is applied only to the data source values that appear among 'vals'.
#' 'vals' can be a single NA value, in which case all the values of the track
#' would be filtered out.
#'
#' QUANTITATIVE DATA SOURCE
#'
#' \tabular{lll}{ FUNC \tab PARAM \tab DESCRIPTION \cr avg \tab NULL \tab
#' Average of all values. \cr min \tab NULL \tab Minimal value. \cr max \tab
#' NULL \tab Maximal value. \cr sample \tab NULL \tab Uniformly sampled source
#' value. \cr sample.time \tab NULL \tab Time of the uniformly sampled source
#' value. \cr size \tab NULL \tab Number of values. \cr earliest \tab NULL \tab
#' Average of the earliest values. \cr latest \tab NULL \tab Average of the
#' latest values. \cr closest \tab NULL \tab Average of values closest to the
#' middle of the interval. \cr stddev \tab NULL \tab Unbiased standard
#' deviation of the values. \cr sum \tab NULL \tab Sum of values. \cr quantile
#' \tab Percentile in the range of [0, 1] \tab Quantile of the values. \cr
#' percentile.upper \tab NULL \tab Average of upper-bound values percentiles.*
#' \cr percentile.upper.min \tab NULL \tab Minimum of upper-bound values
#' percentiles.* \cr percentile.upper.max \tab NULL \tab Maximum of upper-bound
#' values percentiles.* \cr percentile.lower \tab NULL \tab Average of
#' lower-bound values percentiles.* \cr percentile.lower.min \tab NULL \tab
#' Minimum of lower-bound values percentiles.* \cr percentile.lower.max \tab
#' NULL \tab Maximum of lower-bound values percentiles.* \cr lm.intercept \tab
#' NULL \tab Intercept (aka "alpha") of the simple linear regression (X = time,
#' Y = values)\cr lm.slope \tab NULL \tab Slope (aka "beta") of the simple
#' linear regression (X = time, Y = values)\cr earliest.time \tab NULL \tab
#' Time of the earliest value. \cr latest.time \tab NULL \tab Time of the
#' latest value. \cr closest.earlier.time \tab NULL \tab Time of the of the
#' earlier of the closest values. \cr closest.later.time \tab NULL \tab Time of
#' the of the later of the closest values. \cr dt1.earliest \tab NULL \tab Time
#' difference between the earliest value and T1 \cr dt1.latest \tab NULL \tab
#' Time difference between the latest value and T1 \cr dt2.earliest \tab NULL
#' \tab Time difference between T2 and the earliest value \cr dt2.latest \tab
#' NULL \tab Time difference between T2 and the latest value \cr }
#'
#' * Percentile is calculated based on the values of the whole data source even
#' if a subset or a filter are defined.
#'
#' Note: 'time.shift' can be used only when 'keepref' is 'FALSE'. Also when
#' 'keepref' is 'TRUE' only 'avg', 'percentile.upper' and 'percentile.lower'
#' can be used in 'func'.
#'
#' @param vtrack virtual track name.
#' @param src data source. either a track name or a list of two members: ID-Time Values table (see "User
#' Manual") and a logical. If the logical is 'TRUE', the data in the table is treated as categorical, otherwise as quantitative.
#' @param func,params see below.
#' @param keepref see below.
#' @param time.shift time shift and expansion for iterator time.
#' @param id.map id mapping.
#' @param filter virtual track filter. Note that filters with a source of another virtual track are not allowed in order to avoid loops.
#' @return Name of the virtual track (invisibly)
#' @seealso \code{\link{emr_vtrack.attr.src}}, \code{\link{emr_vtrack.ls}},
#' \code{\link{emr_vtrack.exists}}, \code{\link{emr_vtrack.rm}}
#' @keywords ~virtual
#' @examples
#'
#' emr_db.init_examples()
#'
#' emr_vtrack.create("vtrack1", "dense_track",
#'     time.shift = 1,
#'     func = "max"
#' )
#' emr_vtrack.create("vtrack2", "dense_track",
#'     time.shift = c(-5, 10), func = "min"
#' )
#' res <- emr_extract("dense_track", keepref = TRUE, names = "value")
#' emr_vtrack.create("vtrack3", list(res, FALSE),
#'     time.shift = c(-5, 10),
#'     func = "min"
#' )
#' emr_extract(c("dense_track", "vtrack1", "vtrack2", "vtrack3"),
#'     keepref = TRUE, iterator = "dense_track"
#' )
#' @export emr_vtrack.create
emr_vtrack.create <- function(vtrack, src, func = NULL, params = NULL, keepref = FALSE, time.shift = NULL, id.map = NULL, filter = NULL) {
    if (missing(vtrack) || missing(src)) {
        stop("Usage: emr_vtrack.create(vtrack, src, func = NULL, params = NULL, keepref = FALSE, time.shift = NULL, id.map = NULL, filter = NULL)", call. = FALSE)
    }
    .emr_checkroot()

    if (vtrack != make.names(vtrack)) {
        stop(sprintf("\"%s\" is not a syntactically valid name for a variable", vtrack), call. = FALSE)
    }

    if (!exists("EMR_VTRACKS", envir = .GlobalEnv)) {
        EMR_VTRACKS <<- list()
    }

    if (emr_track.exists(vtrack)) {
        stop(sprintf("Track %s already exists (you cannot create a virtual track named as am existing track)", vtrack), call. = FALSE)
    }

    if (emr_filter.exists(vtrack)) {
        stop(sprintf("Filter %s already exists (you cannot create a virtual track named as an existing filter)", vtrack), call. = FALSE)
    }

    if (!length(params) == 1 && any(is.na(params))) {
        stop("Invalid params used for vtrack. NA cannot be used as params together with other values")
    }

    logical <- NULL

    if (is.character(src) && emr_track.logical.exists(src)) {
        logical$params <- params
        logical$src <- src

        ltrack_info <- emr_track.logical.info(src)
        params <- .emr_vtrack_calc_logical_params(src, params)
        src <- ltrack_info$source
    }

    var <- list(src = src, time_shift = time.shift, func = func, params = params, keepref = keepref, id_map = id.map, filter = .emr_filter(filter), logical = logical)

    .emr_call("emr_check_vtrack", vtrack, var, new.env(parent = parent.frame()))
    emr_vtrack.rm(vtrack)
    EMR_VTRACKS[[vtrack]] <<- var

    invisible(vtrack)
}



#' Get or set attributes of a virtual track
#'
#' Get or set attributes of a virtual track.
#'
#' When only 'vtrack' argument is used in the call, the functions return the
#' corresponding attribute of the virtual track. Otherwise a new attribute
#' value is set.
#'
#' Note: since inter-dependency exists between certain attributes, the
#' correctness of the attributes as a whole can only be verified when the
#' virtual track is used in a track expression.
#'
#' For more information about the valid attribute values please refer to the
#' documentation of 'emr_vtrack.create'.
#'
#' @aliases emr_vtrack.attr.src emr_vtrack.attr.func emr_vtrack.attr.params
#' emr_vtrack.attr.keepref emr_vtrack.attr.time.shift emr_vtrack.attr.id.map
#' emr_vtrack.attr.filter
#' @param vtrack virtual track name.
#' @param src,func,params,keepref,time.shift,id.map,filter virtual track
#' attributes.
#' @return None.
#' @seealso \code{\link{emr_vtrack.create}}
#' @keywords ~virtual
#' @examples
#'
#' emr_db.init_examples()
#' emr_vtrack.create("vtrack1", "dense_track")
#' emr_vtrack.attr.src("vtrack1")
#' emr_vtrack.attr.src("vtrack1", "sparse_track")
#' emr_vtrack.attr.src("vtrack1")
#' @export emr_vtrack.attr.src
emr_vtrack.attr.src <- function(vtrack, src) {
    if (missing(vtrack)) {
        stop("Usage: emr_vtrack.attr.src(vtrack, src)", call. = FALSE)
    }
    .emr_checkroot()

    vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[vtrack]]

    if (is.null(vtrack.var)) {
        stop(sprintf("Virtual track \"%s\" does not exist", vtrack), call. = FALSE)
    }

    is_logical_vtrack <- !is.null(vtrack.var$logical)

    if (missing(src)) {
        if (is_logical_vtrack) {
            return(vtrack.var$logical$src)
        } else {
            return(vtrack.var$src)
        }
    } else if (is.character(src) && emr_track.logical.exists(src)) {
        emr_vtrack.rm(vtrack)
        vtrack.var$logical$src <- src

        if (!is_logical_vtrack) {
            vtrack.var$logical$params <- vtrack.var$params
        }

        vtrack.var$params <- .emr_vtrack_calc_logical_params(src, vtrack.var$logical$params)
        ltrack_info <- emr_track.logical.info(src)
        vtrack.var$src <- ltrack_info$source
    } else {
        .emr_call("emr_check_vtrack_attr_src", src, new.env(parent = parent.frame()))
        emr_vtrack.rm(vtrack)
        vtrack.var$src <- src
    }

    EMR_VTRACKS[[vtrack]] <<- vtrack.var
    return(NULL)
}


#' @export
#' @rdname emr_vtrack.attr.src
emr_vtrack.attr.func <- function(vtrack, func) {
    if (missing(vtrack)) {
        stop("Usage: emr_vtrack.attr.func(vtrack, func)", call. = FALSE)
    }
    .emr_checkroot()

    vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[vtrack]]

    if (is.null(vtrack.var)) {
        stop(sprintf("Virtual track \"%s\" does not exist", vtrack), call. = FALSE)
    }

    if (missing(func)) {
        vtrack.var$func
    } else {
        .emr_call("emr_check_vtrack_attr_func", func, new.env(parent = parent.frame()))
        EMR_VTRACKS[[vtrack]]["func"] <<- list(func)
        return(NULL)
    }
}

#' @export
#' @rdname emr_vtrack.attr.src
emr_vtrack.attr.params <- function(vtrack, params) {
    if (missing(vtrack)) {
        stop("Usage: emr_vtrack.attr.params(vtrack, params)", call. = FALSE)
    }

    vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[vtrack]]

    if (is.null(vtrack.var)) {
        stop(sprintf("Virtual track \"%s\" does not exist", vtrack), call. = FALSE)
    }

    is_logical_vtrack <- !is.null(vtrack.var$logical)

    if (missing(params)) {
        if (is_logical_vtrack) {
            return(vtrack.var$logical$params)
        } else {
            return(vtrack.var$params)
        }
    } else if (is_logical_vtrack) {
        vtrack.var$logical$params <- params
        params <- .emr_vtrack_calc_logical_params(vtrack.var$logical$src, params)
    }

    EMR_VTRACKS[[vtrack]]["params"] <<- list(params)
    return(NULL)
}

#' @export
#' @rdname emr_vtrack.attr.src
emr_vtrack.attr.keepref <- function(vtrack, keepref) {
    if (missing(vtrack)) {
        stop("Usage: emr_vtrack.attr.keepref(vtrack, keepref)", call. = FALSE)
    }

    vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[vtrack]]

    if (is.null(vtrack.var)) {
        stop(sprintf("Virtual track \"%s\" does not exist", vtrack), call. = FALSE)
    }

    if (missing(keepref)) {
        vtrack.var$keepref
    } else {
        if (!is.logical(keepref) || is.na(keepref)) {
            stop("'keepref' parameter must be logical", call. = FALSE)
        }

        EMR_VTRACKS[[vtrack]]["keepref"] <<- list(keepref)
        return(NULL)
    }
}

#' @export
#' @rdname emr_vtrack.attr.src
emr_vtrack.attr.time.shift <- function(vtrack, time.shift) {
    if (missing(vtrack)) {
        stop("Usage: emr_vtrack.attr.time.shift(vtrack, time.shift)", call. = FALSE)
    }

    vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[vtrack]]

    if (is.null(vtrack.var)) {
        stop(sprintf("Virtual track \"%s\" does not exist", vtrack), call. = FALSE)
    }

    if (missing(time.shift)) {
        vtrack.var$time_shift
    } else {
        .emr_call("emr_check_vtrack_attr_time_shift", time.shift, new.env(parent = parent.frame()))
        EMR_VTRACKS[[vtrack]]["time_shift"] <<- list(time.shift)
        return(NULL)
    }
}

#' @export
#' @rdname emr_vtrack.attr.src
emr_vtrack.attr.id.map <- function(vtrack, id.map) {
    if (missing(vtrack)) {
        stop("Usage: emr_vtrack.attr.id.map(vtrack, id.map)", call. = FALSE)
    }

    vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[vtrack]]

    if (is.null(vtrack.var)) {
        stop(sprintf("Virtual track \"%s\" does not exist", vtrack), call. = FALSE)
    }

    if (missing(id.map)) {
        vtrack.var$id.map
    } else {
        .emr_call("emr_check_vtrack_attr_id_map", id.map, new.env(parent = parent.frame()))
        EMR_VTRACKS[[vtrack]]["id.map"] <<- list(id.map)
        return(NULL)
    }
}

#' @export
#' @rdname emr_vtrack.attr.src
emr_vtrack.attr.filter <- function(vtrack, filter) {
    if (missing(vtrack)) {
        stop("Usage: emr_vtrack.attr.filter(vtrack, filter)", call. = FALSE)
    }

    vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[vtrack]]
    if (is.null(vtrack.var)) {
        stop(sprintf("Virtual track \"%s\" does not exist", vtrack), call. = FALSE)
    }

    if (missing(filter)) {
        vtrack.var$filter
    } else {
        .emr_call("emr_check_vtrack_attr_filter", .emr_filter(filter), new.env(parent = parent.frame()))
        EMR_VTRACKS[[vtrack]]["filter"] <<- list(.emr_filter(filter))
        return(NULL)
    }
}



#' Checks whether the virtual track exists
#'
#' Checks whether the virtual track exists.
#'
#' This function checks whether the virtual track exists.
#'
#' @param vtrack virtual track name
#' @return 'TRUE' if the virtual track exists, otherwise 'FALSE'.
#' @seealso \code{\link{emr_vtrack.create}}, \code{\link{emr_vtrack.ls}}
#' @keywords ~virtual ~exists
#' @examples
#'
#' emr_db.init_examples()
#' emr_vtrack.create("vtrack1", "dense_track", time.shift = c(5, 10), func = "max")
#' emr_vtrack.exists("vtrack1")
#' @export emr_vtrack.exists
emr_vtrack.exists <- function(vtrack) {
    if (missing(vtrack)) {
        stop("Usage: emr_vtrack.exists(vtrack)", call. = FALSE)
    }

    res <- FALSE
    if (exists("EMR_VTRACKS", envir = .GlobalEnv)) {
        vtracks <- get("EMR_VTRACKS", envir = .GlobalEnv)
        res <- !is.null(vtracks[[vtrack]])
    }
    res
}



#' Returns the definition of a virtual track
#'
#' Returns the definition of a virtual track.
#'
#' This function returns the internal representation of a virtual track.
#'
#' @param vtrack virtual track name
#' @return Internal representation of a virtual track.
#' @seealso \code{\link{emr_vtrack.create}}
#' @keywords ~virtual
#' @examples
#'
#' emr_db.init_examples()
#' emr_vtrack.create("vtrack1", "dense_track", "max", time.shift = c(5, 10))
#' emr_vtrack.info("vtrack1")
#' @export emr_vtrack.info
emr_vtrack.info <- function(vtrack) {
    if (missing(vtrack)) {
        stop("Usage: emr_vtrack.info(vtrack)", call. = FALSE)
    }

    .emr_vtrack.get(vtrack)
}



#' Returns a list of virtual track names
#'
#' Returns a list of virtual track names.
#'
#' This function returns a list of virtual tracks that exist in current R
#' environment that match the pattern (see 'grep'). If called without any
#' arguments all virtual tracks are returned.
#'
#' @param pattern,ignore.case,perl,fixed,useBytes see 'grep'
#' @return An array that contains the names of virtual tracks.
#' @seealso \code{\link{grep}}, \code{\link{emr_vtrack.exists}},
#' \code{\link{emr_vtrack.create}}, \code{\link{emr_vtrack.rm}}
#' @keywords ~virtual ~ls
#' @examples
#'
#' emr_db.init_examples()
#' emr_vtrack.create("vtrack1", "dense_track", func = "max")
#' emr_vtrack.create("vtrack2", "dense_track", func = "min")
#' emr_vtrack.ls()
#' emr_vtrack.ls("*2")
#' @export emr_vtrack.ls
emr_vtrack.ls <- function(pattern = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
    if (!exists("EMR_VTRACKS", envir = .GlobalEnv)) {
        return(NULL)
    }

    vtracks <- get("EMR_VTRACKS", envir = .GlobalEnv)
    vtracknames <- names(vtracks)

    if (!is.list(vtracks) || (length(vtracks) && !is.character(vtracknames)) || length(vtracks) != length(vtracknames)) {
        stop("Invalid format of EMR_VTRACKS variable.\nTo continue working with virtual tracks please remove this variable from the environment.", call. = FALSE)
    }

    if (is.null(vtracknames)) {
        return(character(0))
    }

    if (pattern != "") {
        sort(grep(pattern, vtracknames, value = TRUE, ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes))
    } else {
        sort(vtracknames)
    }
}



#' Deletes a virtual track
#'
#' Deletes a virtual track.
#'
#' This function deletes a virtual track from current R environment.
#'
#' @param vtrack virtual track name
#' @return None.
#' @seealso \code{\link{emr_vtrack.create}}, \code{\link{emr_vtrack.ls}}
#' @keywords ~virtual
#' @examples
#'
#' emr_db.init_examples()
#' emr_vtrack.create("vtrack1", "dense_track")
#' emr_vtrack.create("vtrack2", "dense_track")
#' emr_vtrack.ls()
#' emr_vtrack.rm("vtrack1")
#' emr_vtrack.ls()
#' @export emr_vtrack.rm
emr_vtrack.rm <- function(vtrack) {
    if (missing(vtrack)) {
        stop("Usage: emr_vtrack.rm(vtrack)", call. = FALSE)
    }

    if (exists("EMR_VTRACKS", envir = .GlobalEnv)) {
        emr_vtracks <- get("EMR_VTRACKS", envir = .GlobalEnv)
        emr_vtracks[[vtrack]] <- NULL

        assign("EMR_VTRACKS", emr_vtracks, envir = .GlobalEnv)
    }

    return(NULL)
}


#' Clear all virtual tracks from the current environment
#'
#' @return None.
#'
#' @examples
#'
#' emr_db.init_examples()
#' emr_vtrack.create("vtrack1", "dense_track")
#' emr_vtrack.ls()
#' emr_vtrack.clear()
#' emr_vtrack.ls()
#' @export
emr_vtrack.clear <- function() {
    EMR_VTRACKS <<- list()
    return(NULL)
}
