.emr_filter <- function(filter) {
    eval(parse(text = sprintf("substitute(%s)", filter)))
}


.emr_filter.get <- function(filterstr) {
    if (!emr_filter.exists(filterstr)) {
        stop(sprintf("Filter %s does not exist", filterstr), call. = F)
    }

    root <- get("EMR_GROOT", envir = .GlobalEnv)
    filter <- get("EMR_FILTERS", envir = .GlobalEnv)[[root]][[filterstr]]
    if (is.null(filter)) {
        root <- get("EMR_UROOT", envir = .GlobalEnv)
        filter <- get("EMR_FILTERS", envir = .GlobalEnv)[[root]][[filterstr]]
    }
    if (!is.null(filter$logical)) {
        filter$src <- filter$logical$src
        filter$val <- filter$logical$val
    }
    filter$logical <- NULL

    filter
}

.emr_filter_calc_val_logical <- function(ltrack_name, val) {
    ltrack_info <- emr_track.logical.info(ltrack_name)

    if (is.null(val)) {
        return(ltrack_info$values)
    }

    return(intersect(val, ltrack_info$values))
}




#' Creates a new named filter
#'
#' Creates a new named filter.
#'
#' This function creates a new named filter.
#'
#' 'src' can be either a track name or an id-time table - data frame with the
#' first columns named "id", "time" and an optional "ref".
#'
#' If 'val' is not 'NULL', the time window of the filter is required to contain
#' at least one value from the vector of 'val'.
#'
#' 'val' is allowed to be used only when 'src' is a name of a categorical
#' track.
#'
#' If 'expiration' is not 'NULL' and the filter window contains a value at time
#' 't', the existence of previous values in the time window of [t-expiration,
#' t-1] (aka: "expiration window") is checked. If no such values are found in
#' the expiration window, the filter returns 'TRUE', otherwise 'FALSE'.
#'
#' 'expiration' is allowed to be used only when 'src' is a name of a
#' categorical track and 'keepref' is 'FALSE'.
#'
#' If both 'val' and 'expiration' are not 'NULL' then only values from 'val'
#' vector are checked both in time window and expiration window.
#'
#' Note: 'time.shift' can be used only when 'keepref' is 'FALSE'.
#'
#' @param filter filter name
#' @param src source (track name or id-time table)
#' @param keepref 'TRUE' or 'FALSE'
#' @param time.shift time shift and expansion for iterator time
#' @param val selected values
#' @param expiration expiration period
#' @return None.
#' @seealso \code{\link{emr_filter.attr.src}}, \code{\link{emr_filter.ls}},
#' \code{\link{emr_filter.exists}}, \code{\link{emr_filter.rm}}
#' @keywords ~filter
#' @examples
#'
#' emr_db.init_examples()
#' emr_filter.create("f1", "dense_track", time.shift = c(2, 4))
#' emr_filter.create("f2", "dense_track", keepref = T)
#' emr_extract("sparse_track", filter = "!f1 & f2")
#' @export emr_filter.create
emr_filter.create <- function(filter, src, keepref = F, time.shift = NULL, val = NULL, expiration = NULL) {
    if (missing(filter) || missing(src)) {
        stop("Usage: emr_filter.create(filter, src, keepref = F, time.shift = NULL, val = NULL, expiration = NULL)", call. = F)
    }
    .emr_checkroot()

    if (filter != make.names(filter)) {
        stop(sprintf("\"%s\" is not a syntactically valid name for a variable", filter), call. = F)
    }

    if (!exists("EMR_FILTERS", envir = .GlobalEnv)) {
        EMR_FILTERS <<- list()
    }

    if (emr_track.exists(filter)) {
        stop(sprintf("Track %s already exists", filter), call. = F)
    }

    if (emr_vtrack.exists(filter)) {
        stop(sprintf("Virtual track %s already exists", filter), call. = F)
    }

    if (is.character(src) && length(src) == 1 && !is.na(match(src, .emr_call("emr_track_db_names", EMR_UROOT, new.env(parent = parent.frame()), silent = TRUE)))) {
        root <- get("EMR_UROOT", envir = .GlobalEnv)
    } else {
        root <- get("EMR_GROOT", envir = .GlobalEnv)
    }

    logical <- NULL

    if (is.character(src) && emr_track.logical.exists(src)) {
        logical$src <- src
        logical$val <- val

        ltrack_info <- emr_track.logical.info(src)
        # the values for a filter on a logical track
        # are the intersection between the logical
        # track's values, and the values requested
        # to be filtered. which are then applied on
        # the source track.
        val <- .emr_filter_calc_val_logical(src, val)

        src <- ltrack_info$source

        # when the user requests a filter with values
        # outside the scope of the logical track, we
        # need to simulate a filter which excludes
        # all the data points in the track. This does
        # not  apply to  logical tracks  for  numeric
        # physical tracks which only serve as an alias
        if (length(val) == 0 && emr_track.info(logical$src)$categorical) {
            src <- data.frame(id = numeric(), time = numeric())
            val <- NULL
        }
    }

    var <- list(src = src, time_shift = time.shift, keepref = keepref, val = val, expiration = expiration, logical = logical)
    .emr_call("emr_check_named_filter", var, filter, new.env(parent = parent.frame()))
    emr_filter.rm(filter)
    EMR_FILTERS[[root]][[filter]] <<- var

    retv <- NULL
}



#' Get or set attributes of a named filter
#'
#' Get or set attributes of a named filter.
#'
#' When only 'filter' argument is used in the call, the functions return the
#' corresponding attribute of the named filter. Otherwise a new attribute value
#' is set.
#'
#' Note: since inter-dependency exists between certain attributes, the
#' correctness of the attributes as a whole can only be verified when the named
#' filter is applied to a track expression.
#'
#' For more information about the valid attribute values please refer to the
#' documentation of 'emr_filter.create'.
#'
#' @aliases emr_filter.attr.src emr_filter.attr.keepref
#' emr_filter.attr.time.shift emr_filter.attr.val emr_filter.attr.expiration
#' @param filter filter name.
#' @param src,keepref,time.shift,val,expiration filter attributes.
#' @return None.
#' @seealso \code{\link{emr_filter.create}}
#' @keywords ~filter
#' @examples
#'
#' emr_db.init_examples()
#' emr_filter.create("f1", "dense_track", time.shift = c(2, 4))
#' emr_filter.attr.src("f1")
#' emr_filter.attr.src("f1", "sparse_track")
#' emr_filter.attr.src("f1")
#' @export emr_filter.attr.src
emr_filter.attr.src <- function(filter, src) {
    if (missing(filter)) {
        stop("Usage: emr_filter.attr.src(filter, src)", call. = F)
    }
    .emr_checkroot()

    root <- get("EMR_GROOT", envir = .GlobalEnv)
    filter.var <- get("EMR_FILTERS", envir = .GlobalEnv)[[root]][[filter]]

    if (is.null(filter.var)) {
        root <- get("EMR_UROOT", envir = .GlobalEnv)
        filter.var <- get("EMR_FILTERS", envir = .GlobalEnv)[[root]][[filter]]
    }

    if (is.null(filter.var)) {
        stop(sprintf("Filter \"%s\" does not exist", filter), call. = F)
    }

    is_logical_filter <- !is.null(filter.var$logical)

    if (missing(src)) {
        if (is_logical_filter) {
            return(filter.var$logical$src)
        } else {
            return(filter.var$src)
        }
    } else if (is.character(src) && emr_track.logical.exists(src)) {
        emr_filter.rm(filter)
        filter.var$logical$src <- src

        if (!is_logical_filter) {
            filter.var$logical$val <- filter.var$val
        }

        filter.var$val <- .emr_filter_calc_val_logical(src, filter.var$logical$val)
        ltrack_info <- emr_track.logical.info(src)
        filter.var$src <- ltrack_info$source

        if (length(filter.var$val) == 0) {
            filter.var$src <- data.frame(id = numeric(), time = numeric())
            filter.var$val <- NULL
        }
    } else {
        .emr_call("emr_check_filter_attr_src", src, new.env(parent = parent.frame()))
        emr_filter.rm(filter)
        filter.var$src <- src

        if (is_logical_filter) {
            filter.var$val <- filter.var$logical$val
            filter.var$logical <- NULL
        }
    }

    if (is.character(src) && length(src) == 1 && !is.na(match(src, .emr_call("emr_track_db_names", EMR_UROOT, new.env(parent = parent.frame()), silent = TRUE)))) {
        root <- get("EMR_UROOT", envir = .GlobalEnv)
    } else {
        root <- get("EMR_GROOT", envir = .GlobalEnv)
    }
    EMR_FILTERS[[root]][[filter]] <<- filter.var
    retv <- NULL
}

emr_filter.attr.keepref <- function(filter, keepref) {
    if (missing(filter)) {
        stop("Usage: emr_filter.attr.keepref(filter, keepref)", call. = F)
    }
    .emr_checkroot()

    root <- get("EMR_GROOT", envir = .GlobalEnv)
    filter.var <- get("EMR_FILTERS", envir = .GlobalEnv)[[root]][[filter]]
    if (is.null(filter.var)) {
        root <- get("EMR_UROOT", envir = .GlobalEnv)
        filter.var <- get("EMR_FILTERS", envir = .GlobalEnv)[[root]][[filter]]
    }

    if (is.null(filter.var)) {
        stop(sprintf("Filter \"%s\" does not exist", filter), call. = F)
    }

    if (missing(keepref)) {
        filter.var$keepref
    } else {
        if (!is.logical(keepref) || is.na(keepref)) {
            stop("'keepref' parameter must be logical", call. = F)
        }

        EMR_FILTERS[[root]][[filter]]["keepref"] <<- list(keepref)
        retv <- NULL
    }
}

emr_filter.attr.time.shift <- function(filter, time.shift) {
    if (missing(filter)) {
        stop("Usage: emr_filter.attr.time.shift(filter, time.shift)", call. = F)
    }
    .emr_checkroot()

    root <- get("EMR_GROOT", envir = .GlobalEnv)
    filter.var <- get("EMR_FILTERS", envir = .GlobalEnv)[[root]][[filter]]
    if (is.null(filter.var)) {
        root <- get("EMR_UROOT", envir = .GlobalEnv)
        filter.var <- get("EMR_FILTERS", envir = .GlobalEnv)[[root]][[filter]]
    }

    if (is.null(filter.var)) {
        stop(sprintf("Filter \"%s\" does not exist", filter), call. = F)
    }

    if (missing(time.shift)) {
        filter.var$time_shift
    } else {
        .emr_call("emr_check_filter_attr_time_shift", time.shift, new.env(parent = parent.frame()))
        EMR_FILTERS[[root]][[filter]]["time_shift"] <<- list(time.shift)
        retv <- NULL
    }
}

emr_filter.attr.val <- function(filter, val) {
    if (missing(filter)) {
        stop("Usage: emr_filter.attr.val(filter, val)", call. = F)
    }
    .emr_checkroot()

    root <- get("EMR_GROOT", envir = .GlobalEnv)
    filter.var <- get("EMR_FILTERS", envir = .GlobalEnv)[[root]][[filter]]
    if (is.null(filter.var)) {
        root <- get("EMR_UROOT", envir = .GlobalEnv)
        filter.var <- get("EMR_FILTERS", envir = .GlobalEnv)[[root]][[filter]]
    }

    if (is.null(filter.var)) {
        stop(sprintf("Filter \"%s\" does not exist", filter), call. = F)
    }

    is_logical_filter <- !is.null(filter.var$logical)

    if (missing(val)) {
        if (is_logical_filter) {
            return(filter.var$logical$val)
        } else {
            return(filter.var$val)
        }
    }

    if (!is.numeric(val)) {
        stop("'val' parameter must be a numeric vector", call. = F)
    }

    if (is_logical_filter) {
        filter.var$logical$val <- val
        val <- .emr_filter_calc_val_logical(filter.var$logical$src, val)

        if (length(val) == 0) {
            filter.var$src <- data.frame(id = numeric(), time = numeric())
            val <- NULL
        }
        EMR_FILTERS[[root]][[filter]] <<- filter.var
    } else {
        EMR_FILTERS[[root]][[filter]]["val"] <<- unique(list(val))
    }
    retv <- NULL
}

emr_filter.attr.expiration <- function(filter, expiration) {
    if (missing(filter)) {
        stop("Usage: emr_filter.attr.expiration(filter, expiration)", call. = F)
    }
    .emr_checkroot()

    root <- get("EMR_GROOT", envir = .GlobalEnv)
    filter.var <- get("EMR_FILTERS", envir = .GlobalEnv)[[root]][[filter]]
    if (is.null(filter.var)) {
        root <- get("EMR_UROOT", envir = .GlobalEnv)
        filter.var <- get("EMR_FILTERS", envir = .GlobalEnv)[[root]][[filter]]
    }

    if (is.null(filter.var)) {
        stop(sprintf("Filter \"%s\" does not exist", filter), call. = F)
    }

    if (missing(expiration)) {
        filter.var$expiration
    } else {
        .emr_call("emr_check_filter_attr_expiration", expiration, new.env(parent = parent.frame()))
        EMR_FILTERS[[root]][[filter]]["expiration"] <<- list(expiration)
        retv <- NULL
    }
}



#' Checks whether the named filter exists
#'
#' Checks whether the named filter exists.
#'
#' This function checks whether the named filter exists.
#'
#' @param filter filter name
#' @return 'TRUE', if the named filter exists, otherwise 'FALSE'.
#' @seealso \code{\link{emr_filter.create}}, \code{\link{emr_filter.ls}}
#' @keywords ~filter ~exists
#' @examples
#'
#' emr_db.init_examples()
#' emr_filter.create("f1", "dense_track", time.shift = c(2, 4))
#' emr_filter.exists("f1")
#' @export emr_filter.exists
emr_filter.exists <- function(filter) {
    if (missing(filter)) {
        stop("Usage: emr_filter.exists(filter)", call. = F)
    }
    .emr_checkroot()

    res <- FALSE
    if (exists("EMR_FILTERS", envir = .GlobalEnv)) {
        filters <- get("EMR_FILTERS", envir = .GlobalEnv)
        res <- !is.null(filters[[get("EMR_GROOT", envir = .GlobalEnv)]][[filter]]) ||
            exists("EMR_UROOT", envir = .GlobalEnv) && !is.null(get("EMR_UROOT", envir = .GlobalEnv)) && !is.null(filters[[get("EMR_UROOT", envir = .GlobalEnv)]][[filter]])
    }
    res
}



#' Returns the definition of a named filter
#'
#' Returns the definition of a named filter.
#'
#' This function returns the internal represenation of a named filter.
#'
#' @param filter filter name
#' @return Internal representation of a named filter.
#' @seealso \code{\link{emr_filter.create}}
#' @keywords ~filter
#' @examples
#'
#' emr_db.init_examples()
#' emr_filter.create("f1", "dense_track", time.shift = c(2, 4))
#' emr_filter.info("f1")
#' @export emr_filter.info
emr_filter.info <- function(filter) {
    if (missing(filter)) {
        stop("Usage: emr_filter.info(filter)", call. = F)
    }
    .emr_checkroot()

    .emr_filter.get(filter)
}



#' Returns a list of named filters
#'
#' Returns a list of named filters.
#'
#' This function returns a list of named filters that exist in current R
#' environment that match the pattern (see 'grep'). If called without any
#' arguments all named filters are returned.
#'
#' @param pattern,ignore.case,perl,fixed,useBytes see 'grep'
#' @return An array that contains the names of filters.
#' @seealso \code{\link{grep}}, \code{\link{emr_filter.exists}},
#' \code{\link{emr_filter.create}}, \code{\link{emr_filter.rm}}
#' @keywords ~filter ~ls
#' @examples
#'
#' emr_db.init_examples()
#' emr_filter.create("f1", "dense_track", time.shift = c(2, 4))
#' emr_filter.create("f2", "dense_track", keepref = T)
#' emr_filter.ls()
#' emr_filter.ls("*2")
#' @export emr_filter.ls
emr_filter.ls <- function(pattern = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
    if (!exists("EMR_FILTERS", envir = .GlobalEnv)) {
        return(NULL)
    }
    .emr_checkroot()

    emr_filters <- get("EMR_FILTERS", envir = .GlobalEnv)
    emr_roots <- names(emr_filters)
    if (!is.list(emr_filters) || (length(emr_filters) && !is.character(emr_roots)) || length(emr_filters) != length(emr_roots)) {
        stop("Invalid format of EMR_FILTERS variable.\nTo continue working with filters please remove this variable from the environment.", call. = F)
    }

    all.filternames <- NULL
    roots <- c("EMR_GROOT", "EMR_UROOT")

    for (root in roots) {
        if (exists(root, envir = .GlobalEnv) && !is.null(get(root, envir = .GlobalEnv))) {
            emr_root <- get(root, envir = .GlobalEnv)
            idx <- match(emr_root, emr_roots)
            if (!is.na(idx)) {
                filters <- emr_filters[[idx]]
                filternames <- names(filters)
                if (!is.list(filters) || (length(filters) && !is.character(filternames)) || length(filters) != length(filternames)) {
                    stop("Invalid format of EMR_FILTERS variable.\nTo continue working with filters please remove this variable from the environment.", call. = F)
                }
                all.filternames <- c(all.filternames, filternames)
            }
        }
    }

    if (is.null(all.filternames)) {
        return(NULL)
    }

    if (pattern != "") {
        sort(grep(pattern, all.filternames, value = TRUE, ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes))
    } else {
        sort(all.filternames)
    }
}



#' Deletes a named filter
#'
#' Deletes a named filter.
#'
#' This function deletes a named filter from current R environment.
#'
#' @param filter filter name
#' @return None.
#' @seealso \code{\link{emr_filter.create}}, \code{\link{emr_filter.ls}}
#' @keywords ~filter
#' @examples
#'
#' emr_db.init_examples()
#' emr_filter.create("f1", "dense_track", time.shift = c(2, 4))
#' emr_filter.create("f2", "dense_track", keepref = T)
#' emr_filter.ls()
#' emr_filter.rm("f1")
#' emr_filter.ls()
#' @export emr_filter.rm
emr_filter.rm <- function(filter) {
    if (missing(filter)) {
        stop("Usage: emr_filter.rm(filter)", call. = F)
    }
    .emr_checkroot()

    if (exists("EMR_FILTERS", envir = .GlobalEnv)) {
        emr_filters <- get("EMR_FILTERS", envir = .GlobalEnv)
        emr_filters[[get("EMR_GROOT", envir = .GlobalEnv)]][[filter]] <- NULL

        if (exists("EMR_UROOT", envir = .GlobalEnv) && !is.null(get("EMR_UROOT", envir = .GlobalEnv))) {
            emr_filters[[get("EMR_UROOT", envir = .GlobalEnv)]][[filter]] <- NULL
        }

        assign("EMR_FILTERS", emr_filters, envir = .GlobalEnv)
    }

    retv <- NULL
}
