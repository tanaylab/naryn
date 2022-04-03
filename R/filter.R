#' Parse an R filter string
#'
#' @noRd
#' @export
.emr_filter <- function(filter) {
    eval(parse(text = sprintf("substitute(%s)", filter)))
}


.emr_filter.get <- function(filterstr) {
    if (!emr_filter.exists(filterstr)) {
        stop(sprintf("Filter %s does not exist", filterstr), call. = FALSE)
    }

    filter <- get("EMR_FILTERS", envir = .GlobalEnv)[[filterstr]]

    if (!is.null(filter$logical)) {
        filter$src <- filter$logical$src
        filter$val <- filter$logical$val
    }

    filter$logical <- NULL
    filter$vtrack <- NULL

    filter
}

.emr_filter_calc_val_logical <- function(ltrack_name, val) {
    ltrack_info <- emr_track.logical.info(ltrack_name)

    if (is.null(val)) {
        if (is.null(ltrack_info$values)) {
            return(NULL)
        }
        return(ltrack_info$values)
    }

    return(intersect(val, ltrack_info$values))
}

.str_to_operator <- function(str) {
    convertor <- c("eq" = "=", "lt" = "<", "lte" = "<=", "gt" = ">", "gte" = ">=")
    return(convertor[str])
}

.operator_to_str <- function(op) {
    convertor <- c("=" = "eq", "<" = "lt", "<=" = "lte", ">" = "gt", ">=" = "gte")
    return(convertor[op])
}


.emr_parse_exprs <- function(expr) {
    res <- c()
    if (!is.null(expr)) {
        res <- all.vars(as.list(parse(text = expr))[[1]])
    }
    return(res)
}

.extract_vtrack_filters <- function(vtracks, iterator, keepref, stime, etime) {
    # if the iterator is the same as the virtual track iterator and the
    # virtual track has a filter, we can optimize the extract call by applying
    # that filter.
    # In the future we would want to add another optimization and apply the original iterator
    # filters, excluding those which are virtual tracks.
    vtrack_filters <- vtracks %>%
        purrr::keep(~
            !is.null(emr_vtrack.info(.x)$filter) &&
                is.character(iterator) &&
                is.character(emr_vtrack.info(.x)$src) &&
                iterator == emr_vtrack.info(.x)$src) %>%
        purrr::map_chr(~ deparse(emr_vtrack.info(.x)$filter))

    if (length(vtrack_filters) > 0) {
        extract_filter <- paste(glue::glue("({vtrack_filters})"), collapse = " | ")
    } else {
        extract_filter <- NULL
    }

    vtrack_filters_result <- emr_extract(vtracks, iterator = iterator, keepref = keepref, stime = stime, etime = etime, filter = extract_filter)

    return(vtrack_filters_result)
}

.emr_detect_vtrack_filters <- function(filter) {
    parsed_filters <- .emr_parse_exprs(filter)

    vtrack_filters <- purrr::keep(parsed_filters, ~ {
        emr_filter.exists(.x) &&
            is.character(emr_filter.info(.x)$src) &&
            emr_vtrack.exists(emr_filter.info(.x)$src)
    })

    # look for vtrack names that were given as a filter
    explicit_vtracks <- purrr::keep(parsed_filters, emr_vtrack.exists)

    return(list(vtrack_filters = vtrack_filters, explicit_vtracks = explicit_vtracks))
}

#' The function overrides the filters which are applied on vtracks,
#' It uses the queries iterator to extract the vtrack expression and
#' creates a new operator filter based on the extract result.
#' The function returns the original information of filters passed
#' as a named list with the 'new' and 'updated' filters that were created / changed
#' during the operation. This list can be later sent to .emr_recreate_vtrack_filters,
#' which, as the name suggests, removes the 'new' filters and restores the 'updated'
#' filters to their original state.
#'
#' @noRd
.emr_gen_vtrack_filters <- function(filter, iterator, keepref, stime, etime) {
    query_vt_f <- .emr_detect_vtrack_filters(filter)
    vtrack_filters <- query_vt_f$vtrack_filters
    explicit_vtracks <- query_vt_f$explicit_vtracks

    orig_vt_filters <- vtrack_filters %>%
        purrr::map(~ {
            info <- emr_filter.info(.x)
            info$filter <- .x
            return(info)
        })

    vtrack_filters <- c(vtrack_filters, explicit_vtracks)

    # filters we want to remove after the operation is finished
    rm_filters <- purrr::discard(explicit_vtracks, emr_filter.exists)

    orig_filters <- list(new = rm_filters, updated = orig_vt_filters)

    tryCatch(
        {
            # create a filter with the same name as the virtual track
            purrr::walk(explicit_vtracks, ~ {
                vtrack <- emr_vtrack.info(.x)
                .create_named_filter(
                    filter = .x,
                    src = .x,
                    time.shift = vtrack$time_shift
                )
            })

            # We only need to extract filters which either:
            # 1. Have a value parameter, i.e. filter the result of the virtual track extraction.
            # 2. Their source virtual track has a filter.
            # 3. Have a different time shift than the virtual track.
            # In all other cases we can just create a new filter based on the source virtual track,
            # with the same time.shift and expiration
            vtrack_filters_to_extract <- purrr::keep(vtrack_filters, ~ {
                filter_i <- emr_filter.info(.x)
                vtrack_i <- emr_vtrack.info(filter_i$src)
                return(
                    !is.null(filter_i$val) ||
                        !is.null(vtrack_i$filter) ||
                        (
                            !is.null(filter_i$time_shift) &&
                                !is.null(vtrack_i$time_shift) &&
                                !(all(filter_i$time_shift == vtrack_i$time_shift))
                        )
                )
            })

            other_vtrack_filters <- setdiff(vtrack_filters, vtrack_filters_to_extract)

            vtracks <- purrr::map_chr(vtrack_filters_to_extract, ~ {
                emr_filter.info(.x)$src
            })

            if (length(vtracks) > 0 && is.null(iterator)) {
                stop("Please specify an iterator. NULL iterator is not allowed when there are filters on vtracks.")
            }

            if (length(vtracks) > 0) {
                vtrack_filters_result <- .extract_vtrack_filters(vtracks, iterator, keepref, stime, etime)
            }

            # create new filters based on the extraction result
            purrr::walk2(vtrack_filters_to_extract, vtracks, ~ {
                orig_filter <- emr_filter.info(.x)

                .create_named_filter(
                    filter = .x,
                    src = vtrack_filters_result %>% dplyr::select(id, time, ref, value = !!.y) %>% na.omit(),
                    time.shift = orig_filter$time_shift,
                    val = orig_filter$val,
                    expiration = orig_filter$expiration,
                    operator = orig_filter$operator,
                    use_values = !is.null(orig_filter$val) # we use the values only if the original filter had values
                )
            })

            # For the rest of the filters - translate the time shift and
            # expiration virtual track paramters to filter paramters
            purrr::walk(other_vtrack_filters, ~ {
                orig_filter <- emr_filter.info(.x)
                vtrack <- emr_vtrack.info(orig_filter$src)
                .create_named_filter(.x,
                    src = vtrack$src,
                    time.shift = orig_filter$time_shift,
                    expiration = orig_filter$expiration
                )
            })
        },
        error = {
            .emr_recreate_vtrack_filters(orig_filters)
        }
    )

    return(orig_filters)
}

#' The function receives the output of .emr_gen_vtrack_filters
#' and reverts the filters to there old, original form.
#'
#' @noRd
.emr_recreate_vtrack_filters <- function(orig_filters) {
    purrr::walk(orig_filters$updated, ~ {
        emr_filter.create(
            filter = .x$filter,
            src = .x$src,
            time.shift = .x$time_shift,
            val = .x$val,
            expiration = .x$expiration,
            operator = .x$operator,
            use_values = .x$use_values
        )
    })

    purrr::walk(orig_filters$new, emr_filter.rm)
}



#' Generate a default name for a naryn filter
#'
#' Generate a default name for a naryn filter
#'
#' Given filter parameters, generate a name with the following format:
#' "f_{src}.kr{keepref}.vals_{val}.ts_{time.shift}.exp_{expiration}.op_{operator}"
#' Where for 'val' and 'time.shift' the values are separated by an
#' underscore.
#'
#' If \code{time.shift}, \code{val} or \code{expiration} are
#' \code{NULL} - their section would not appear in the generated name.
#'
#'
#' @inheritParams emr_filter.create
#'
#' @return a default name for the filter
#'
#' @seealso \code{\link{emr_filter.create}}
#' @keywords ~filter
#'
#' @examples
#'
#' emr_db.init_examples()
#' emr_filter.name("dense_track", time.shift = c(2, 4))
#' @export
emr_filter.name <- function(src, keepref = FALSE, time.shift = NULL, val = NULL, expiration = NULL, operator = "=") {
    if (missing(src)) {
        stop("Usage: emr_filter.name(src, keepref = FALSE, time.shift = NULL, val = NULL, expiration = NULL)", call. = FALSE)
    }

    if (!is.character(src)) {
        stop("Cannot generate automatic filter name when source is a data.frame", call. = FALSE)
    }

    if (keepref) {
        keepref_str <- "krT."
    } else {
        keepref_str <- "krF."
    }

    if (!is.null(val)) {
        val <- formatC(val, format = "fg") # do not use scientific notation
        val_str <- glue::glue("vals_{vals}.", vals = paste(sort(unique(val)), collapse = "_"))
    } else {
        val_str <- ""
    }

    if (!is.null(time.shift)) {
        time.shift <- formatC(time.shift, format = "fg") # do not use scientific notation
        if (length(time.shift) == 1) {
            time.shift_str <- glue::glue("ts_{time.shift}.")
        } else if (length(time.shift) == 2) {
            time.shift_str <- glue::glue("ts_{time.shift[1]}_{time.shift[2]}.")
        } else {
            stop("cannot parse time.shift argument", .call = FALSE)
        }
    } else {
        time.shift_str <- ""
    }

    if (!is.null(expiration)) {
        expiration <- formatC(expiration, format = "fg") # do not use scientific notation
        expiration_str <- glue::glue("exp_{expiration}.")
    } else {
        expiration_str <- ""
    }

    if (operator != "=") {
        operator <- .operator_to_str(operator)
        operator_str <- glue::glue("op_{operator}")
    } else {
        operator_str <- ""
    }

    filter_name <- glue::glue("f_{src}.{keepref_str}{val_str}{time.shift_str}{expiration_str}{operator_str}")

    filter_name <- gsub("-", "minus", filter_name)
    filter_name <- gsub("\\.$", "", filter_name)

    return(as.character(filter_name))
}


#' Create a filter from an automatically generated name
#'
#' @param filter name of a filter automatically generated by \code{emr_filter.name}. Can be a vector of filter names.
#'
#' @seealso \code{\link{emr_filter.create}}, \code{\link{emr_filter.create_from_name}}
#' @keywords ~filter
#'
#' @examples
#' emr_db.init_examples()
#' name <- emr_filter.name("dense_track", time.shift = c(2, 4))
#' emr_filter.create_from_name(name)
#' @export
emr_filter.create_from_name <- function(filter) {
    if (missing(filter)) {
        stop("Usage: emr_filter.create_from_name(filter)", call. = FALSE)
    }

    if (length(filter) > 1) {
        return(purrr::map_chr(filter, emr_filter.create_from_name))
    }

    # src
    parsed_str <- stringr::str_match(filter, "f_(.+)\\.kr")
    src <- parsed_str[, 2]

    # keepref
    parsed_str <- stringr::str_match(filter, glue::glue("{parsed_str[, 1]}([TF])"))
    keepref <- as.logical(parsed_str[, 2])
    if (is.na(keepref)) {
        stop("Couldn't find keepref. Did you create the name using emr_track.name?", call. = FALSE)
    }

    # vals
    vals_str <- stringr::str_match(filter, glue::glue("{parsed_str[, 1]}\\.vals_(.+)(?=\\.ts)"))

    if (is.na(vals_str[, 2])) {
        vals_str <- stringr::str_match(filter, glue::glue("{parsed_str[, 1]}\\.vals_(.+)(?=\\.exp)"))
    }
    if (is.na(vals_str[, 2])) {
        vals_str <- stringr::str_match(filter, glue::glue("{parsed_str[, 1]}\\.vals_(.+)(?=\\.op)"))
    }
    if (is.na(vals_str[, 2])) {
        vals_str <- stringr::str_match(filter, glue::glue("{parsed_str[, 1]}\\.vals_(.+)$"))
    }

    if (is.na(vals_str[, 2])) {
        val <- NULL
    } else {
        parsed_str <- vals_str
        val <- stringr::str_split(vals_str[, 2], "_")[[1]]
        val <- gsub("minus", "-", val)
        val <- as.numeric(val)
        if (any(is.na(val))) {
            stop("Couldn't parse values. Did you create the name using emr_track.name?", call. = FALSE)
        }
    }

    # time shift
    ts_str <- stringr::str_match(filter, glue::glue("{parsed_str[, 1]}\\.ts_(.+)(?=\\.exp)"))
    if (is.na(ts_str[, 2])) {
        ts_str <- stringr::str_match(filter, glue::glue("{parsed_str[, 1]}\\.ts_(.+)(?=\\.op)"))
    }
    if (is.na(ts_str[, 2])) {
        ts_str <- stringr::str_match(filter, glue::glue("{parsed_str[, 1]}\\.ts_(.+)$"))
    }
    if (is.na(ts_str[, 2])) {
        time.shift <- NULL
    } else {
        parsed_str <- ts_str
        time.shift <- stringr::str_split(ts_str[, 2], "_")[[1]]
        time.shift <- gsub("minus", "-", time.shift)
        time.shift <- as.numeric(time.shift)
        if (any(is.na(time.shift))) {
            stop("Couldn't parse time.shift. Did you create the name using emr_track.name?", call. = FALSE)
        }
    }

    # expiration
    exp_str <- stringr::str_match(filter, glue::glue("{parsed_str[, 1]}\\.exp_([^.]+)(?=\\.op)"))
    if (is.na(exp_str[, 2])) {
        exp_str <- stringr::str_match(filter, glue::glue("{parsed_str[, 1]}\\.exp_(.+)$"))
    }
    if (is.na(exp_str[, 2])) {
        expiration <- NULL
    } else {
        parsed_str <- exp_str
        expiration <- stringr::str_split(exp_str[, 2], "_")[[1]]
        expiration <- gsub("minus", "-", expiration)
        expiration <- as.numeric(expiration)
        if (any(is.na(expiration))) {
            stop("Couldn't parse expiration. Did you create the name using emr_track.name?", call. = FALSE)
        }
    }

    # operator
    op_str <- stringr::str_match(filter, glue::glue("{parsed_str[, 1]}\\.op_([^.]+)$"))
    if (is.na(op_str[, 2])) {
        operator <- "="
    } else {
        parsed_str <- op_str
        operator <- stringr::str_split(op_str[, 2], "_")[[1]]
        operator <- .str_to_operator(operator)

        if (any(is.na(operator))) {
            stop("Couldn't parse operator. Did you create the name using emr_track.name?", call. = FALSE)
        }
    }

    emr_filter.create(filter, src = src, keepref = keepref, time.shift = time.shift, val = val, expiration = expiration, operator = operator)
}


#' Creates a new named filter
#'
#' Creates a new named filter.
#'
#' This function creates a new named filter.
#'
#' 'src' can be either a track name, a virtual track name, or an id-time table - data frame with the
#' first columns named "id", "time" and an optional "ref".
#'
#' If 'val' is not 'NULL', the time window of the filter is required to contain
#' at least one value from the vector of 'val' which passes the 'operator' (see below).
#'
#' 'val' is allowed to be used only when 'src' is a name of a track. When val is specified,
#' the filter will filter the i.d, time points by applying the 'operator' argument on the
#' value of the point.
#'
#' If 'expiration' is not 'NULL' and the filter window contains a value at time
#' 't', the existence of previous values in the time window of [t-expiration,
#' t-1] (aka: "expiration window") is checked. If no such values are found in
#' the expiration window, the filter returns 'TRUE', otherwise 'FALSE'.
#'
#' 'expiration' is allowed to be used only when 'src' is a name of a
#' categorical track and 'keepref' is 'FALSE'.
#'
#' 'operator' corresponds to the 'val' argument. The point passes the filter
#' if the point's value passes the operator. For example if the point's value is 4,
#' the operator is "<" and val is 5, the expression evaluated is 4 < 5 (pass).
#' When 'operator' is not "=", 'vals' must exist, and be of length 1.
#'
#' If both 'val' and 'expiration' are not 'NULL' then only values from 'val'
#' vector are checked both in time window and expiration window.
#'
#' Note: 'time.shift' can be used only when 'keepref' is 'FALSE'.
#' Note:  A zero length vector is interpeted by R as NULL, so \code{val=c()} would create
#' a filter which returns all the values of \code{src}
#'
#' @param filter filter name. If NULL - a name would be generated automatically using \code{emr_filter.name}.
#' @param src source (track name, virtual track name or id-time table). Can be a vector of track names.
#' @param keepref 'TRUE' or 'FALSE'
#' @param time.shift time shift and expansion for iterator time
#' @param val selected values
#' @param expiration expiration period
#' @param operator operator for filtering. Accepts one of: "=", "<", "<=", ">", ">="
#' @return Name of the filter (invisibly, if filter name wasn't generated automatically)
#' @seealso \code{\link{emr_filter.attr.src}}, \code{\link{emr_filter.ls}},
#' \code{\link{emr_filter.exists}}, \code{\link{emr_filter.rm}}, \code{\link{emr_filter.create_from_name}}
#' @keywords ~filter
#' @examples
#'
#' emr_db.init_examples()
#' emr_filter.create("f1", "dense_track", time.shift = c(2, 4))
#' emr_filter.create("f2", "dense_track", keepref = TRUE)
#' emr_extract("sparse_track", filter = "!f1 & f2")
#' @export emr_filter.create
emr_filter.create <- function(filter, src, keepref = FALSE, time.shift = NULL, val = NULL, expiration = NULL, operator = "=", use_values = FALSE) {
    if (missing(filter) || missing(src)) {
        stop("Usage: emr_filter.create(filter, src, keepref = FALSE, time.shift = NULL, val = NULL, expiration = NULL)", call. = FALSE)
    }
    .emr_checkroot()
    if (is.character(src) && length(src) > 1 || (!is.null(filter) && length(filter) > 1)) {
        if (is.null(filter)) {
            return(purrr::map_chr(src, ~ emr_filter.create(filter = NULL, src = .x, keepref = keepref, time.shift = time.shift, val = val, expiration = expiration)))
        } else {
            if (length(filter) != length(src)) {
                stop("'filter' and 'src' should have the same length.", call. = FALSE)
            }
            return(purrr::map2_chr(filter, src, ~ emr_filter.create(filter = .x, src = .y, keepref = keepref, time.shift = time.shift, val = val, expiration = expiration)))
        }
    }

    auto_filter <- FALSE
    if (is.null(filter)) {
        filter <- emr_filter.name(src, keepref, time.shift, val, expiration)
        auto_filter <- TRUE
    }

    if (filter != make.names(filter)) {
        stop(sprintf("\"%s\" is not a syntactically valid name for a variable", filter), call. = FALSE)
    }

    if (startsWith(filter, "..emr")) {
        stop("Filters with an '..emr' prefix are reserved for internal 'naryn' usage. Please rename your filter.", call. = FALSE)
    }

    if (emr_track.exists(filter)) {
        stop(sprintf("Track %s already exists (you cannot create a filter named as an existing track)", filter), call. = FALSE)
    }

    if (emr_vtrack.exists(filter)) {
        stop(sprintf("Virtual track %s already exists (you cannot create a filter named as an existing virtual track)", filter), call. = FALSE)
    }

    .create_named_filter(filter, src, keepref, time.shift, val, expiration, operator, use_values)

    if (auto_filter) {
        return(filter)
    } else {
        invisible(filter)
    }
}

.create_named_filter <- function(filter, src, keepref = FALSE, time.shift = NULL, val = NULL, expiration = NULL, operator = "=", use_values = FALSE) {
    if (!exists("EMR_FILTERS", envir = .GlobalEnv)) {
        EMR_FILTERS <<- list()
    }

    logical <- NULL

    if (is.character(src) && emr_track.logical.exists(src)) {
        logical$src <- src
        logical$val <- val

        ltrack_info <- emr_track.logical.info(src)
        if (is.null(ltrack_info$values)) {
            src <- ltrack_info$source
        } else {
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
    }

    vtrack <- NULL

    if (is.character(src) && emr_vtrack.exists(src)) {
        if (use_values && is.null(val)) {
            stop("Can not set 'use_values' to TRUE, when 'val' is set to NULL")
        }
        vtrack <- emr_vtrack.info(src)
    }

    var <- list(src = src, time_shift = time.shift, keepref = keepref, val = val, expiration = expiration, logical = logical, operator = operator, vtrack = vtrack, use_values = use_values)

    # filters on vtracks are created lazily on extract
    if (is.null(vtrack)) {
        .emr_call("emr_check_named_filter", var, filter, new.env(parent = parent.frame()))
        emr_filter.rm(filter)
    }

    EMR_FILTERS[[filter]] <<- var
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
        stop("Usage: emr_filter.attr.src(filter, src)", call. = FALSE)
    }

    filter.var <- get("EMR_FILTERS", envir = .GlobalEnv)[[filter]]

    if (is.null(filter.var)) {
        stop(sprintf("Filter \"%s\" does not exist", filter), call. = FALSE)
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

    EMR_FILTERS[[filter]] <<- filter.var
    return(NULL)
}

#' @export
#'
#' @rdname emr_filter.attr.src
emr_filter.attr.keepref <- function(filter, keepref) {
    if (missing(filter)) {
        stop("Usage: emr_filter.attr.keepref(filter, keepref)", call. = FALSE)
    }

    filter.var <- get("EMR_FILTERS", envir = .GlobalEnv)[[filter]]
    if (is.null(filter.var)) {
        stop(sprintf("Filter \"%s\" does not exist", filter), call. = FALSE)
    }

    if (missing(keepref)) {
        filter.var$keepref
    } else {
        if (!is.logical(keepref) || is.na(keepref)) {
            stop("'keepref' parameter must be logical", call. = FALSE)
        }

        EMR_FILTERS[[filter]]["keepref"] <<- list(keepref)
        return(NULL)
    }
}

#' @export
#'
#' @rdname emr_filter.attr.src
emr_filter.attr.time.shift <- function(filter, time.shift) {
    if (missing(filter)) {
        stop("Usage: emr_filter.attr.time.shift(filter, time.shift)", call. = FALSE)
    }

    filter.var <- get("EMR_FILTERS", envir = .GlobalEnv)[[filter]]
    if (is.null(filter.var)) {
        stop(sprintf("Filter \"%s\" does not exist", filter), call. = FALSE)
    }

    if (missing(time.shift)) {
        filter.var$time_shift
    } else {
        .emr_call("emr_check_filter_attr_time_shift", time.shift, new.env(parent = parent.frame()))
        EMR_FILTERS[[filter]]["time_shift"] <<- list(time.shift)
        return(NULL)
    }
}

#' @export
#'
#' @rdname emr_filter.attr.src
emr_filter.attr.val <- function(filter, val) {
    if (missing(filter)) {
        stop("Usage: emr_filter.attr.val(filter, val)", call. = FALSE)
    }

    filter.var <- get("EMR_FILTERS", envir = .GlobalEnv)[[filter]]
    if (is.null(filter.var)) {
        stop(sprintf("Filter \"%s\" does not exist", filter), call. = FALSE)
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
        stop("'val' parameter must be a numeric vector", call. = FALSE)
    }

    if (is_logical_filter) {
        filter.var$logical$val <- val
        val <- .emr_filter_calc_val_logical(filter.var$logical$src, val)

        if (length(val) == 0) {
            filter.var$src <- data.frame(id = numeric(), time = numeric())
            val <- NULL
        }
        EMR_FILTERS[[filter]] <<- filter.var
    } else {
        EMR_FILTERS[[filter]]["val"] <<- unique(list(val))
    }
    return(NULL)
}

#' @export
#'
#' @rdname emr_filter.attr.src
emr_filter.attr.expiration <- function(filter, expiration) {
    if (missing(filter)) {
        stop("Usage: emr_filter.attr.expiration(filter, expiration)", call. = FALSE)
    }

    filter.var <- get("EMR_FILTERS", envir = .GlobalEnv)[[filter]]
    if (is.null(filter.var)) {
        stop(sprintf("Filter \"%s\" does not exist", filter), call. = FALSE)
    }

    if (missing(expiration)) {
        filter.var$expiration
    } else {
        .emr_call("emr_check_filter_attr_expiration", expiration, new.env(parent = parent.frame()))
        EMR_FILTERS[[filter]]["expiration"] <<- list(expiration)
        return(NULL)
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
        stop("Usage: emr_filter.exists(filter)", call. = FALSE)
    }

    res <- FALSE
    if (exists("EMR_FILTERS", envir = .GlobalEnv)) {
        filters <- get("EMR_FILTERS", envir = .GlobalEnv)
        res <- !is.null(filters[[filter]])
    }
    res
}


#' Returns the definition of a named filter
#'
#' Returns the definition of a named filter.
#'
#' This function returns the internal representation of a named filter.
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
        stop("Usage: emr_filter.info(filter)", call. = FALSE)
    }

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
#' If no filter was found, \code{character(0)} would be returned.
#' @seealso \code{\link{grep}}, \code{\link{emr_filter.exists}},
#' \code{\link{emr_filter.create}}, \code{\link{emr_filter.rm}}
#' @keywords ~filter ~ls
#' @examples
#'
#' emr_db.init_examples()
#' emr_filter.create("f1", "dense_track", time.shift = c(2, 4))
#' emr_filter.create("f2", "dense_track", keepref = TRUE)
#' emr_filter.ls()
#' emr_filter.ls("*2")
#' @export emr_filter.ls
emr_filter.ls <- function(pattern = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
    if (!exists("EMR_FILTERS", envir = .GlobalEnv)) {
        return(character(0))
    }

    filters <- get("EMR_FILTERS", envir = .GlobalEnv)
    filternames <- names(filters)

    if (!is.list(filters) || (length(filters) && !is.character(filternames)) || length(filters) != length(filternames)) {
        stop("Invalid format of EMR_FILTERS variable.\nTo continue working with filters please remove this variable from the environment.", call. = FALSE)
    }

    if (is.null(filternames)) {
        return(character(0))
    }

    if (pattern != "") {
        sort(grep(pattern, filternames, value = TRUE, ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes))
    } else {
        sort(filternames)
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
#' emr_filter.create("f2", "dense_track", keepref = TRUE)
#' emr_filter.ls()
#' emr_filter.rm("f1")
#' emr_filter.ls()
#' @export emr_filter.rm
emr_filter.rm <- function(filter) {
    if (missing(filter)) {
        stop("Usage: emr_filter.rm(filter)", call. = FALSE)
    }

    if (exists("EMR_FILTERS", envir = .GlobalEnv)) {
        emr_filters <- get("EMR_FILTERS", envir = .GlobalEnv)
        emr_filters[[filter]] <- NULL

        assign("EMR_FILTERS", emr_filters, envir = .GlobalEnv)
    }

    return(NULL)
}

#' Clear all filters from the current environment
#'
#' @return None.
#'
#' @examples
#'
#' emr_db.init_examples()
#' emr_filter.create("f1", "dense_track", time.shift = c(2, 4))
#' emr_filter.ls()
#' emr_filter.clear()
#' emr_filter.ls()
#' @export
emr_filter.clear <- function() {
    EMR_FILTERS <<- list()
    return(NULL)
}
