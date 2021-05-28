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
    filter
}


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

    if (is.character(src) && length(src) == 1 && !is.na(match(src, .emr_call("emr_user_track_names", new.env(parent = parent.frame()), silent = TRUE)))) {
          root <- get("EMR_UROOT", envir = .GlobalEnv)
      } else {
          root <- get("EMR_GROOT", envir = .GlobalEnv)
      }

    var <- list(src = src, time_shift = time.shift, keepref = keepref, val = val, expiration = expiration)
    .emr_call("emr_check_named_filter", var, filter, new.env(parent = parent.frame()))
    emr_filter.rm(filter)
    EMR_FILTERS[[root]][[filter]] <<- var

    retv <- NULL
}

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

    if (missing(src)) {
          filter.var$src
      } else {
        .emr_call("emr_check_filter_attr_src", src, new.env(parent = parent.frame()))
        emr_filter.rm(filter)
        filter.var$src <- src
        if (is.character(src) && length(src) == 1 && !is.na(match(src, .emr_call("emr_user_track_names", new.env(parent = parent.frame()), silent = TRUE)))) {
              root <- get("EMR_UROOT", envir = .GlobalEnv)
          } else {
              root <- get("EMR_GROOT", envir = .GlobalEnv)
          }
        EMR_FILTERS[[root]][[filter]] <<- filter.var
        retv <- NULL
    }
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

    if (missing(val)) {
          filter.var$val
      } else {
        if (!is.numeric(val)) {
              stop("'val' parameter must be a numeric vector", call. = F)
          }

        EMR_FILTERS[[root]][[filter]]["val"] <<- unique(list(val))
        retv <- NULL
    }
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

emr_filter.info <- function(filter) {
    if (missing(filter)) {
          stop("Usage: emr_filter.info(filter)", call. = F)
      }
    .emr_checkroot()

    .emr_filter.get(filter)
}

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
