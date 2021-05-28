.emr_vtrack.get <- function(vtrackstr) {
    if (!emr_vtrack.exists(vtrackstr)) {
          stop(sprintf("Virtual track %s does not exist", vtrackstr), call. = F)
      }

    root <- get("EMR_GROOT", envir = .GlobalEnv)
    vtrack <- get("EMR_VTRACKS", envir = .GlobalEnv)[[root]][[vtrackstr]]
    if (is.null(vtrack)) {
        root <- get("EMR_UROOT", envir = .GlobalEnv)
        vtrack <- get("EMR_VTRACKS", envir = .GlobalEnv)[[root]][[vtrackstr]]
    }
    vtrack
}

emr_vtrack.create <- function(vtrack, src, func = NULL, params = NULL, keepref = F, time.shift = NULL, id.map = NULL, filter = NULL) {
    if (missing(vtrack) || missing(src)) {
          stop("Usage: emr_vtrack.create(vtrack, src, func = NULL, params = NULL, keepref = F, time.shift = NULL, id.map = NULL, filter = NULL)", call. = F)
      }
    .emr_checkroot()

    if (vtrack != make.names(vtrack)) {
          stop(sprintf("\"%s\" is not a syntactically valid name for a variable", vtrack), call. = F)
      }

    if (!exists("EMR_VTRACKS", envir = .GlobalEnv)) {
          EMR_VTRACKS <<- list()
      }

    if (emr_track.exists(vtrack)) {
          stop(sprintf("Track %s already exists", vtrack), call. = F)
      }

    if (emr_filter.exists(vtrack)) {
          stop(sprintf("Filter %s already exists", vtrack), call. = F)
      }

    if (is.character(src) && length(src) == 1 && !is.na(match(src, .emr_call("emr_user_track_names", new.env(parent = parent.frame()), silent = TRUE)))) {
          root <- get("EMR_UROOT", envir = .GlobalEnv)
      } else {
          root <- get("EMR_GROOT", envir = .GlobalEnv)
      }

    var <- list(src = src, time_shift = time.shift, func = func, params = params, keepref = keepref, id_map = id.map, filter = .emr_filter(filter))
    .emr_call("emr_check_vtrack", vtrack, var, new.env(parent = parent.frame()))
    emr_vtrack.rm(vtrack)
    EMR_VTRACKS[[root]][[vtrack]] <<- var

    retv <- NULL
}

emr_vtrack.attr.src <- function(vtrack, src) {
    if (missing(vtrack)) {
          stop("Usage: emr_vtrack.attr.src(vtrack, src)", call. = F)
      }
    .emr_checkroot()

    root <- get("EMR_GROOT", envir = .GlobalEnv)
    vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[root]][[vtrack]]
    if (is.null(vtrack.var)) {
        root <- get("EMR_UROOT", envir = .GlobalEnv)
        vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[root]][[vtrack]]
    }

    if (is.null(vtrack.var)) {
          stop(sprintf("Virtual track \"%s\" does not exist", vtrack), call. = F)
      }

    if (missing(src)) {
          vtrack.var$src
      } else {
        .emr_call("emr_check_vtrack_attr_src", src, new.env(parent = parent.frame()))
        emr_vtrack.rm(vtrack)
        vtrack.var$src <- src
        if (is.character(src) && length(src) == 1 && !is.na(match(src, .emr_call("emr_user_track_names", new.env(parent = parent.frame()), silent = TRUE)))) {
              root <- get("EMR_UROOT", envir = .GlobalEnv)
          } else {
              root <- get("EMR_GROOT", envir = .GlobalEnv)
          }
        EMR_VTRACKS[[root]][[vtrack]] <<- vtrack.var
        retv <- NULL
    }
}

emr_vtrack.attr.func <- function(vtrack, func) {
    if (missing(vtrack)) {
          stop("Usage: emr_vtrack.attr.func(vtrack, func)", call. = F)
      }
    .emr_checkroot()

    root <- get("EMR_GROOT", envir = .GlobalEnv)
    vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[root]][[vtrack]]
    if (is.null(vtrack.var)) {
        root <- get("EMR_UROOT", envir = .GlobalEnv)
        vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[root]][[vtrack]]
    }

    if (is.null(vtrack.var)) {
          stop(sprintf("Virtual track \"%s\" does not exist", vtrack), call. = F)
      }

    if (missing(func)) {
          vtrack.var$func
      } else {
        .emr_call("emr_check_vtrack_attr_func", func, new.env(parent = parent.frame()))
        EMR_VTRACKS[[root]][[vtrack]]["func"] <<- list(func)
        retv <- NULL
    }
}

emr_vtrack.attr.params <- function(vtrack, params) {
    if (missing(vtrack)) {
          stop("Usage: emr_vtrack.attr.params(vtrack, params)", call. = F)
      }
    .emr_checkroot()

    root <- get("EMR_GROOT", envir = .GlobalEnv)
    vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[root]][[vtrack]]
    if (is.null(vtrack.var)) {
        root <- get("EMR_UROOT", envir = .GlobalEnv)
        vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[root]][[vtrack]]
    }

    if (is.null(vtrack.var)) {
          stop(sprintf("Virtual track \"%s\" does not exist", vtrack), call. = F)
      }

    if (missing(params)) {
          vtrack.var$params
      } else {
        EMR_VTRACKS[[root]][[vtrack]]["params"] <<- list(params)
        retv <- NULL
    }
}

emr_vtrack.attr.keepref <- function(vtrack, keepref) {
    if (missing(vtrack)) {
          stop("Usage: emr_vtrack.attr.keepref(vtrack, keepref)", call. = F)
      }
    .emr_checkroot()

    root <- get("EMR_GROOT", envir = .GlobalEnv)
    vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[root]][[vtrack]]
    if (is.null(vtrack.var)) {
        root <- get("EMR_UROOT", envir = .GlobalEnv)
        vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[root]][[vtrack]]
    }

    if (is.null(vtrack.var)) {
          stop(sprintf("Virtual track \"%s\" does not exist", vtrack), call. = F)
      }

    if (missing(keepref)) {
          vtrack.var$keepref
      } else {
        if (!is.logical(keepref) || is.na(keepref)) {
              stop("'keepref' parameter must be logical", call. = F)
          }

        EMR_VTRACKS[[root]][[vtrack]]["keepref"] <<- list(keepref)
        retv <- NULL
    }
}

emr_vtrack.attr.time.shift <- function(vtrack, time.shift) {
    if (missing(vtrack)) {
          stop("Usage: emr_vtrack.attr.time.shift(vtrack, time.shift)", call. = F)
      }
    .emr_checkroot()

    root <- get("EMR_GROOT", envir = .GlobalEnv)
    vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[root]][[vtrack]]
    if (is.null(vtrack.var)) {
        root <- get("EMR_UROOT", envir = .GlobalEnv)
        vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[root]][[vtrack]]
    }

    if (is.null(vtrack.var)) {
          stop(sprintf("Virtual track \"%s\" does not exist", vtrack), call. = F)
      }

    if (missing(time.shift)) {
          vtrack.var$time_shift
      } else {
        .emr_call("emr_check_vtrack_attr_time_shift", time.shift, new.env(parent = parent.frame()))
        EMR_VTRACKS[[root]][[vtrack]]["time_shift"] <<- list(time.shift)
        retv <- NULL
    }
}

emr_vtrack.attr.id.map <- function(vtrack, id.map) {
    if (missing(vtrack)) {
          stop("Usage: emr_vtrack.attr.id.map(vtrack, id.map)", call. = F)
      }
    .emr_checkroot()

    root <- get("EMR_GROOT", envir = .GlobalEnv)
    vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[root]][[vtrack]]
    if (is.null(vtrack.var)) {
        root <- get("EMR_UROOT", envir = .GlobalEnv)
        vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[root]][[vtrack]]
    }

    if (is.null(vtrack.var)) {
          stop(sprintf("Virtual track \"%s\" does not exist", vtrack), call. = F)
      }

    if (missing(id.map)) {
          vtrack.var$id.map
      } else {
        .emr_call("emr_check_vtrack_attr_id_map", id.map, new.env(parent = parent.frame()))
        EMR_VTRACKS[[root]][[vtrack]]["id.map"] <<- list(id.map)
        retv <- NULL
    }
}

emr_vtrack.attr.filter <- function(vtrack, filter) {
    if (missing(vtrack)) {
          stop("Usage: emr_vtrack.attr.filter(vtrack, filter)", call. = F)
      }
    .emr_checkroot()

    root <- get("EMR_GROOT", envir = .GlobalEnv)
    vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[root]][[vtrack]]
    if (is.null(vtrack.var)) {
        root <- get("EMR_UROOT", envir = .GlobalEnv)
        vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[root]][[vtrack]]
    }

    if (is.null(vtrack.var)) {
          stop(sprintf("Virtual track \"%s\" does not exist", vtrack), call. = F)
      }

    if (missing(filter)) {
          vtrack.var$filter
      } else {
        .emr_call("emr_check_vtrack_attr_filter", .emr_filter(filter), new.env(parent = parent.frame()))
        EMR_VTRACKS[[root]][[vtrack]]["filter"] <<- list(.emr_filter(filter))
        retv <- NULL
    }
}

emr_vtrack.exists <- function(vtrack) {
    if (missing(vtrack)) {
          stop("Usage: emr_vtrack.exists(vtrack)", call. = F)
      }
    .emr_checkroot()

    res <- FALSE
    if (exists("EMR_VTRACKS", envir = .GlobalEnv)) {
        vtracks <- get("EMR_VTRACKS", envir = .GlobalEnv)
        res <- !is.null(vtracks[[get("EMR_GROOT", envir = .GlobalEnv)]][[vtrack]]) ||
            exists("EMR_UROOT", envir = .GlobalEnv) && !is.null(get("EMR_UROOT", envir = .GlobalEnv)) && !is.null(vtracks[[get("EMR_UROOT", envir = .GlobalEnv)]][[vtrack]])
    }
    res
}

emr_vtrack.info <- function(vtrack) {
    if (missing(vtrack)) {
          stop("Usage: emr_vtrack.info(vtrack)", call. = F)
      }
    .emr_checkroot()

    .emr_vtrack.get(vtrack)
}

emr_vtrack.ls <- function(pattern = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
    .emr_checkroot()

    if (!exists("EMR_VTRACKS", envir = .GlobalEnv)) {
          return(NULL)
      }

    emr_vtracks <- get("EMR_VTRACKS", envir = .GlobalEnv)
    emr_roots <- names(emr_vtracks)
    if (!is.list(emr_vtracks) || (length(emr_vtracks) && !is.character(emr_roots)) || length(emr_vtracks) != length(emr_roots)) {
          stop("Invalid format of EMR_VTRACKS variable.\nTo continue working with virtual tracks please remove this variable from the environment.", call. = F)
      }

    all.vtracknames <- NULL
    roots <- c("EMR_GROOT", "EMR_UROOT")

    for (root in roots) {
        if (exists(root, envir = .GlobalEnv) && !is.null(get(root, envir = .GlobalEnv))) {
            emr_root <- get(root, envir = .GlobalEnv)
            idx <- match(emr_root, emr_roots)
            if (!is.na(idx)) {
                vtracks <- emr_vtracks[[idx]]
                vtracknames <- names(vtracks)
                if (!is.list(vtracks) || (length(vtracks) && !is.character(vtracknames)) || length(vtracks) != length(vtracknames)) {
                      stop("Invalid format of EMR_VTRACKS variable.\nTo continue working with virtual tracks please remove this variable from the environment.", call. = F)
                  }
                all.vtracknames <- c(all.vtracknames, vtracknames)
            }
        }
    }

    if (is.null(all.vtracknames)) {
          return(NULL)
      }

    if (pattern != "") {
          sort(grep(pattern, all.vtracknames, value = TRUE, ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes))
      } else {
          sort(all.vtracknames)
      }
}

emr_vtrack.rm <- function(vtrack) {
    if (missing(vtrack)) {
          stop("Usage: emr_vtrack.rm(vtrack)", call. = F)
      }
    .emr_checkroot()

    if (exists("EMR_VTRACKS", envir = .GlobalEnv)) {
        emr_vtracks <- get("EMR_VTRACKS", envir = .GlobalEnv)
        emr_vtracks[[get("EMR_GROOT", envir = .GlobalEnv)]][[vtrack]] <- NULL

        if (exists("EMR_UROOT", envir = .GlobalEnv) && !is.null(get("EMR_UROOT", envir = .GlobalEnv))) {
              emr_vtracks[[get("EMR_UROOT", envir = .GlobalEnv)]][[vtrack]] <- NULL
          }

        assign("EMR_VTRACKS", emr_vtracks, envir = .GlobalEnv)
    }
    retv <- NULL
}
