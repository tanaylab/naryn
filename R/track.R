
.emr_tracks_filter <- function(..., tracks, ignore.case, perl, fixed, useBytes) {
    args <- as.list(substitute(list(...)))[-1L]
    args <- list(...)

    if (is.null(tracks) || !length(tracks)) {
          return(character(0))
      }

    if (length(args) >= 1) {
        attrs <- c()
        patterns <- c()

        # first filter out file names (this filtering is faster than filtering by track attribute)
        for (i in 1:length(args)) {
            arg <- as.character(args[[i]])
            if (is.null(names(args)) || names(args)[i] == "") {
                  tracks <- grep(arg, tracks, value = TRUE, ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes)
              } else {
                attrs <- c(attrs, names(args)[i])
                patterns <- c(patterns, as.character(args[[i]]))
            }
        }

        # filter out by attributes
        if (length(attrs)) {
            attrs_table <- .emr_call("emr_get_tracks_attrs", tracks, attrs, new.env(parent = parent.frame()))
            for (i in 1:length(attrs)) {
                tracks <- with(attrs_table, attrs_table[attr == attrs[i] & grepl(patterns[i], value), ])$track
                attrs_table <- attrs_table[attrs_table$track %in% tracks, ]
                if (!nrow(attrs_table)) {
                      return(character(0))
                  }
            }
        }
    }
    sort(tracks)
}

.emr_track.dir <- function(track) {
    if (is.na(match(track, .emr_call("emr_global_track_names", new.env(parent = parent.frame()), silent = TRUE)))) {
          dirname <- get("EMR_UROOT", envir = .GlobalEnv)
      } else {
          dirname <- get("EMR_GROOT", envir = .GlobalEnv)
      }
    dirname
}

.emr_track.filename <- function(track) {
    paste0(.emr_track.dir(track), "/", track, ".nrtrack")
}

.emr_track.var.dir <- function(track) {
    paste0(.emr_track.dir(track), "/.", track, ".var")
}

.emr_track.pyvar.dir <- function(track) {
    paste0(.emr_track.dir(track), "/.", track, ".pyvar")
}


.emr_dir.mv <- function(src, tgt) {
    dir.create(tgt, mode = "0777")
    file.copy(paste0(src, "/."), tgt, recursive = T)
    unlink(src, recursive = TRUE)
}


emr_track.addto <- function(track, src) {
    if (missing(track) || missing(src)) {
          stop("Usage: emr_track.addto(track, src)", call. = F)
      }
    .emr_checkroot()

    if (emr_track.readonly(track)) {
          stop(sprintf("Cannot add data to track %s: it is read-only.\n", track), call. = F)
      }

    .emr_call("emr_import", track, NULL, NULL, src, T, new.env(parent = parent.frame()))
    retv <- NULL
}

emr_track.attr.export <- function(track = NULL, attr = NULL) {
    .emr_checkroot()

    if (is.null(track)) {
          track <- .emr_call("emr_track_names", new.env(parent = parent.frame()), silent = TRUE)
      } else {
          track <- unique(track)
      }
    if (!is.null(attr)) {
          attr <- unique(attr)
      }

    .emr_call("emr_get_tracks_attrs", track, attr, new.env(parent = parent.frame()))
}

emr_track.attr.get <- function(track = NULL, attr = NULL) {
    if (missing(track) || missing(attr)) {
          stop("Usage: emr_track.attr.get(track, attr)", call. = F)
      }
    .emr_checkroot()

    res <- emr_track.attr.export(track, attr)
    if (nrow(res)) {
          res[1, 2]
      } else {
          NULL
      }
}

emr_track.attr.rm <- function(track = NULL, attr = NULL) {
    if (missing(track) || missing(attr)) {
          stop("Usage: emr_track.attr.rm(track, attr)", call. = F)
      }
    .emr_checkroot()

    .emr_call("emr_set_track_attr", track, attr, NULL, new.env(parent = parent.frame()))
    retv <- 0 # suppress return value
}

emr_track.attr.set <- function(track = NULL, attr = NULL, value = NULL) {
    if (missing(track) || missing(attr) || missing(value)) {
          stop("Usage: emr_track.attr.set(track, attr, value)", call. = F)
      }
    .emr_checkroot()

    .emr_call("emr_set_track_attr", track, attr, value, new.env(parent = parent.frame()))
    retv <- 0 # suppress return value
}

emr_track.create <- function(track, space, categorical, expr, stime = NULL, etime = NULL, iterator = NULL, keepref = F, filter = NULL) {
    if (missing(track) || missing(space) || missing(categorical) || missing(expr)) {
          stop("Usage: emr_track.create(track, space = \"user\", categorical, expr, stime = NULL, etime = NULL, iterator = NULL, keepref = F, filter = NULL)", call. = F)
      }
    .emr_checkroot()

    space <- tolower(space)
    if (space == "user" && (!exists("EMR_UROOT", envir = .GlobalEnv) || is.null(get("EMR_UROOT", envir = .GlobalEnv)))) {
          stop("User space root directory is not set. Please call emr_db.init(user.dir=...)", call. = F)
      }

    if (emr_track.exists(track)) {
          stop(sprintf("Track %s already exists", track), call. = F)
      }

    if (emr_vtrack.exists(track)) {
          stop(sprintf("Virtual track %s already exists", track), call. = F)
      }

    if (emr_filter.exists(track)) {
          stop(sprintf("Filter %s already exists", track), call. = F)
      }

    .emr_call("emr_track_create", track, space, categorical, expr, stime, etime, iterator, keepref, .emr_filter(filter), new.env(parent = parent.frame()))
    retv <- NULL
}

emr_track.exists <- function(track) {
    if (missing(track)) {
          stop("Usage: emr_track.exist(track)", call. = F)
      }
    .emr_checkroot()
    !is.na(match(track, .emr_call("emr_track_names", new.env(parent = parent.frame()), silent = TRUE)))
}

emr_track.ids <- function(track) {
    if (missing(track)) {
          stop("Usage: emr_track.ids(track)", call. = F)
      }
    .emr_checkroot()

    .emr_call("emr_track_ids", track, new.env(parent = parent.frame()))
}

emr_track.import <- function(track, space, categorical, src) {
    if (missing(track) || missing(space) || missing(src) || missing(categorical)) {
          stop("Usage: emr_track.import(track, space, categorical, src)", call. = F)
      }
    .emr_checkroot()

    space <- tolower(space)
    if (space == "user" && (!exists("EMR_UROOT", envir = .GlobalEnv) || is.null(get("EMR_UROOT", envir = .GlobalEnv)))) {
          stop("User space root directory is not set. Please call emr_db.init(user.dir=...)", call. = F)
      }

    if (emr_vtrack.exists(track)) {
          stop(sprintf("Virtual track %s already exists", track), call. = F)
      }

    if (emr_filter.exists(track)) {
          stop(sprintf("Filter %s already exists", track), call. = F)
      }

    .emr_call("emr_import", track, space, categorical, src, F, new.env(parent = parent.frame()))
    retv <- NULL
}

emr_track.info <- function(track) {
    if (missing(track)) {
          stop("Usage: emr_track.info(track)", call. = F)
      }
    .emr_checkroot()

    .emr_call("emr_track_info", track, new.env(parent = parent.frame()))
}

emr_track.ls <- function(..., ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
    .emr_checkroot()
    tracks <- .emr_call("emr_track_names", new.env(parent = parent.frame()), silent = TRUE)
    .emr_tracks_filter(..., tracks = tracks, ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes)
}

emr_track.mv <- function(src, tgt, space = NULL) {
    if (missing(src) || missing(tgt)) {
          stop("Usage: emr_track.mv(src, tgt, space = NULL)", call. = F)
      }
    .emr_checkroot()

    if (!is.null(space)) {
        space <- tolower(space)
        if (space == "user" && (!exists("EMR_UROOT", envir = .GlobalEnv) || is.null(get("EMR_UROOT", envir = .GlobalEnv)))) {
              stop("User space root directory is not set. Please call emr_db.init(user.dir=...)", call. = F)
          }
    }

    if (emr_track.readonly(src)) {
          stop(sprintf("Cannot move track %s: it is read-only.\n", src), call. = F)
      }

    if (emr_vtrack.exists(tgt)) {
          stop(sprintf("Virtual track %s already exists", tgt), call. = F)
      }

    if (emr_filter.exists(tgt)) {
          stop(sprintf("Filter %s already exists", tgt), call. = F)
      }

    dirname1 <- .emr_track.var.dir(src)
    dirname2 <- .emr_track.pyvar.dir(src)

    .emr_call("emr_track_mv", src, tgt, space, new.env(parent = parent.frame()))

    if (file.exists(dirname1)) {
          .emr_dir.mv(dirname1, .emr_track.var.dir(tgt))
      }

    if (file.exists(dirname2)) {
          .emr_dir.mv(dirname2, .emr_track.pyvar.dir(tgt))
      }

    retv <- NULL
}

emr_track.percentile <- function(track, val, lower = T) {
    if (missing(track) || missing(val)) {
          stop("Usage: emr_track.percentile(track, val, lower)", call. = F)
      }
    .emr_checkroot()
    .emr_call("emr_track_percentile", track, val, lower, new.env(parent = parent.frame()))
}

emr_track.readonly <- function(track, readonly = NULL) {
    if (missing(track)) {
          stop("Usage: emr_track.readonly(track, readonly = NULL)", call. = F)
      }
    .emr_checkroot()

    if (!emr_track.exists(track)) {
          stop(sprintf("Track %s does not exist", track), call. = F)
      }

    file <- .emr_track.filename(track)
    if (file.access(file, 0) == -1) {
          stop(sprintf("File %s does not exist", file), call. = F)
      }

    if (is.null(readonly)) {
        # read-only == no write permissions
        if (file.access(file, 2) == 0) {
              return(FALSE)
          }
        return(TRUE)
    }

    if (readonly) {
          mode <- "444"
      } else {
          mode <- "666"
      }

    if (Sys.chmod(file, mode, use_umask = F) == FALSE) {
          stop(sprintf("Failed to set read-only attribute for track %s", track), call. = F)
      }
    retv <- NULL
}

emr_track.rm <- function(track, force = F) {
    if (missing(track)) {
          stop("Usage: emr_track.rm(track, force = F)", call. = F)
      }
    .emr_checkroot()

    if (!emr_track.exists(track)) {
        if (force) {
              return(invisible())
          }
        stop(sprintf("Track %s does not exist", track), call. = F)
    }

    readonly <- F
    if (force) {
          tryCatch({
              readonly <- emr_track.readonly(track)
          })
      } else {
          readonly <- emr_track.readonly(track)
      }

    if (readonly) {
          stop(sprintf("Cannot remove track %s: it is read-only.\n", track), call. = F)
      }

    answer <- "N"
    if (force) {
          answer <- "Y"
      } else {
        str <- sprintf("Are you sure you want to delete track %s (Y/N)? ", track)
        cat(str)
        answer <- toupper(readLines(n = 1))
    }

    if (answer == "Y" || answer == "YES") {
        dirname1 <- .emr_track.var.dir(track)
        dirname2 <- .emr_track.pyvar.dir(track)
        .emr_call("emr_track_rm", track, new.env(parent = parent.frame()))

        if (file.exists(dirname1)) {
              unlink(dirname1, recursive = TRUE)
          }

        if (file.exists(dirname2)) {
              unlink(dirname2, recursive = TRUE)
          }
    }

    retv <- NULL
}

emr_track.global.ls <- function(..., ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
    .emr_checkroot()
    tracks <- .emr_call("emr_global_track_names", new.env(parent = parent.frame()), silent = TRUE)
    .emr_tracks_filter(..., tracks = tracks, ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes)
}

emr_track.user.ls <- function(..., ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
    .emr_checkroot()
    tracks <- .emr_call("emr_user_track_names", new.env(parent = parent.frame()), silent = TRUE)
    .emr_tracks_filter(..., tracks = tracks, ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes)
}

emr_track.unique <- function(track) {
    if (missing(track)) {
          stop("Usage: emr_track.unique(track)", call. = F)
      }
    .emr_checkroot()

    .emr_call("emr_track_unique", track, new.env(parent = parent.frame()))
}

emr_track.var.get <- function(track, var) {
    if (missing(track) || missing(var)) {
          stop("Usage: emr_track.var.get(track, var)", call. = F)
      }
    .emr_checkroot()

    if (!emr_track.exists(track)) {
          stop(sprintf("Track %s does not exist", track), call. = F)
      }

    filename <- paste(.emr_track.var.dir(track), var, sep = "/")
    if (!file.exists(filename)) {
          stop(sprintf("Track variable %s does not exist", var), call. = F)
      }

    f <- file(filename, "rb")
    val <- unserialize(f)
    close(f)
    val
}

emr_track.var.ls <- function(track, pattern = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
    if (missing(track)) {
          stop("Usage: emr_track.var.ls(track, pattern = \"\", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)", call. = F)
      }
    .emr_checkroot()

    if (!emr_track.exists(track)) {
          stop(sprintf("Track %s does not exist", track), call. = F)
      }

    dirname <- .emr_track.var.dir(track)

    options(warn = -1) # disable warnings since dir() on non dir or non existing dir produces warnings
    invisible(files <- dir(dirname))
    options(warn = 0) # restore the warning behavior
    if (length(files) > 0 && pattern != "") {
          sort(grep(pattern, files, value = TRUE, ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes))
      } else {
          sort(files)
      }
}

emr_track.var.rm <- function(track, var) {
    if (missing(track) || missing(var)) {
          stop("Usage: emr_track.var.rm(track, var)", call. = F)
      }
    .emr_checkroot()

    if (!emr_track.exists(track)) {
          stop(sprintf("Track %s does not exist", track), call. = F)
      }

    if (emr_track.readonly(track)) {
          stop(sprintf("Cannot remove vars from track %s: it is read-only.\n", track), call. = F)
      }

    dirname <- .emr_track.var.dir(track)
    filename <- paste(dirname, var, sep = "/")
    if (!file.exists(filename)) {
          stop(sprintf("Track variable %s does not exist", var), call. = F)
      }

    file.remove(filename)

    if (!length(dir(dirname))) {
          unlink(dirname, recursive = TRUE)
      }

    retv <- NULL
}

emr_track.var.set <- function(track, var, value) {
    if (missing(track) || missing(var) || missing(value)) {
          stop("Usage: emr_track.var.set(track, var, value)", call. = F)
      }
    .emr_checkroot()

    if (!emr_track.exists(track)) {
          stop(sprintf("Track %s does not exist", track), call. = F)
      }

    if (emr_track.readonly(track)) {
          stop(sprintf("Cannot set vars for track %s: it is read-only.\n", track), call. = F)
      }

    dirname <- .emr_track.var.dir(track)

    if (!file.exists(dirname)) {
          dir.create(dirname, mode = "0777")
      }

    filename <- paste(dirname, var, sep = "/")

    # save the variable
    f <- file(filename, "wb")
    serialize(value, f)
    close(f)
}
