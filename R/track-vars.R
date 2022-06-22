.emr_track.var.dir <- function(track) {
    paste0(.emr_track.dir(track), "/.", track, ".var")
}

.emr_track.pyvar.dir <- function(track) {
    paste0(.emr_track.dir(track), "/.", track, ".pyvar")
}

#' Returns value of a track variable
#'
#' Returns value of a track variable.
#'
#' This function returns the value of a track variable. If the variable does
#' not exist NULL is returned.
#'
#' @param track track name
#' @param var track variable name
#' @return Track variable value. If the variable does not exists, NULL is returned.
#' @seealso \code{\link{emr_track.var.set}}, \code{\link{emr_track.var.ls}},
#' \code{\link{emr_track.var.rm}}
#' @keywords ~variable
#' @examples
#'
#' emr_db.init_examples()
#' emr_track.var.set("sparse_track", "test_var", 1:10)
#' emr_track.var.get("sparse_track", "test_var")
#' emr_track.var.rm("sparse_track", "test_var")
#' @export emr_track.var.get
emr_track.var.get <- function(track, var) {
    if (missing(track) || missing(var)) {
        stop("Usage: emr_track.var.get(track, var)", call. = FALSE)
    }
    .emr_checkroot()

    if (!emr_track.exists(track)) {
        stop(sprintf("Track %s does not exist", track), call. = FALSE)
    }

    if (emr_track.logical.exists(track)) {
        dirname <- .emr_track.logical.var.dir(track)
    } else {
        dirname <- .emr_track.var.dir(track)
    }

    filename <- paste(dirname, var, sep = "/")

    if (!file.exists(filename)) {
        return(NULL)
    }

    f <- file(filename, "rb")
    val <- unserialize(f)
    close(f)
    val
}



#' Returns a list of track variables for a track
#'
#' Returns a list of track variables for a track.
#'
#' This function returns a list of track variables of a track that match the
#' pattern (see 'grep'). If called without any arguments all track variables of
#' a track are returned.
#'
#' Overriding a track also overrides it's track variables, the
#' variables will persist when the track is no longer overridden
#'
#' @param track track name
#' @param pattern,ignore.case,perl,fixed,useBytes see 'grep'
#' @return An array that contains the names of track variables.
#' @seealso \code{\link{grep}}, \code{\link{emr_track.var.get}},
#' \code{\link{emr_track.var.set}}, \code{\link{emr_track.var.rm}}
#' @keywords ~variable ~ls
#' @examples
#'
#' emr_db.init_examples()
#' emr_track.var.ls("sparse_track")
#' emr_track.var.set("sparse_track", "test_var1", 1:10)
#' emr_track.var.set("sparse_track", "test_var2", "v")
#' emr_track.var.ls("sparse_track")
#' emr_track.var.ls("sparse_track", pattern = "2")
#' emr_track.var.rm("sparse_track", "test_var1")
#' emr_track.var.rm("sparse_track", "test_var2")
#' @export emr_track.var.ls
emr_track.var.ls <- function(track, pattern = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
    if (missing(track)) {
        stop("Usage: emr_track.var.ls(track, pattern = \"\", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)", call. = FALSE)
    }
    .emr_checkroot()

    if (!emr_track.exists(track)) {
        stop(sprintf("Track %s does not exist", track), call. = FALSE)
    }

    if (emr_track.logical.exists(track)) {
        dirname <- .emr_track.logical.var.dir(track)
    } else {
        dirname <- .emr_track.var.dir(track)
    }

    options(warn = -1) # disable warnings since dir() on non dir or non existing dir produces warnings
    invisible(files <- dir(dirname))
    options(warn = 0) # restore the warning behavior
    if (length(files) > 0 && pattern != "") {
        sort(grep(pattern, files, value = TRUE, ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes))
    } else {
        sort(files)
    }
}



#' Deletes a track variable
#'
#' Deletes a track variable.
#'
#' This function deletes a track variable.
#'
#' @param track track name
#' @param var track variable name
#' @return None.
#' @seealso \code{\link{emr_track.var.get}}, \code{\link{emr_track.var.set}},
#' \code{\link{emr_track.var.ls}}
#' @keywords ~variable
#' @examples
#'
#' emr_db.init_examples()
#' emr_track.var.set("sparse_track", "test_var1", 1:10)
#' emr_track.var.set("sparse_track", "test_var2", "v")
#' emr_track.var.ls("sparse_track")
#' emr_track.var.rm("sparse_track", "test_var1")
#' emr_track.var.rm("sparse_track", "test_var2")
#' emr_track.var.ls("sparse_track")
#' @export emr_track.var.rm
emr_track.var.rm <- function(track, var) {
    if (missing(track) || missing(var)) {
        stop("Usage: emr_track.var.rm(track, var)", call. = FALSE)
    }
    .emr_checkroot()

    if (!emr_track.exists(track)) {
        stop(sprintf("Track %s does not exist", track), call. = FALSE)
    }

    if (emr_track.readonly(track)) {
        stop(sprintf("Cannot remove vars from track %s: it is read-only.\n", track), call. = FALSE)
    }
    if (emr_track.logical.exists(track)) {
        dirname <- .emr_track.logical.var.dir(track)
    } else {
        dirname <- .emr_track.var.dir(track)
    }

    filename <- paste(dirname, var, sep = "/")
    if (!file.exists(filename)) {
        stop(sprintf("Track variable %s does not exist", var), call. = FALSE)
    }

    file.remove(filename)

    if (!length(dir(dirname))) {
        unlink(dirname, recursive = TRUE)
    }
}



#' Assigns value to a track variable
#'
#' Assigns value to a track variable.
#'
#' This function creates a track variable and assigns 'value' to it. If the
#' track variable already exists its value is overwritten.
#'
#' @param track track name
#' @param var track variable name
#' @param value value
#' @return None.
#' @seealso \code{\link{emr_track.var.get}}, \code{\link{emr_track.var.ls}},
#' \code{\link{emr_track.var.rm}}
#' @keywords ~variable
#' @examples
#'
#' emr_db.init_examples()
#' emr_track.var.set("sparse_track", "test_var", 1:10)
#' emr_track.var.get("sparse_track", "test_var")
#' emr_track.var.rm("sparse_track", "test_var")
#' @export emr_track.var.set
emr_track.var.set <- function(track, var, value) {
    if (missing(track) || missing(var) || missing(value)) {
        stop("Usage: emr_track.var.set(track, var, value)", call. = FALSE)
    }
    .emr_checkroot()

    if (!emr_track.exists(track)) {
        stop(sprintf("Track %s does not exist", track), call. = FALSE)
    }

    if (emr_track.readonly(track)) {
        stop(sprintf("Cannot set vars for track %s: it is read-only.\n", track), call. = FALSE)
    }

    if (emr_track.logical.exists(track)) {
        dirname <- .emr_track.logical.var.dir(track)
    } else {
        dirname <- .emr_track.var.dir(track)
    }

    if (!file.exists(dirname)) {
        dir.create(dirname, mode = "0777")
    }

    filename <- paste(dirname, var, sep = "/")

    # save the variable
    f <- file(filename, "wb")
    serialize(value, f)
    close(f)
}
