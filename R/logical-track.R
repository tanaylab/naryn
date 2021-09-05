.emr_track.logical.dir <- function() {
    dirname <- get("EMR_GROOT", envir = .GlobalEnv)
    paste0(dirname, "/logical")
}

.emr_track.logical.filename <- function(track) {
    paste0(.emr_track.logical.dir(), "/", track, ".ltrack")
}

.emr_track.logical.var.dir <- function(track) {
    paste0(.emr_track.logical.dir(), "/.", track, ".var")
}

.emr_track.logical.pyvar.dir <- function(track) {
    paste0(.emr_track.logical.dir(), "/.", track, ".pyvar")
}

#' Creates a logical track
#'
#' Creates a logical track
#'
#' This function creates a logical track based on an existing categorical track
#' in the global space.
#'
#'
#' @param track the name of the newly created logical track
#' @param src name of the physical track
#' @param values vector of selected values
#' @return None.
#'
#' @keywords ~track ~create_logical
#' @export emr_track.create_logical
emr_track.create_logical <- function(track, src, values = NULL) {
    .emr_checkroot()
    .emr_call("emr_create_logical", track, src, values, new.env(parent = parent.frame()), silent = TRUE)
}

#' Deletes a logical track
#'
#' @param track the name of the newly created logical track
#' @param force if 'TRUE', supresses user confirmation of a named track removal
#' @return None.
#'
#' @keywords ~track ~create_logical
#' @export emr_track.create_logical
emr_track.logical.rm <- function(track, force = FALSE, rm_vars = TRUE) {
    .emr_checkroot()
    if (!emr_track.exists(track)) {
        if (force) {
            return(invisible())
        }
        stop(sprintf("Track %s does not exist", track), call. = F)
    }

    if (!emr_track.logical.exists(track)) {
        stop(sprintf("Track %s is not a logical track", track), call. = F)
    }

    answer <- "N"
    if (force) {
        answer <- "Y"
    } else {
        str <- sprintf("Are you sure you want to delete logical track %s (Y/N)? ", track)
        cat(str)
        answer <- toupper(readLines(n = 1))
    }

    if (answer == "Y" || answer == "YES") {
        dirname1 <- .emr_track.logical.var.dir(track)
        dirname2 <- .emr_track.logical.pyvar.dir(track)
        .emr_call("emr_remove_logical", track, new.env(parent = parent.frame()), silent = TRUE)

        if (rm_vars && file.exists(dirname1)) {
            unlink(dirname1, recursive = TRUE)
        }

        if (rm_vars && file.exists(dirname2)) {
            unlink(dirname2, recursive = TRUE)
        }
    }
}


#' Is a track logical
#'
#' @param name of the track
#'
#' @return TRUE if \code{track} is a logical track and FALSE otherwise
#'
#' @examples
#'
#' emr_track.logical.exists("logical_track")
#' @noRd
emr_track.logical.exists <- function(track) {
    .emr_checkroot()
    .emr_call("emr_is_logical", track, new.env(parent = parent.frame()), silent = TRUE)
}

#' Returns information about a logical track
#'
#' Returns information about a logical track
#'
#' This function returns the source and values of a logical track
#'
#'
#' @param track track name
#' @return A list that contains \code{source} - the source of the logical track, and \code{values}: the values of the logical track.
#' @seealso \code{\link{emr_track.ls}}
#' @keywords ~track ~info ~property
#' @examples
#'
#' emr_db.init_examples()
#' emr_track.logical.info("logical_track")
#' @noRd
emr_track.logical.info <- function(track) {
    if (missing(track)) {
        stop("Usage: emr_track.logical.info(track)", call. = F)
    }
    .emr_checkroot()

    .emr_call("emr_logical_track_info", track, new.env(parent = parent.frame()))
}

#' Detect logical tracks in a track expression
#'
#' @param expr track expression (string)
#'
#' @return vector with names of logical tracks in the track expression
#'
#' @examples
#'
#' detect_expr_logical_tracks("logical_track+5")
#' @noRd
detect_expr_logical_tracks <- function(expr) {
    .emr_call("emr_expr_logical_tracks", expr, new.env(parent = parent.frame()))
}

#' Detect physical tracks in a track expression
#'
#' @param expr track expression (string)
#'
#' @return vector with names of physical tracks in the track expression
#'
#' @examples
#'
#' detect_expr_physical_tracks("dense_track+5")
#' @noRd
detect_expr_physical_tracks <- function(expr) {
    .emr_call("emr_expr_physical_tracks", expr, new.env(parent = parent.frame()))
}

#' Detect virtual tracks in a track expression
#'
#' @param expr track expression (string)
#'
#' @return vector with names of virtual tracks in the track expression
#'
#' @examples
#'
#' detect_expr_virtual_tracks("dense_track+5")
#' @noRd
detect_expr_virtual_tracks <- function(expr) {
    .emr_call("emr_expr_virtual_tracks", expr, new.env(parent = parent.frame()))
}


#' Detect a single track in a track expression
#'
#' @description
#' When the iterator is NULL - we want to detect a single
#' track in the track expression in order to change the iterator to it
#'
#' @param exprs vector of track expressions
#'
#' @return name of the track if there exists only one in the expression and NULL otherwise.
#' An error is thrown if there is more than a single track
#'
#'
#' @noRd
expand_null_iterator <- function(exprs) {
    tracks <- c()
    vtracks <- c()

    for (expr in exprs) {
        tracks <- c(tracks, detect_expr_logical_tracks(expr))
        tracks <- c(tracks, detect_expr_physical_tracks(expr))
        vtracks <- c(vtracks, detect_expr_virtual_tracks(expr))
    }

    # naryn doesn't allow explicit vtrack iterator
    if (length(tracks) == 1 && length(vtracks) == 0) {
        return(tracks)
    }

    if (length(vtracks) == 1) {
        vtrack_info <- emr_vtrack.info(vtracks)
        src <- vtrack_info$src

        if (is.character(src) && emr_track.logical.exists(src)) {
            return(src)
        }
    }

    if (length(c(tracks, vtracks)) > 1) {
        stop("Unable to implicitly set iterator policy: track expression contains several different data sources")
    }

    return(NULL)
}

random_filter_name <- function(pattern) {
    basename(tempfile(pattern = pattern))
}

#' Create a filter for logical track
#'
#' @param ltrack output of \code{emr_track.logical.info}
#' @param filter existing filter (the new filter would be added)
#' @param filter_name name for the new filter (optional)
#'
#' @return a string with the logical track filter (i.e. filter for the values of the original track), added (with '&' operator) to the original filter (if exists).
#'
#'
#' @examples
#'
#' ltrack <- emr_track.logical.info("logical_track")
#' create_logical_track_filter("logical_track")
#' @noRd
create_logical_track_filter <- function(ltrack, filter = NULL, filter_name = NULL, env = parent.frame()) {
    if (is.null(filter_name)) {
        filter_name <- random_filter_name("logical_filter_")
        withr::defer(
            emr_filter.rm(filter_name),
            envir = env
        )
    }
    emr_filter.create(filter_name, src = ltrack$source, val = ltrack$values, keepref = TRUE)

    res <- filter_name
    if (!is.null(filter)) {
        res <- glue::glue("({filter_name}) & ({filter})")
    }

    return(res)
}


#' Get a list of logical track names which depend on the src track given
#'
#' @param src a string track name of a physical track
#'
#' @return a list of logical track names which depend on the src track given
#'
#' @examples
#' get_dependent_ltracks("ph1")
#' @noRd
get_dependent_ltracks <- function(src) {
    if (!emr_track.exists(src)) {
        stop("Source track does not exist or is not a physical track")
    }
    .emr_checkroot()
    .emr_call("emr_ltrack_dependencies", src, new.env(parent = parent.frame()))
}
