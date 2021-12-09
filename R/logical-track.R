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

.emr_track.logical.attrs.fname <- function(track) {
    paste0(.emr_track.logical.dir(), "/.", track, ".attrs")
}

#' Creates a logical track
#'
#' Creates a logical track
#'
#' This function creates a logical track based on an existing categorical track
#' in the global space.
#'
#' Note: Both the logical track and source should be on the global db. If the logical track
#' would be created and afterwards the db would be loaded as non-global db the logical tracks
#' would **not** be visible.
#'
#' @param track one or more names of the newly created logical tracks.
#' @param src name of the physical tracks for each logical \code{track}
#' @param values vector of selected values. When creating multiple logical tracks at once
#' - \code{values} should be a list of vectors (with one vector of values for each logical track).
#' @return None.
#'
#' @examples
#' \dontrun{
#' emr_track.logical.create("logical_track", "categorical_track", values = c(2, 3))
#'
#' # multiple tracks
#' emr_track.logical.create(
#'     c("logical_track1", "logical_track2", "logical_track3"),
#'     rep("categorical_track", 3),
#'     values = list(c(2, 3), NULL, c(1, 4))
#' )
#' }
#'
#' @keywords ~track ~create_logical
#' @export emr_track.logical.create
emr_track.logical.create <- function(track, src, values = NULL) {
    .emr_checkroot()
    dups <- duplicated(track)
    if (any(dups)) {
        stop("The following tracks appear more than once: ", paste(unique(track[dups]), collapse = ", "))
    }

    if (length(track) != length(src)) {
        stop("Number of tracks is not equal to the number of sources", call. = FALSE)
    }

    if (length(track) > 1) {
        stopifnot(is.list(values))
        if (length(track) != length(values)) {
            stop("Number of tracks is not equal to the number of entries in the values list", call. = FALSE)
        }
        purrr::pwalk(list(track, src, values), function(tr, sr, v) {
            .emr_call("emr_create_logical", tr, sr, v, FALSE, new.env(parent = parent.frame()), silent = TRUE)
        })
        .emr_call("update_logical_tracks_file", new.env(parent = parent.frame()), silent = TRUE)
    } else {
        if (is.list(values)) {
            if (length(values) != 1) {
                stop("Number of tracks is not equal to the number of entries in the values list")
            }
            values <- unlist(values)
        }
        .emr_call("emr_create_logical", track, src, values, TRUE, new.env(parent = parent.frame()), silent = TRUE)
    }
}

remove_logical_track <- function(track, force, rm_vars, update) {
    if (!emr_track.exists(track)) {
        if (force) {
            return(invisible())
        }
        stop(sprintf("Track %s does not exist", track), call. = FALSE)
    }

    if (!emr_track.logical.exists(track)) {
        stop(sprintf("Track %s is not a logical track", track), call. = FALSE)
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
        attr_fname <- .emr_track.logical.attrs.fname(track)
        if (file.exists(attr_fname)) {
            unlink(attr_fname)
        }

        dirname1 <- .emr_track.logical.var.dir(track)
        dirname2 <- .emr_track.logical.pyvar.dir(track)
        .emr_call("emr_remove_logical", track, update, new.env(parent = parent.frame()), silent = TRUE)

        if (rm_vars && file.exists(dirname1)) {
            unlink(dirname1, recursive = TRUE)
        }

        if (rm_vars && file.exists(dirname2)) {
            unlink(dirname2, recursive = TRUE)
        }
    }
}

#' Deletes a logical track
#'
#' @param track the name of one or more tracks to delete
#' @param force if 'TRUE', suppresses user confirmation of a named track removal
#' @param rm_vars remove track variables
#' @return None.
#'
#' @keywords ~track ~create_logical
#' @export
emr_track.logical.rm <- function(track, force = FALSE, rm_vars = TRUE) {
    .emr_checkroot()
    if (length(track) > 1) {
        purrr::walk(track, remove_logical_track, force = force, rm_vars = rm_vars, update = FALSE)
        .emr_call("update_logical_tracks_file", new.env(parent = parent.frame()), silent = TRUE)
    } else {
        remove_logical_track(track, force = force, rm_vars = rm_vars, update = TRUE)
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
        stop("Usage: emr_track.logical.info(track)", call. = FALSE)
    }
    .emr_checkroot()

    .emr_call("emr_logical_track_info", track, new.env(parent = parent.frame()))
}

random_filter_name <- function(pattern) {
    basename(tempfile(pattern = pattern))
}

#' Create a filter for logical track
#'
#' @param ltrack name of logical track
#' @param filter existing filter (the new filter would be added)
#' @param filter_name name for the new filter (optional)
#'
#' @return a string with the logical track filter (i.e. filter for the values of the original track), added (with '&' operator) to the original filter (if exists).
#'
#'
#' @examples
#'
#' create_logical_track_filter("logical_track")
#' @noRd
create_logical_track_filter <- function(ltrack, filter = NULL, filter_name = NULL, env = parent.frame()) {
    ltrack <- emr_track.logical.info(ltrack)
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


#' Get a list of logical track names which depend on the given src track
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
