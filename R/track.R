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
    track_path <- emr_track.info(track)$path # track info holds the path
    return(dirname(track_path))
}

.emr_track.filename <- function(track) {
    paste0(.emr_track.dir(track), "/", track, ".nrtrack")
}

.emr_dir.mv <- function(src, tgt) {
    dir.create(tgt, mode = "0777")
    file.copy(paste0(src, "/."), tgt, recursive = TRUE)
    unlink(src, recursive = TRUE)
}

._emr_backward_comp_space <- function(space) {
    if (is.null(space)) {
        return(NULL)
    }

    # if space is a db path as it should be
    # we do not want to lower case it, so
    # using a temp param
    lspace <- tolower(space)

    if (lspace == "user") {
        if ((!exists("EMR_UROOT", envir = .GlobalEnv) || is.null(get("EMR_UROOT", envir = .GlobalEnv)))) {
            stop("User space root directory is not set. Please call emr_db.connect", call. = FALSE)
        }
        db_id <- EMR_UROOT
    } else if (lspace == "global") {
        db_id <- EMR_GROOT
    } else if (!is.null(space)) {
        db_id <- normalizePath(space)
    } else {
        db_id <- space
    }
    return(db_id)
}



#' Adds new records to a track
#'
#' Adds new records to a track from a TAB-delimited file or a data frame.
#'
#' This function adds new records to a track. The records are contained either
#' in a file or a data frame.
#'
#' If 'src' is a file name, the latter must be constituted of four columns
#' separated by spaces or 'TAB' characters: ID, time, reference and value. The
#' file might contain lines of comments which should start with a '#'
#' character. Note that the file should not contain a header line.
#'
#' Alternatively 'src' can be a data frame consisting of the columns named
#' "id", "time", "ref" and "value". Note: "ref" column in the data frame is
#' optional.
#'
#' Adding to a logical track adds the values to the underlying physical
#' track, and is allowed only if all the values are within the logical
#' track allowed values and only from a data frame \code{src}. Note that
#' this might affect other logical tracks pointing to the same physical
#' track and therefore requires confirmation from the user unless
#' \code{force=TRUE}.
#'
#'
#' @param track track name
#' @param src file name or data-frame containing the track records
#' @param force if 'TRUE', suppresses user confirmation for addition to
#' logical tracks
#' @return None.
#' @seealso \code{\link{emr_track.import}}, \code{\link{emr_track.create}},
#' \code{\link{emr_db.init}}, \code{\link{emr_track.ls}}
#' @keywords ~import
#' @export emr_track.addto
emr_track.addto <- function(track, src, force = FALSE) {
    if (missing(track) || missing(src)) {
        stop("Usage: emr_track.addto(track, src)", call. = FALSE)
    }
    .emr_checkroot()

    if (emr_track.readonly(track)) {
        stop(sprintf("Cannot add data to track %s: it is read-only.\n", track), call. = FALSE)
    }

    if (emr_track.logical.exists(track)) {
        if (is.character(src)) {
            stop("Cannot add to a logical track when src is a file name. Please load the file to a data frame and rerun emr_track.addto with src as the data frame.")
        }

        if (!is.data.frame(src) || !all(c("id", "time", "value") %in% colnames(src))) {
            stop("Invalid format of src. Please provide a data frame with 'id','time','ref' and 'value' columns.")
        }

        ltrack <- emr_track.logical.info(track)

        if (emr_track.readonly(ltrack$source)) {
            stop(sprintf("Cannot add data to track %s: it's source track (\"%s\") is read-only.\n", ltrack$source, track), call. = FALSE)
        }

        if (!all(src$value %in% ltrack$value)) {
            stop(sprintf("src contains values which are not part of the logical track. You can add them directly to the physical track (\"%s\")", ltrack$source))
        }

        answer <- "N"
        if (force) {
            answer <- "Y"
        } else {
            str <- sprintf("Adding to the logical track %s would update the physical track %s and might affect other logical tracks. Are you sure (Y/N)? ", track, ltrack$source)
            cat(str)
            answer <- toupper(readLines(n = 1))
        }

        if (answer == "Y" || answer == "YES") {
            track <- ltrack$source
        } else {
            return(NULL)
        }
    } else {
        dependent_ltracks <- get_dependent_ltracks(track)
        answer <- "N"
        if (force || length(dependent_ltracks) == 0) {
            answer <- "Y"
        } else {
            str <- sprintf(
                "We found other tracks which depend on the track you are about to update.\nupdating the track will update the following tracks as well.\n%s\nAre you sure you want to update track %s (Y/N)? ",
                paste0(dependent_ltracks, sep = "", collapse = ", "), track
            )
            cat(str)
            answer <- toupper(readLines(n = 1))
        }
        if (!(answer == "Y" || answer == "YES")) {
            return(NULL)
        }
    }

    .emr_call("emr_import", track, NULL, NULL, src, TRUE, new.env(parent = parent.frame()))
}

#' Creates a track from a track expression
#'
#' Creates a track from a track expression.
#'
#' This function creates a new track based on the values from
#' the track expression. The location of the track is controlled via 'space'
#' parameter which can be any of the db_dirs supplied in emr_db.connect
#'
#' @inheritSection emr_extract iterator
#'
#' @param track the name of the newly created track
#' @param space db path, one of the paths supplied in emr_db.connect
#' @param categorical if 'TRUE' track is marked as categorical
#' @param expr track expression
#' @param stime start time scope
#' @param etime end time scope
#' @param iterator track expression iterator. If 'NULL' iterator is determined
#' implicitly based on track expressions. See also 'iterator' section.
#' @param keepref If 'TRUE' references are preserved in the iterator
#' @param filter Iterator filter
#' @param override Boolean indicating whether the creation intends to override an existing track (default FALSE)
#' @return None.
#' @seealso \code{\link{emr_track.import}}, \code{\link{emr_track.addto}},
#' \code{\link{emr_track.rm}}, \code{\link{emr_track.readonly}},
#' \code{\link{emr_track.ls}}, \code{\link{emr_track.exists}}
#' @keywords ~track ~create
#' @export emr_track.create
emr_track.create <- function(track, space = EMR_UROOT, categorical, expr, stime = NULL, etime = NULL, iterator = NULL, keepref = FALSE, filter = NULL, override = FALSE) {

    # when space is missing, writing for the last db in the order of connections
    if (missing(space)) {
        if ((!exists("EMR_UROOT", envir = .GlobalEnv) || is.null(get("EMR_UROOT", envir = .GlobalEnv)))) {
            space <- EMR_GROOT
        } else {
            space <- EMR_UROOT
        }
    }

    if (missing(track) || missing(categorical) || missing(expr)) {
        stop("Usage: emr_track.create(track, space = EMR_GROOT, categorical, expr, stime = NULL, etime = NULL, iterator = NULL, keepref = FALSE, filter = NULL)", call. = FALSE)
    }
    .emr_checkroot()

    if (emr_vtrack.exists(track)) {
        stop(sprintf("Virtual track %s already exists", track), call. = FALSE)
    }

    if (emr_filter.exists(track)) {
        stop(sprintf("Filter %s already exists", track), call. = FALSE)
    }

    db_id <- ._emr_backward_comp_space(space)

    orig_filters <- .emr_gen_vtrack_filters(filter, iterator, keepref, stime, etime)
    on.exit(.emr_recreate_vtrack_filters(orig_filters), add = TRUE)

    .emr_call("emr_track_create", track, db_id, categorical, expr, stime, etime, iterator, keepref, .emr_filter(filter), override, new.env(parent = parent.frame()))
}



#' Checks whether the track exists
#'
#' Checks whether the track exists.
#'
#' This function checks whether the track exists.
#' If \code{db_id} is passed, the function checks
#' whether the track exists in the specific db.
#'
#' @param track track name
#' @param db_id string of a db dir passed to \code{emr_db.connect}
#'
#' @return 'TRUE' if the tracks exists, otherwise 'FALSE'
#' @seealso \code{\link{emr_track.ls}}, \code{\link{emr_track.info}}
#' @keywords ~track ~exists
#' @examples
#'
#' emr_db.init_examples()
#' emr_track.exists("sparse_track")
#' @export emr_track.exists
emr_track.exists <- function(track, db_id = NULL) {
    if (missing(track)) {
        stop("Usage: emr_track.exist(track)", call. = FALSE)
    }
    .emr_checkroot()

    if (length(track) == 1) {
        track_exists <- single_track_exists(track, db_id)
    } else {
        track_exists <- multiple_tracks_exist(track, db_id)
    }

    return(track_exists)
}

single_track_exists <- function(track, db_id = NULL) {
    if (is.null(db_id)) {
        track_exists <- FALSE
        for (root in EMR_ROOTS) {
            track_exists <- track_exists | .emr_call("emr_track_exists", track, root, new.env(parent = parent.frame()))
        }
        track_exists <- track_exists | .emr_call("emr_logical_track_exists", track, new.env(parent = parent.frame()))
    } else {
        track_exists <- .emr_call("emr_track_exists", track, db_id, new.env(parent = parent.frame()))
    }
    return(track_exists)
}

multiple_tracks_exist <- function(tracks, db_id = NULL) {
    if (is.null(db_id)) {
        track_exists <- !is.na(match(tracks, .emr_call("emr_track_names", new.env(parent = parent.frame()), silent = TRUE)))
        track_exists <- track_exists | !is.na(match(tracks, .emr_call("emr_logical_track_names", new.env(parent = parent.frame()), silent = TRUE)))
    } else {
        track_exists <- !is.na(match(tracks, .emr_call("emr_track_db_names", db_id, new.env(parent = parent.frame()), silent = TRUE)))
    }
    return(track_exists)
}



#' Returns track ids
#'
#' Returns the ids contained by the track.
#'
#' Returns the ids contained by the track.
#'
#' Note: this function ignores the current subset, i.e. ids of the whole track
#' are returned.
#'
#' @param track track name
#' @return An Ids Table
#' @seealso \code{\link{emr_track.unique}}, \code{\link{emr_track.info}}
#' @keywords ~track ~ids
#' @examples
#'
#' emr_db.init_examples()
#' emr_track.ids("categorical_track")
#' @export emr_track.ids
emr_track.ids <- function(track) {
    if (missing(track)) {
        stop("Usage: emr_track.ids(track)", call. = FALSE)
    }
    .emr_checkroot()

    .emr_call("emr_track_ids", track, new.env(parent = parent.frame()))
}



#' Imports a track from a file or data-frame
#'
#' Imports a track from a file or data-frame.
#'
#' This function creates a new track from a text file or a data-frame.
#' The location of the track is controlled via 'space' parameter which
#' can be any of the db_dirs supplied in emr_db.connect.
#'
#' If 'src' is a file name, the latter must be constituted of four columns
#' separated by spaces or 'TAB' characters: ID, time, reference and value. The
#' file might contain lines of comments which should start with a '#'
#' character.
#'
#' Alternatively 'src' can be an ID-Time Values table, which is a data frame with
#' the following columns: "id" "time" "ref" and "value". Note that the
#' file should not contain a header.
#'
#' (see "User Manual" for more info).
#'
#' @param track the name of the newly created track
#' @param space db dir string (path), one of the paths supplied in emr_db.connect
#' @param categorical if 'TRUE' track is marked as categorical
#' @param src file name or data-frame containing the track records
#' @param override Boolean indicating whether the creation intends to override an existing track (default FALSE)
#' @return None.
#' @seealso \code{\link{emr_track.addto}}, \code{\link{emr_track.create}},
#' \code{\link{emr_track.readonly}}, \code{\link{emr_db.init}},
#' \code{\link{emr_track.ls}}
#' @keywords ~import
#' @export emr_track.import
emr_track.import <- function(track, space, categorical, src, override = FALSE) {
    # when space is missing, writing for the last db in the order of connections
    if (missing(space)) {
        if ((!exists("EMR_UROOT", envir = .GlobalEnv) || is.null(get("EMR_UROOT", envir = .GlobalEnv)))) {
            space <- EMR_GROOT
        } else {
            space <- EMR_UROOT
        }
    }
    if (missing(track) || missing(src) || missing(categorical)) {
        stop("Usage: emr_track.import(track, space, categorical, src)", call. = FALSE)
    }
    .emr_checkroot()

    if (emr_vtrack.exists(track)) {
        stop(sprintf("Virtual track %s already exists", track), call. = FALSE)
    }

    if (emr_filter.exists(track)) {
        stop(sprintf("Filter %s already exists", track), call. = FALSE)
    }

    db_id <- ._emr_backward_comp_space(space)
    .emr_call("emr_import", track, db_id, categorical, src, FALSE, override, new.env(parent = parent.frame()))
}


#' Returns information about the track.
#'
#' This function returns information about the track: type, data type, number
#' of vales, number of unique values, minimal / maximal value, minimal /
#' maximal id, minimal / maximal time.
#'
#' Note: this function ignores the current subset, i.e. it is applied to the
#' whole track.
#'
#' @param track track name
#' @return A list that contains track properties
#' @seealso \code{\link{emr_track.ls}}
#' @keywords ~track ~info ~property
#' @examples
#'
#' emr_db.init_examples()
#' emr_track.info("sparse_track")
#' @export emr_track.info
emr_track.info <- function(track) {
    if (missing(track)) {
        stop("Usage: emr_track.info(track)", call. = FALSE)
    }
    .emr_checkroot()

    if (is.character(track) && emr_track.logical.exists(track)) {
        ltrack <- emr_track.logical.info(track)
        .emr_call("emr_logical_track_user_info", track, ltrack$source, NULL, NULL, ltrack$source, TRUE, .emr_filter(create_logical_track_filter(track)), c(EMR_ROOTS), new.env(parent = parent.frame()))
    } else {
        .emr_call("emr_track_info", track, new.env(parent = parent.frame()))
    }
}

.emr_track_dbs <- function(track, dataframe, func, c_func) {
    .emr_checkroot()
    if (length(track) > 1) {
        if (!dataframe) {
            return(purrr::map(track, func, dataframe = FALSE) %>% do.call(c, .))
        } else {
            return(purrr::map_dfr(track, func, dataframe = TRUE))
        }
    }

    if (is.character(track) && emr_track.logical.exists(track)) {
        dbs <- EMR_GROOT
    } else {
        dbs <- .emr_call(c_func, track, new.env(parent = parent.frame()))
    }

    if (!dataframe) {
        names(dbs) <- rep(track, length(dbs))
        return(dbs)
    } else {
        return(data.frame(track = track, db = dbs))
    }
}

#' Returns a vector of db ids which have a
#' version of the track
#'
#' @param track one or more track names
#' @param dataframe return a data frame with with columns
#' called 'track' and 'db' instead of a vector of database ids.
#' @return A named vector of db ids for each track. If \code{dataframe} is TRUE - returns a data frame with columns
#' called 'track' and 'db' with the track and database ids (multiple rows per track in the case of
#' \code{emr_track.dbs}).
#' @seealso \code{\link{emr_track.info}}
#' @keywords ~track ~info ~property ~db ~db_id ~connect
#'
#' @description
#' \code{emr_track.dbs} returns all the databases which have a version of the track,
#' while \code{emr_track.current_db} returns the database from which `naryn` currently takes
#' the track according to the override rules.
#'
#' @examples
#'
#' # both db1 and db2 have a track named track1
#' \dontrun{
#' emr_db.connect(c("/db1", "/db2"))
#' emr_track.dbs("track1")
#' emr_track.dbs(emr_track.ls())
#'
#' emr_track.current_db("track1")
#' emr_track.current_db(emr_track.ls())
#' }
#' @export emr_track.dbs
emr_track.dbs <- function(track, dataframe = FALSE) {
    if (missing(track)) {
        stop("Usage: emr_track.dbs(track)", call. = FALSE)
    }
    return(.emr_track_dbs(track, dataframe, emr_track.dbs, "emr_track_dbs"))
}

#' Returns current database of a track
#'
#'
#' @rdname emr_track.dbs
#' @export
emr_track.current_db <- function(track, dataframe = FALSE) {
    if (missing(track)) {
        stop("Usage: emr_track.current_db(track)", call. = FALSE)
    }
    return(.emr_track_dbs(track, dataframe, emr_track.current_db, "emr_track_db"))
}

#' Returns a list of track names
#'
#' Returns a list of track names in the database.
#'
#' 'emr_track.ls' returns a list of all tracks (global and user) in the
#' database that match the pattern (see 'grep'). If called without any
#' arguments all tracks are returned.
#'
#' If pattern is specified without a track attribute (i.e. in the form of
#' 'pattern') then filtering is applied to the track names. If pattern is
#' supplied with a track attribute (i.e. in the form of 'name = pattern') then
#' track attribute is matched against the pattern.
#'
#' Multiple patterns are applied one after another. The resulted list of tracks
#' should match all the patterns.
#'
#' If \code{db_id} parameter is set, only tracks within the specific db would be shown.
#' Note that tracks which were overriden by other databases would not be shown, even if
#' their files exist within the database. See \code{emr_db.connect} for more details.
#'
#' 'emr_track.global.ls', 'emr_track.user.ls', 'emr_track.logical.ls' work similarly to
#' 'emr_track.ls' but instead of returning all track names, each of them
#' returns either global, local or logical tracks accordingly.
#'
#' @aliases emr_track.ls emr_track.global.ls emr_track.user.ls emr_track.logical.ls
#' @param ... these arguments are of either form 'pattern' or 'attribute =
#' pattern'
#' @param db_id db dir string (path), one of the paths supplied in emr_db.connect. If NULL - all track names would be
#' returned.
#' @param ignore.case,perl,fixed,useBytes see 'grep'
#' @return An array that contains the names of tracks that match the supplied
#' patterns.
#' @seealso \code{\link{grep}}, \code{\link{emr_db.init}},
#' \code{\link{emr_track.exists}}
#' @keywords ~track ~tracks ~ls
#' @examples
#'
#' emr_db.init_examples()
#'
#' # get all track names
#' emr_track.ls()
#'
#' # get track names that match the pattern "den*"
#' emr_track.ls("den*")
#'
#' emr_track.attr.set("sparse_track", "gender", "female")
#' emr_track.attr.set("dense_track", "gender", "male")
#' emr_track.ls(gender = "")
#' emr_track.ls(gender = "female")
#' emr_track.ls(gender = "^male")
#' @export emr_track.ls
emr_track.ls <- function(..., db_id = NULL, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
    .emr_checkroot()
    if (!is.null(db_id)) {
        if (db_id == EMR_GROOT) {
            return(emr_track.global.ls(..., ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes))
        } else {
            return(emr_track.db.ls(..., db_id = db_id, ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes))
        }
    }
    tracks <- .emr_call("emr_track_names", new.env(parent = parent.frame()), silent = TRUE)
    logical_tracks <- .emr_call("emr_logical_track_names", new.env(parent = parent.frame()), silent = TRUE)
    .emr_tracks_filter(..., tracks = sort(c(tracks, logical_tracks)), ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes)
}

#' emr_track.ls for global db
#'
#' @export
#' @rdname emr_track.ls
emr_track.global.ls <- function(..., ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
    .emr_checkroot()
    tracks <- .emr_call("emr_track_db_names", EMR_GROOT, new.env(parent = parent.frame()), silent = TRUE)
    logical_tracks <- .emr_call("emr_logical_track_names", new.env(parent = parent.frame()), silent = TRUE)
    .emr_tracks_filter(..., tracks = sort(c(tracks, logical_tracks)), ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes)
}

#' emr_track.ls for user db
#'
#' @export
#' @rdname emr_track.ls
emr_track.user.ls <- function(..., ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
    .emr_checkroot()
    tracks <- .emr_call("emr_track_db_names", EMR_UROOT, new.env(parent = parent.frame()), silent = TRUE)
    .emr_tracks_filter(..., tracks = tracks, ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes)
}

#' emr_track.ls for specific db
#'
#' @noRd
emr_track.db.ls <- function(..., db_id, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
    .emr_checkroot()
    tracks <- .emr_call("emr_track_db_names", db_id, new.env(parent = parent.frame()), silent = TRUE)
    .emr_tracks_filter(..., tracks = tracks, ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes)
}

#' emr_track.ls for logical tracks
#'
#' @export
#' @rdname emr_track.ls
emr_track.logical.ls <- function(..., ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
    .emr_checkroot()
    tracks <- .emr_call("emr_logical_track_names", new.env(parent = parent.frame()), silent = TRUE)
    tracks <- .emr_tracks_filter(..., tracks = tracks, ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes)
    return(sort(tracks))
}




#' Moves (renames) a track
#'
#' Moves (renames) a track
#'
#' This function moves (renames) 'src' track into 'tgt'. If 'space' equals
#' 'NULL', the track remains in the same space. Otherwise it is moved
#' to the specified space.
#'
#' Note that logical tracks cannot be moved to the user space.
#'
#' @param src source track name
#' @param tgt target track name
#' @param space db path (string), one of the paths supplied in emr_db.connect or NULL
#' @return None.
#' @seealso \code{\link{emr_track.create}}, \code{\link{emr_track.rm}},
#' \code{\link{emr_track.ls}}
#' @keywords ~track
#' @export emr_track.mv
emr_track.mv <- function(src, tgt, space = NULL) {
    if (missing(src) || missing(tgt)) {
        stop("Usage: emr_track.mv(src, tgt, space = NULL)", call. = FALSE)
    }
    .emr_checkroot()

    if (!is.null(space)) {
        space <- tolower(space)

        if (emr_track.logical.exists(src) && space != EMR_GROOT) {
            stop("cannot move logical tracks out of global space")
        }
    }

    if (emr_track.readonly(src)) {
        stop(sprintf("Cannot move track %s: it is read-only.\n", src), call. = FALSE)
    }

    if (emr_vtrack.exists(tgt)) {
        stop(sprintf("Virtual track %s already exists", tgt), call. = FALSE)
    }

    if (emr_filter.exists(tgt)) {
        stop(sprintf("Filter %s already exists", tgt), call. = FALSE)
    }

    if (emr_track.logical.exists(src)) {
        ltrack <- emr_track.logical.info(src)
        emr_track.logical.rm(src, force = TRUE)
        emr_track.logical.create(tgt, ltrack$source, ltrack$values)
        dirname1 <- .emr_track.logical.var.dir(src)
        dirname2 <- .emr_track.logical.pyvar.dir(src)
    } else {
        # when moving a physical track we need
        # to move all the ltracks which depend
        # on it
        db_id <- ._emr_backward_comp_space(space)

        dependent_ltracks <- get_dependent_ltracks(src)
        dirname1 <- .emr_track.var.dir(src)
        dirname2 <- .emr_track.pyvar.dir(src)

        .emr_call("emr_track_mv", src, tgt, db_id, new.env(parent = parent.frame()))

        for (ltrack in dependent_ltracks) {
            ltrack_info <- emr_track.logical.info(ltrack)
            emr_track.logical.rm(ltrack, force = TRUE, rm_vars = FALSE)
            emr_track.logical.create(ltrack, tgt, ltrack_info$values)
        }
    }

    if (file.exists(dirname1)) {
        .emr_dir.mv(dirname1, .emr_track.var.dir(tgt))
    }

    if (file.exists(dirname2)) {
        .emr_dir.mv(dirname2, .emr_track.pyvar.dir(tgt))
    }
}



#' Returns track percentile of the values
#'
#' Returns track percentile of the values.
#'
#' This function returns the percentiles of the values given in 'val' based on
#' track data.
#'
#' If 'lower' is 'TRUE' percentile indicates the relative number of track
#' values lower than 'val'. If 'lower' is 'FALSE' percentile reflects the
#' relative number of track values lower or equal than 'val'.
#'
#' @param track track name
#' @param val vector of values
#' @param lower how to calculate percentiles
#' @return A vector of percentile values
#' @seealso \code{\link{emr_track.unique}}
#' @keywords ~track ~percentile
#' @examples
#'
#' emr_db.init_examples()
#'
#' # percentiles of 30, 50
#' emr_track.percentile("dense_track", c(30, 50))
#'
#' # calculate percentiles of track's earliest values in time window
#' emr_vtrack.create("v1",
#'     src = "dense_track", func = "earliest",
#'     time.shift = c(-5, 5)
#' )
#' emr_extract(c(
#'     "dense_track",
#'     "emr_track.percentile(\"dense_track\", v1, FALSE)"
#' ),
#' keepref = TRUE, names = c("col1", "col2")
#' )
#' @export emr_track.percentile
emr_track.percentile <- function(track, val, lower = TRUE) {
    if (missing(track) || missing(val)) {
        stop("Usage: emr_track.percentile(track, val, lower)", call. = FALSE)
    }
    .emr_checkroot()

    if (emr_track.logical.exists(track)) {
        ltrack <- emr_track.logical.info(track)
        if (!is.null(ltrack$values) || emr_track.info(ltrack$source)$categorical) {
            stop(sprintf("Track %s is categorical: percentile queries are not supported", track))
        }
        track <- ltrack$source
    }

    .emr_call("emr_track_percentile", track, val, lower, new.env(parent = parent.frame()))
}



#' Gets or sets "read-only" property of a track
#'
#' Gets or sets "readonly" property of a track.
#'
#' This function gets or sets "read-onlyness" of the track. If 'readonly' is
#' 'NULL' the functions returns whether the track is R/O. Otherwise it sets
#' "read-onlyness" to the value indicated by 'readonly'.
#'
#' Logical tracks inherit their "read-onlyness" from the source
#' physical tracks.
#'
#' Overriding a track also overrides it's "read-onlyness", it's
#' "read-onlyness" will persist when the track is no longer overridden
#'
#' @param track track name
#' @param readonly if 'NULL', return "readonlyness" of the track, otherwise
#' sets it
#' @return None.
#' @seealso \code{\link{emr_track.create}}, \code{\link{emr_track.mv}},
#' \code{\link{emr_track.ls}}, \code{\link{emr_track.rm}}
#' @keywords ~track
#' @export emr_track.readonly
emr_track.readonly <- function(track, readonly = NULL) {
    if (missing(track)) {
        stop("Usage: emr_track.readonly(track, readonly = NULL)", call. = FALSE)
    }
    .emr_checkroot()

    if (!emr_track.exists(track)) {
        stop(sprintf("Track %s does not exist", track), call. = FALSE)
    }

    orig_track <- track

    if (emr_track.logical.exists(track)) {
        file <- .emr_track.logical.filename(track)
    } else {
        file <- .emr_track.filename(track)
    }

    if (file.access(file, 0) == -1) {
        stop(sprintf("File %s does not exist", file), call. = FALSE)
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

    if (Sys.chmod(file, mode, use_umask = FALSE) == FALSE) {
        stop(sprintf("Failed to set read-only attribute for track %s", orig_track), call. = FALSE)
    }
}


#' Deletes a track
#'
#' Deletes a track.
#'
#' This function deletes a user track from the database. By default
#' 'emr_track.rm' requires the user to interactively confirm the deletion. Set
#' 'force' to 'TRUE' to suppress the user prompt.
#'
#' @param track track name
#' @param force if 'TRUE', suppresses user confirmation of a named track removal
#' @return None.
#' @seealso \code{\link{emr_track.create}}, \code{\link{emr_track.mv}},
#' \code{\link{emr_track.ls}}, \code{\link{emr_track.readonly}}
#' @keywords ~track
#' @export emr_track.rm
emr_track.rm <- function(track, force = FALSE) {
    if (missing(track)) {
        stop("Usage: emr_track.rm(track, force = FALSE)", call. = FALSE)
    }
    .emr_checkroot()
    if (!emr_track.exists(track)) {
        if (force) {
            return(invisible())
        }
        stop(sprintf("Track %s does not exist", track), call. = FALSE)
    }

    readonly <- FALSE
    if (force) {
        tryCatch({
            readonly <- emr_track.readonly(track)
        })
    } else {
        readonly <- emr_track.readonly(track)
    }

    if (readonly) {
        stop(sprintf("Cannot remove track %s: it is read-only.\n", track), call. = FALSE)
    }

    if (emr_track.logical.exists(track)) {
        return(emr_track.logical.rm(track, force = force))
    }

    answer <- "N"
    dependent_ltracks <- get_dependent_ltracks(track)

    if (force) {
        answer <- "Y"
    } else {
        if (length(dependent_ltracks) == 0) {
            str <- sprintf("Are you sure you want to delete track %s (Y/N)? ", track)
        } else {
            str <- sprintf(
                "We found other tracks which depend on the track you are about to remove.\nremoving the track will remove the following tracks as well.\n%s\nAre you sure you want to delete track %s (Y/N)? ",
                paste0(dependent_ltracks, sep = "", collapse = ", "), track
            )
        }
        cat(str)
        answer <- toupper(readLines(n = 1))
    }

    if (answer == "Y" || answer == "YES") {
        attr_fname <- .emr_track.attrs.fname(track)
        if (file.exists(attr_fname)) {
            unlink(attr_fname)
        }


        dirname1 <- .emr_track.var.dir(track)
        dirname2 <- .emr_track.pyvar.dir(track)

        for (ltrack in dependent_ltracks) {
            emr_track.logical.rm(ltrack, force = TRUE)
        }

        .emr_call("emr_track_rm", track, new.env(parent = parent.frame()))

        if (file.exists(dirname1)) {
            unlink(dirname1, recursive = TRUE)
        }

        if (file.exists(dirname2)) {
            unlink(dirname2, recursive = TRUE)
        }
    }
}



#' Returns track values
#'
#' Returns unique and sorted track values
#'
#' Returns unique and sorted track values. NaN values (if exist in the track)
#' are not returned.
#'
#' Note: this function ignores the current subset, i.e. the unique values of
#' the whole track are returned.
#'
#' @param track track name
#' @return A vector of values
#' @seealso \code{\link{emr_track.ids}}, \code{\link{emr_track.info}}
#' @keywords ~track ~unique
#' @examples
#'
#' emr_db.init_examples()
#' emr_track.unique("categorical_track")
#' @export emr_track.unique
emr_track.unique <- function(track) {
    if (missing(track)) {
        stop("Usage: emr_track.unique(track)", call. = FALSE)
    }
    .emr_checkroot()

    if (emr_track.logical.exists(track)) {
        ltrack <- emr_track.logical.info(track)
        res <- .emr_call("emr_track_unique", ltrack$source, new.env(parent = parent.frame()))
        if (!is.null(ltrack$values)) {
            res <- res[res %in% ltrack$values]
        }
    } else {
        res <- .emr_call("emr_track_unique", track, new.env(parent = parent.frame()))
    }

    return(res)
}
