.emr_track.attrs.fname <- function(track) {
    paste0(.emr_track.dir(track), "/.", track, ".attrs")
}

#' Returns attributes values of tracks
#'
#' Returns attributes values of tracks.
#'
#' This function returns a data frame that contains attributes values of one or
#' more tracks. The data frame is constituted of 3 columns named 'track',
#' 'attr' and 'value'.
#'
#' 'track' parameter is optionally used to retrieve only the attributes of the
#' specific track(s). If 'NULL', attributes of all the tracks are returned.
#'
#' Likewise 'attr' allows to retrieve only specifically named attributes.
#'
#' If both 'track' and 'attr' are used, the attributes that fulfill both of the
#' conditions are returned
#'
#' Overriding a track also overrides it's track attributes, the
#' attributes will persist when the track is no longer overridden.
#'
#' @param track a vector of track names or 'NULL'
#' @param attr a vector of attribute names or 'NULL'
#' @param include_missing when TRUE - adds a row for tracks which do not have the 'attr' with NA,
#' or tracks which do not exist. Otherwise tracks without an attribute would be omitted from the data
#' frame, and an error would be thrown for tracks which do not exist.
#' @return A data frame containing attributes values of tracks.
#' @seealso \code{\link{emr_track.attr.get}}, \code{\link{emr_track.attr.set}}
#' @keywords ~attr ~attribute
#' @examples
#'
#' emr_db.init_examples()
#' emr_track.attr.export()
#' emr_track.attr.set("sparse_track", "gender", "female")
#' emr_track.attr.set("sparse_track", "tag", "")
#' emr_track.attr.set("dense_track", "gender", "male")
#' emr_track.attr.export()
#' emr_track.attr.export(track = "sparse_track")
#' emr_track.attr.export(attr = "gender")
#' emr_track.attr.export(track = "sparse_track", attr = "gender")
#' @export emr_track.attr.export
emr_track.attr.export <- function(track = NULL, attr = NULL, include_missing = FALSE) {
    .emr_checkroot()

    if (is.null(track)) {
        track <- emr_track.ls()
    } else {
        track <- unique(track)
    }
    if (!is.null(attr)) {
        attr <- unique(attr)
    }

    if (include_missing) {
        tracks_to_compute <- track[emr_track.exists(track)]
    } else {
        tracks_to_compute <- track
    }

    if (length(tracks_to_compute) == 0) {
        res <- data.frame(track = character(), attr = character(), value = character())
    } else {
        res <- .emr_call("emr_get_tracks_attrs", tracks_to_compute, attr, new.env(parent = parent.frame()))
    }

    if (include_missing) {
        attr <- attr %||% unique(res$attr)
        res <- res %>%
            dplyr::mutate(
                track = factor(track, levels = !!track),
                attr = factor(attr, levels = !!attr)
            )

        res <- res %>%
            tidyr::complete(track, attr, fill = list(value = NA)) %>%
            dplyr::mutate(track = as.character(track), attr = as.character(attr), value = as.character(value))

        res <- as.data.frame(res)
    }

    return(res)
}



#' Returns the value of the track attribute
#'
#' Returns the value of the track attribute.
#'
#' This function returns the value of a track attribute or 'NULL' if the
#' attribute does not exist.
#'
#' @param track track name
#' @param attr attribute name
#' @return Track attribute value or 'NULL'.
#' @seealso \code{\link{emr_track.attr.export}},
#' \code{\link{emr_track.attr.set}}
#' @keywords ~attr ~attribute
#' @examples
#'
#' emr_db.init_examples()
#' emr_track.attr.set("sparse_track", "test_attr", "value")
#' emr_track.attr.get("sparse_track", "test_attr")
#' @export emr_track.attr.get
emr_track.attr.get <- function(track = NULL, attr = NULL) {
    if (missing(track) || missing(attr)) {
        stop("Usage: emr_track.attr.get(track, attr)", call. = FALSE)
    }

    if (length(track) > 1) {
        stop("'emr_track.attr.get' can be used only with a single track. For multiple tracks please use 'emr_track.attr.export'", call. = FALSE)
    }
    .emr_checkroot()

    res <- emr_track.attr.export(track, attr)
    if (nrow(res)) {
        res$value[1]
    } else {
        NULL
    }
}



#' Deletes a track attribute
#'
#' Deletes a track attribute.
#'
#' This function deletes a track attribute.
#'
#' @param track one or more track names
#' @param attr attribute name
#' @return None.
#' @seealso \code{\link{emr_track.attr.set}}, \code{\link{emr_track.attr.get}},
#' \code{\link{emr_track.attr.export}}
#' @keywords ~attr ~attribute
#' @examples
#'
#' emr_db.init_examples()
#' emr_track.attr.set("sparse_track", "test_attr", "value")
#' emr_track.attr.export()
#' emr_track.attr.rm("sparse_track", "test_attr")
#' emr_track.attr.export()
#' @export emr_track.attr.rm
emr_track.attr.rm <- function(track, attr) {
    if (missing(track) || missing(attr)) {
        stop("Usage: emr_track.attr.rm(track, attr)", call. = FALSE)
    }
    .emr_checkroot()

    dups <- duplicated(track)
    if (any(dups)) {
        stop("The following tracks appear more than once: ", paste(unique(track[dups]), collapse = ", "))
    }

    if (length(track) > 1) {
        purrr::walk(track, function(tr) {
            .emr_call("emr_set_track_attr", tr, attr, NULL, FALSE, new.env(parent = parent.frame()))
        })
        dbs <- emr_track.dbs(track, dataframe = FALSE)
        purrr::walk(dbs, ~ {
            .emr_call("update_tracks_attrs_file", .x, new.env(parent = parent.frame()))
        })
    } else {
        .emr_call("emr_set_track_attr", track, attr, NULL, TRUE, new.env(parent = parent.frame()))
    }

    retv <- 0 # suppress return value
}



#' Assigns a value to the track attribute
#'
#' Assigns a value to the track attribute.
#'
#' This function creates a track attribute and assigns 'value' to it. If the
#' attribute already exists its value is overwritten.
#'
#' Note that both attributes and values sould be in ASCII encoding.
#'
#' @param track one or more track names
#' @param attr one or more attribute names
#' @param value on or more values (strings). Can be an empty string ('').
#' @return None.
#' @seealso \code{\link{emr_track.attr.get}}, \code{\link{emr_track.attr.rm}},
#' \code{\link{emr_track.attr.export}}
#' @keywords ~attr ~attribute
#' @examples
#'
#' emr_db.init_examples()
#' emr_track.attr.set("sparse_track", "test_attr", "value")
#' emr_track.attr.get("sparse_track", "test_attr")
#' @export emr_track.attr.set
emr_track.attr.set <- function(track, attr, value) {
    if (missing(track) || missing(attr) || missing(value)) {
        stop("Usage: emr_track.attr.set(track, attr, value)", call. = FALSE)
    }
    .emr_checkroot()

    if (length(track) != length(attr)) {
        stop("Number of tracks is not equal to the number of attributes", call. = FALSE)
    }

    if (length(attr) != length(value)) {
        stop("Number of values is not equal to the number of attributes", call. = FALSE)
    }

    if (!all(is_ascii_character(attr))) {
        stop("Attribute name must be in ASCII encoding", call. = FALSE)
    }

    if (!all(is_ascii_character(value))) {
        stop("Value must be in ASCII encoding", call. = FALSE)
    }

    if (length(track) > 1) {
        purrr::pwalk(list(track, attr, value), function(tr, a, v) {
            .emr_call("emr_set_track_attr", tr, a, v, FALSE, new.env(parent = parent.frame()))
        })
        dbs <- emr_track.dbs(track, dataframe = FALSE)
        purrr::walk(dbs, ~ {
            .emr_call("update_tracks_attrs_file", .x, new.env(parent = parent.frame()))
        })
    } else {
        .emr_call("emr_set_track_attr", track, attr, value, TRUE, new.env(parent = parent.frame()))
    }

    retv <- 0 # suppress return value
}
