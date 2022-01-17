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
emr_track.attr.export <- function(track = NULL, attr = NULL) {
    .emr_checkroot()

    if (is.null(track)) {
        track <- emr_track.ls()
    } else {
        track <- unique(track)
    }
    if (!is.null(attr)) {
        attr <- unique(attr)
    }

    .emr_call("emr_get_tracks_attrs", track, attr, new.env(parent = parent.frame()))
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
#' @param track track name
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
emr_track.attr.rm <- function(track = NULL, attr = NULL) {
    if (missing(track) || missing(attr)) {
        stop("Usage: emr_track.attr.rm(track, attr)", call. = FALSE)
    }
    .emr_checkroot()

    .emr_call("emr_set_track_attr", track, attr, NULL, new.env(parent = parent.frame()))
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
#' @param track track name
#' @param attr attribute name
#' @param value value
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
emr_track.attr.set <- function(track = NULL, attr = NULL, value = NULL) {
    if (missing(track) || missing(attr) || missing(value)) {
        stop("Usage: emr_track.attr.set(track, attr, value)", call. = FALSE)
    }
    .emr_checkroot()

    .emr_call("emr_set_track_attr", track, attr, value, new.env(parent = parent.frame()))
    retv <- 0 # suppress return value
}
