#' Reload entries from disk
#'
#' @param db_dir The directory of the database to reload entries from
#'
#' @return None
#'
#' @examples
#' emr_db.init_examples()
#' emr_entries.reload()
#'
#' @export
emr_entries.reload <- function(db_dir = NULL) {
    if (is.null(db_dir)) {
        db_dir <- emr_db.ls()[1]
    }

    if (file.exists(file.path(db_dir, "entries.yaml"))) {
        entries <- yaml::read_yaml(file.path(db_dir, "entries.yaml"))
        .naryn$entries[[db_dir]] <- entries
    }
}

#' Get an entry
#'
#' @param key The key of the entry to get
#'
#' @return The entry. If the key does not exist, NULL is returned.
#'
#' @examples
#' emr_db.init_examples()
#' emr_entries.get("record1")
#'
#' @inheritParams emr_entries.reload
#' @export
emr_entries.get <- function(key, db_dir = NULL) {
    if (is.null(db_dir)) {
        db_dir <- emr_db.ls()[1]
    }

    if (is.null(.naryn$entries[[db_dir]])) {
        emr_entries.reload(db_dir)
    }

    .naryn$entries[[db_dir]][[key]]
}

#' Get all entries
#'
#' @return A list of entries
#'
#' @examples
#' emr_db.init_examples()
#' emr_entries.get_all()
#'
#' @inheritParams emr_entries.reload
#' @export
emr_entries.get_all <- function(db_dir = NULL) {
    if (is.null(db_dir)) {
        db_dir <- emr_db.ls()[1]
    }

    if (is.null(.naryn$entries[[db_dir]])) {
        emr_entries.reload(db_dir)
    }

    .naryn$entries[[db_dir]]
}

update_entries_file <- function(db_dir) {
    entries <- .naryn$entries[[db_dir]]
    entries_file <- file.path(db_dir, "entries.yaml")
    # check if file has write permissions
    if (file.exists(entries_file) && file.access(entries_file, mode = 2)) {
        stop("Cannot write to entries file. Please check file permissions.")
    }

    yaml::write_yaml(entries, entries_file)
}

#' Set an entry
#'
#' @param key The key of the entry to set
#' @param value The value of the entry to set. This can be anything that can be serialized to YAML
#'
#' @return None
#'
#' @examples
#' emr_db.init_examples()
#' emr_entries.set("record1", "new value")
#' emr_entries.get("record1")
#'
#' @inheritParams emr_entries.reload
#' @export
emr_entries.set <- function(key, value, db_dir = NULL) {
    if (is.null(db_dir)) {
        db_dir <- emr_db.ls()[1]
    }

    if (is.null(.naryn$entries[[db_dir]])) {
        emr_entries.reload(db_dir)
    }

    .naryn$entries[[db_dir]][[key]] <- value
    update_entries_file(db_dir)
}

#' Remove an entry
#'
#' @param key The key of the entry to remove
#'
#' @return None
#'
#' @examples
#' emr_db.init_examples()
#' emr_entries.rm("record1")
#' emr_entries.ls()
#'
#' @inheritParams emr_entries.reload
#' @export
emr_entries.rm <- function(key, db_dir = NULL) {
    if (is.null(db_dir)) {
        db_dir <- emr_db.ls()[1]
    }

    if (is.null(.naryn$entries[[db_dir]])) {
        emr_entries.reload(db_dir)
    }

    .naryn$entries[[db_dir]][[key]] <- NULL
    update_entries_file(db_dir)
}

#' Remove all entries
#'
#' @return None
#'
#' @examples
#' emr_db.init_examples()
#' emr_entries.rm_all()
#'
#' @inheritParams emr_entries.reload
#' @export
emr_entries.rm_all <- function(db_dir = NULL) {
    if (is.null(db_dir)) {
        db_dir <- emr_db.ls()[1]
    }

    if (is.null(.naryn$entries[[db_dir]])) {
        emr_entries.reload(db_dir)
    }

    .naryn$entries[[db_dir]] <- list()
    update_entries_file(db_dir)
}


#' List entries
#'
#' @return A list of entries
#'
#' @examples
#' emr_db.init_examples()
#' emr_entries.ls()
#'
#' @inheritParams emr_entries.reload
#' @export
emr_entries.ls <- function(db_dir = NULL) {
    if (is.null(db_dir)) {
        db_dir <- emr_db.ls()[1]
    }

    if (is.null(.naryn$entries[[db_dir]])) {
        emr_entries.reload(db_dir)
    }

    keys <- names(.naryn$entries[[db_dir]])
    if (is.null(keys)) {
        keys <- character(0)
    }
    return(keys)
}
