#' Reload entries from disk
#'
#' @param db_dir One or more database directories to reload entries from. If NULL - the first database is used.
#'
#' @return None. If the entries were reloaded - the file timestamp is returned invisibly.
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

    if (length(db_dir) > 1) {
        return(apply_entries_multiple_db(db_dir, emr_entries.reload, ret = FALSE))
    }

    entries_file <- file.path(db_dir, "entries.yaml")

    if (file.exists(entries_file)) {
        # compare timestamps to see if we need to reload
        entries_timestamp <- file.info(entries_file)$mtime

        if (!is.null(.naryn$entries[[db_dir]]) &&
            !is.null(.naryn$entries_timestamp[[db_dir]]) &&
            entries_timestamp == .naryn$entries_timestamp[[db_dir]]) {
            return()
        }

        entries <- yaml::read_yaml(file.path(db_dir, "entries.yaml"))
        .naryn$entries[[db_dir]] <- entries
        .naryn$entries_timestamp[[db_dir]] <- entries_timestamp
    }
}

#' Apply an entries function to multiple databases
#'
#' @param db_dir A vector of database directories
#' @param fun The function to apply
#' @param ret Whether to return the result
#' @param ... Additional arguments to pass to the function
#'
#' @return The result of the function. If ret is FALSE, NULL is returned.
#'
#'
#' @noRd
apply_entries_multiple_db <- function(db_dir, fun, ret = TRUE, ...) {
    if (ret) {
        res <- purrr::map(db_dir, fun, ...)
        names(res) <- db_dir
        return(res)
    }
    purrr::walk(db_dir, fun, ...)
}

#' Get an entry
#'
#' @param key The key of the entry to get
#'
#' @return The entry value. If the key does not exist, NULL is returned. For multiple databases, a named list of database entries is returned.
#'
#' @examples
#' emr_db.init_examples()
#' emr_entries.get("entry1")
#'
#' @inheritParams emr_entries.reload
#' @export
emr_entries.get <- function(key, db_dir = NULL) {
    if (is.null(db_dir)) {
        db_dir <- emr_db.ls()[1]
    }

    if (length(db_dir) > 1) {
        return(apply_entries_multiple_db(db_dir, emr_entries.get, key = key, ret = TRUE))
    }

    emr_entries.reload(db_dir)

    .naryn$entries[[db_dir]][[key]]
}

#' Get all entries
#'
#' @return A list of entries. For multiple databases, a named list of database entries is returned.
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

    if (length(db_dir) > 1) {
        return(apply_entries_multiple_db(db_dir, emr_entries.get_all, ret = TRUE))
    }

    emr_entries.reload(db_dir)

    .naryn$entries[[db_dir]]
}

update_entries_timestamp <- function(db_dir) {
    entries_file <- file.path(db_dir, "entries.yaml")
    entries_timestamp <- file.info(entries_file)$mtime
    .naryn$entries_timestamp[[db_dir]] <- entries_timestamp
}

update_entries_file <- function(db_dir) {
    entries <- .naryn$entries[[db_dir]]
    entries_file <- file.path(db_dir, "entries.yaml")
    # check if file has write permissions
    if (file.exists(entries_file) && file.access(entries_file, mode = 2)) {
        stop("Cannot write to entries file. Please check file permissions.")
    }

    yaml::write_yaml(entries, entries_file)

    update_entries_timestamp(db_dir)
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
#' emr_entries.set("entry1", "new value")
#' emr_entries.get("entry1")
#'
#' @inheritParams emr_entries.reload
#' @export
emr_entries.set <- function(key, value, db_dir = NULL) {
    if (is.null(db_dir)) {
        db_dir <- emr_db.ls()[1]
    }

    if (length(db_dir) > 1) {
        return(apply_entries_multiple_db(db_dir, emr_entries.set, key = key, value = value, ret = FALSE))
    }

    emr_entries.reload(db_dir)

    .naryn$entries[[db_dir]][[key]] <- value
    update_entries_file(db_dir)
}

#' Remove an entry
#'
#' @param key The key of the entry to remove. If the key does not exist, nothing happens.
#'
#' @return None
#'
#' @examples
#' emr_db.init_examples()
#' emr_entries.rm("entry1")
#' emr_entries.ls()
#'
#' @inheritParams emr_entries.reload
#' @export
emr_entries.rm <- function(key, db_dir = NULL) {
    if (is.null(db_dir)) {
        db_dir <- emr_db.ls()[1]
    }

    if (length(db_dir) > 1) {
        return(apply_entries_multiple_db(db_dir, emr_entries.rm, key = key, ret = FALSE))
    }

    emr_entries.reload(db_dir)

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

    if (length(db_dir) > 1) {
        return(apply_entries_multiple_db(db_dir, emr_entries.rm_all, ret = FALSE))
    }

    emr_entries.reload(db_dir)

    .naryn$entries[[db_dir]] <- list()
    update_entries_file(db_dir)
}


#' List entries
#'
#' @return A vector of entry names. For multiple databases, a named list of database entries is returned.
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

    if (length(db_dir) > 1) {
        return(apply_entries_multiple_db(db_dir, emr_entries.ls, ret = TRUE))
    }

    emr_entries.reload(db_dir)

    keys <- names(.naryn$entries[[db_dir]])
    if (is.null(keys)) {
        keys <- character(0)
    }
    return(keys)
}
