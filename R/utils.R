.emr_call <- function(...) {
    tryCatch(
        {
            res <- .Call(...)
        },
        interrupt = function(interrupt) {
            stop("Command interrupted!", call. = FALSE)
        }
    )
    res
}

.emr_checkroot <- function() {
    if (!exists("EMR_GROOT", envir = .GlobalEnv) || is.null(get("EMR_GROOT", envir = .GlobalEnv))) {
        stop("Database root directory is not set. Please call emr_db.connect().", call. = FALSE)
    }
}

.emr_getOption <- function(x, default = NULL) {
    if (missing(default)) {
        return(options(x)[[1L]])
    }
    if (x %in% names(options())) {
        options(x)[[1L]]
    } else {
        default
    }
}
