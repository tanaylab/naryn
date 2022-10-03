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

.emr_env <- function() {
    e <- new.env(parent = parent.frame(2))
    assign(".naryn", .naryn, envir = e)
    return(e)
}

.emr_checkroot <- function() {
    if (!exists("EMR_GROOT", envir = .naryn) || is.null(get("EMR_GROOT", envir = .naryn))) {
        stop("Database root directory is not set. Please call emr_db.connect().", call. = FALSE)
    }
}

#' Get a vector of variables inside an expression
#'
#' @param expr string with a naryn expression
#'
#' @noRd
#' @export
.emr_expr_vars <- function(expr) {
    all.vars(as.list(parse(text = expr))[[1]])
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

is_ascii_character <- function(x) {
    !is.na(x) & !is.na(iconv(x, "ASCII", "ASCII"))
}
