#' Prints call stack of the last uncaught error
#'
#' Prints call stack of the last uncaught error in a friendly way.
#'
#' Similarly to 'traceback' this function prints the call stack of the last
#' uncaught error. Yet 'emr_traceback' does it in a more friendly way by
#' omitting the calls that occurred inside the library.
#'
#' @param x see 'traceback'
#' @param max.lines see 'traceback'
#' @return See 'traceback'.
#' @seealso \code{\link{traceback}}
#' @keywords ~trace ~error ~exception
#' @examples
#'
#' try({
#'     emr_db.init_examples()
#'     f <- function() {
#'         emr_screen("blablabla")
#'     }
#'     f()
#'     emr_traceback()
#' })
#'
#' @export emr_traceback
emr_traceback <- function(x = NULL, max.lines = getOption("deparse.max.lines")) {
    x <- NULL

    if (is.null(x) && (exists(".Traceback", envir = baseenv()))) {
        x <- get(".Traceback", envir = baseenv())
    }

    if (!is.null(x) && length(x) > 0) {
        # get the call stack and concatenate all complex commands together
        x <- sapply(x, paste, collapse = "")

        # extract call stack function names
        fnames <- gsub("^(\\S+)\\s*\\(.*\\)$", "\\1", x, perl = TRUE)

        # get the indices of lib functions
        libindices <- which(fnames %in% get(".EMR_FUNCS", envir = .naryn))

        # cut whatever comes after the first lib function
        if (length(libindices) > 0) {
            x <- get(".Traceback")[libindices[length(libindices)]:length(get(".Traceback"))]
        }
    }

    traceback(x, max.lines)
}
