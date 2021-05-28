emr_traceback <- function(x = NULL, max.lines = getOption("deparse.max.lines")) {
    x <- NULL

    if (is.null(x) && (exists(".Traceback", envir = baseenv()))) {
          x <- get(".Traceback", envir = baseenv())
      }

    if (!is.null(x) && length(x) > 0) {
        # get the call stack and concatenate all complex commands together
        x <- sapply(x, paste, collapse = "")

        # extract call stack function names
        fnames <- gsub("^(\\S+)\\s*\\(.*\\)$", "\\1", x, perl = T)

        # get the indices of lib functions
        libindices <- which(fnames %in% get(".EMR_FUNCS", envir = .GlobalEnv))

        # cut whatever comes after the first lib function
        if (length(libindices) > 0) {
            x <- get(".Traceback")[libindices[length(libindices)]:length(get(".Traceback"))]
        }
    }

    traceback(x, max.lines)
}
