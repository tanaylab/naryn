get_src_str <- function(src) {
    glue::glue("{src}.")
}

get_keepref_str <- function(keepref) {
    if (keepref) {
        return("krT.")
    } else {
        return("krF.")
    }
}

get_val_str <- function(val) {
    if (!is.null(val)) {
        val <- formatC(val, format = "fg") # do not use scientific notation
        return(glue::glue("vals_{vals}.", vals = paste(sort(unique(val)), collapse = "_")))
    } else {
        return("")
    }
}

get_time_shift_str <- function(time.shift) {
    if (!is.null(time.shift)) {
        time.shift <- formatC(time.shift, format = "fg") # do not use scientific notation
        if (length(time.shift) == 1) {
            return(glue::glue("ts_{time.shift}."))
        } else if (length(time.shift) == 2) {
            return(glue::glue("ts_{time.shift[1]}_{time.shift[2]}."))
        } else {
            stop("cannot parse time.shift argument", .call = FALSE)
        }
    } else {
        return("")
    }
}

get_expiration_str <- function(expiration) {
    if (!is.null(expiration)) {
        expiration <- formatC(expiration, format = "fg") # do not use scientific notation
        return(glue::glue("exp_{expiration}."))
    } else {
        return("")
    }
}

get_operator_str <- function(operator) {
    if (operator != "=") {
        operator <- .operator_to_str(operator)
        return(glue::glue("op_{operator}"))
    } else {
        return("")
    }
}

get_params_str <- function(params) {
    params <- formatC(params, format = "fg")
    return(glue::glue("params_{p}.", p = paste(sort(unique(params)), collapse = "_")))
}

get_filter_str <- function(filter) {
    if (!is.null(filter)) {
        return(glue::glue("filter_{filter}."))
    } else {
        return("")
    }
}

get_func_str <- function(func) {
    if (!is.null(func)) {
        return(glue::glue("func_{func}."))
    } else {
        return("func_")
    }
}
