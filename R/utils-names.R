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

.str_to_operator <- function(str) {
    convertor <- c("eq" = "=", "lt" = "<", "lte" = "<=", "gt" = ">", "gte" = ">=")
    return(convertor[str])
}

.operator_to_str <- function(op) {
    convertor <- c("=" = "eq", "<" = "lt", "<=" = "lte", ">" = "gt", ">=" = "gte")
    return(convertor[op])
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

emr_vtrack.parse_params <- function(params_str) {
    params <- strsplit(params_str, "_")[[1]]
    params <- as.numeric(params)
    return(params)
}

get_filter_str <- function(filter) {
    if (!is.null(filter)) {
        filter <- logical_to_varname(filter)
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

#' Convert a logical expression to a valid variable name
#'
#' This function converts a logical expression into a valid R variable name by
#' replacing special characters with special strings. It also checks if any
#' special string is present in the input and aborts with an error message in
#' such case. Note that spaces are removed from the input.
#'
#' @param logic_expr A string containing a logical expression to be converted.
#'
#' @return A string that represents a valid R variable name.
#'
#' @examples
#' logical_to_varname("x > 10 & y < 20")
#'
#' @noRd
logical_to_varname <- function(logic_expr) {
    # Check if any of the special strings is present in the input
    special_strings <- c("__gt__", "__lt__", "__eq__", "__and__", "__or__", "__not__", "__ob__", "__cb__")
    for (special_string in special_strings) {
        if (grepl(special_string, logic_expr)) {
            stop(glue::glue("Invalid input: {logic_expr} contains a special string: {special_string}"), .call = FALSE)
        }
    }

    # Continue with the conversion
    logic_expr <- gsub(" ", "", logic_expr) # remove spaces
    logic_expr <- gsub(">", "__gt__", logic_expr)
    logic_expr <- gsub("<", "__lt__", logic_expr)
    logic_expr <- gsub("=", "__eq__", logic_expr)
    logic_expr <- gsub("&", "__and__", logic_expr)
    logic_expr <- gsub("\\|", "__or__", logic_expr)
    logic_expr <- gsub("!", "__not__", logic_expr)
    logic_expr <- gsub("\\(", "__ob__", logic_expr) # opening bracket
    logic_expr <- gsub("\\)", "__cb__", logic_expr) # closing bracket

    # Ensure the first character is not a digit
    if (grepl("^[0-9]", logic_expr)) {
        logic_expr <- paste0("__", logic_expr)
    }
    return(logic_expr)
}

#' Convert a valid variable name back to its original logical expression
#'
#' This function converts a valid R variable name back to its original logical
#' expression by replacing special strings with their corresponding special characters.
#'
#' @param var_name A string that represents a valid R variable name.
#'
#' @return A string that represents the original logical expression.
#'
#' @examples
#' varname_to_logical("x__gt__10__and__y__lt__20")
#'
#' @noRd
varname_to_logical <- function(var_name) {
    var_name <- gsub("__gt__", ">", var_name)
    var_name <- gsub("__lt__", "<", var_name)
    var_name <- gsub("__eq__", "=", var_name)
    var_name <- gsub("__and__", "&", var_name)
    var_name <- gsub("__or__", "|", var_name)
    var_name <- gsub("__not__", "!", var_name)
    var_name <- gsub("__ob__", "(", var_name) # opening bracket
    var_name <- gsub("__cb__", ")", var_name) # closing bracket


    return(var_name)
}
