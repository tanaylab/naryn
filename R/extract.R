
emr_cor <- function(..., cor.exprs = NULL, include.lowest = FALSE, right = TRUE, stime = NULL, etime = NULL, iterator = NULL, keepref = F, filter = NULL) {
    args <- list(...)
    if (length(args) < 2 || (length(args) %% 2 != 0 && (length(args) - 1) %% 2 != 0) || is.null(cor.exprs)) {
          stop("Usage: emr_cor([factor.expr, breaks]+, cor.exprs, include.lowest = F, right = T, stime = NULL, etime = NULL, iterator = NULL, keepref = F, filter = NULL)", call. = F)
      }
    .emr_checkroot()

    exprs <- c()
    breaks <- list()

    for (i in (0:(length(args) / 2 - 1))) {
        exprs <- append(exprs, args[[i * 2 + 1]])
        breaks[length(breaks) + 1] <- list(args[[i * 2 + 2]])
    }

    exprs <- append(exprs, cor.exprs)

    res <- .emr_call("emr_covariance", exprs, breaks, include.lowest, right, stime, etime, iterator, keepref, .emr_filter(filter), new.env(parent = parent.frame()))
    res
}

emr_dist <- function(..., include.lowest = FALSE, right = TRUE, stime = NULL, etime = NULL, iterator = NULL, keepref = FALSE, filter = NULL) {
    args <- list(...)
    if (length(args) < 2 || (length(args) %% 2 != 0 && (length(args) - 1) %% 2 != 0)) {
          stop("Usage: emr_dist([expr, breaks]+, include.lowest = F, right = T, stime = NULL, etime = NULL, iterator = NULL, keepref = F, filter = NULL)", call. = F)
      }
    .emr_checkroot()

    exprs <- c()
    breaks <- list()

    for (i in (0:(length(args) / 2 - 1))) {
        exprs <- append(exprs, args[[i * 2 + 1]])
        breaks[length(breaks) + 1] <- list(args[[i * 2 + 2]])
    }

    res <- .emr_call("emr_dist", exprs, breaks, include.lowest, right, stime, etime, iterator, keepref, .emr_filter(filter), new.env(parent = parent.frame()))
    res
}

emr_extract <- function(expr, tidy = F, sort = F, names = NULL, stime = NULL, etime = NULL, iterator = NULL, keepref = F, filter = NULL) {
    if (missing(expr)) {
          stop("Usage: emr_extract(expr, tidy = F, sort = F, names = NULL, tidy = F, stime = NULL, etime = NULL, iterator = NULL, keepref = F, filter = NULL)", call. = F)
      }
    .emr_checkroot()

    .emr_call("emr_extract", expr, names, tidy, sort, stime, etime, iterator, keepref, .emr_filter(filter), new.env(parent = parent.frame()))
}

emr_ids_coverage <- function(ids, tracks, stime = NULL, etime = NULL, filter = NULL) {
    if (missing(ids) || missing(tracks)) {
          stop("Usage: emr_ids_coverage(ids, tracks, stime = NULL, etime = NULL, filter = NULL)", call. = F)
      }
    .emr_checkroot()

    if (is.null(stime) && is.null(etime) && is.null(filter)) {
          .emr_call("emr_ids_dist", ids, tracks, new.env(parent = parent.frame()))
      } else {
        if (is.null(filter)) {
              filter <- "..emr_tmp_filter"
          } else {
              filter <- paste0("(", filter, ")", "& ..emr_tmp_filter")
          }

        if (is.character(ids)) { # ids is a name of the track
              assign("..emr_tmp_filter", emr_track.ids(ids), envir = .GlobalEnv)
          } else {
              assign("..emr_tmp_filter", data.frame(id = unique(ids$id)), envir = .GlobalEnv)
          }

        tryCatch(
            {
                .emr_call("emr_ids_dist_with_iterator", ids, tracks, stime, etime, .emr_filter(filter), new.env(parent = parent.frame()))
            },
            finally = {
                rm("..emr_tmp_filter", envir = .GlobalEnv)
            }
        )
    }
}

emr_ids_vals_coverage <- function(ids, tracks, stime = NULL, etime = NULL, filter = NULL) {
    if (missing(ids) || missing(tracks)) {
          stop("Usage: emr_ids_vals_coverage(ids, tracks, stime = NULL, etime = NULL, filter = NULL)", call. = F)
      }
    .emr_checkroot()

    if (is.null(filter)) {
          filter <- "..emr_tmp_filter"
      } else {
          filter <- paste0("(", filter, ")", "& ..emr_tmp_filter")
      }

    if (is.character(ids)) { # ids is a name of the track
          assign("..emr_tmp_filter", emr_track.ids(ids), envir = .GlobalEnv)
      } else {
          assign("..emr_tmp_filter", data.frame(id = unique(ids$id)), envir = .GlobalEnv)
      }

    tryCatch(
        {
            .emr_call("emr_ids_vals_dist", ids, tracks, stime, etime, .emr_filter(filter), new.env(parent = parent.frame()))
        },
        finally = {
            rm("..emr_tmp_filter", envir = .GlobalEnv)
        }
    )
}

emr_quantiles <- function(expr, percentiles = 0.5, stime = NULL, etime = NULL, iterator = NULL, keepref = F, filter = NULL) {
    if (missing(expr)) {
          stop("Usage: emr_quantiles(expr, percentiles = 0.5, stime = NULL, etime = NULL, iterator = NULL, keepref = F, filter = NULL)", call. = F)
      }
    .emr_checkroot()

    .emr_call("emr_quantiles", expr, percentiles, stime, etime, iterator, keepref, .emr_filter(filter), new.env(parent = parent.frame()))
}

emr_screen <- function(expr, sort = F, stime = NULL, etime = NULL, iterator = NULL, keepref = F, filter = NULL) {
    if (missing(expr)) {
          stop("Usage: emr_screen(expr, sort = F, stime = NULL, etime = NULL, iterator = NULL, keepref = F, filter = NULL)", call. = F)
      }
    .emr_checkroot()

    .emr_call("emr_screen", expr, sort, stime, etime, iterator, keepref, .emr_filter(filter), new.env(parent = parent.frame()))
}

emr_summary <- function(expr, stime = NULL, etime = NULL, iterator = NULL, keepref = F, filter = NULL) {
    if (missing(expr)) {
          stop("Usage: emr_summary(expr, stime = NULL, etime = NULL, iterator = NULL, keepref = F, filter = NULL)", call. = F)
      }
    .emr_checkroot()

    .emr_call("emr_summary", expr, stime, etime, iterator, keepref, .emr_filter(filter), new.env(parent = parent.frame()))
}
