#' An environment for storing the package global variables
#'
#'
#' @export
#' @noRd
.naryn <- new.env(parent = emptyenv())
.naryn$entries <- list()

.onLoad <- function(lib, pkg) {
}

.onAttach <- function(lib, pkg) {
    Sys.umask("007")

    assign(".EMR_FUNCS", getNamespaceExports("naryn"), envir = .naryn)

    assign(".EMR_LIBDIR", path.package("naryn"), envir = .naryn)

    options(emr_multitasking = TRUE)
    options(emr_min.processes = max(1, parallel::detectCores() * 0.2))
    options(emr_max.processes = max(1, parallel::detectCores() * 0.8))
    options(emr_max.data.size = 10000000)
    options(emr_eval.buf.size = 1000)
    options(emr_quantile.edge.data.size = 100000)
    options(emr_warning.itr.no.filter.size = 100000)

    # set the EMR_GROOT to samples dir
    emr_db.init_examples()
}

.onDetach <- function(lib) {
    .emr_call("emr_dbunload", .emr_env(), silent = TRUE)
    if (exists(".EMR_FUNCS", envir = .naryn)) {
        remove(".EMR_FUNCS", envir = .naryn)
    }
}
