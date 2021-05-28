.onLoad <- function(lib, pkg) {
}

.onAttach <- function(lib, pkg) {
    Sys.umask("0002")

    assign(".EMR_FUNCS", getNamespaceExports("naryn"), envir = .GlobalEnv)

    if (R.Version()$major >= 3) {
          assign(".EMR_LIBDIR", path.package("naryn"), envir = .GlobalEnv)
      } else {
          assign(".EMR_LIBDIR", .path.package("naryn"), envir = .GlobalEnv)
      }

    options(emr_multitasking = TRUE)
    options(emr_min.processes = 8)
    options(emr_max.processes = 20)
    options(emr_max.data.size = 10000000)
    options(emr_eval.buf.size = 1000)
    options(emr_quantile.edge.data.size = 100000)
    options(emr_warning.itr.no.filter.size = 100000)

    # set the EMR_GROOT to samples dir
    emr_db.init_examples()
}

.onDetach <- function(lib) {
    .emr_call("emr_dbunload", new.env(parent = parent.frame()), silent = TRUE)
    if (exists(".EMR_FUNCS", envir = .GlobalEnv)) {
          remove(".EMR_FUNCS", envir = .GlobalEnv)
      }
}
