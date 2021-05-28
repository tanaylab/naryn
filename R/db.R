
emr_db.init <- function(global.dir = NULL, user.dir = NULL, global.load.on.demand = T, user.load.on.demand = T, do.reload = F) {
    if (is.null(global.dir)) {
          stop("Usage: emr_db.init(global.dir, user.dir = NULL, global.load.on.demand = T, user.load.on.demand = T, do.reload = F)", call. = F)
      }

    # replace ~ mark by full path
    oldwd <- getwd()

    global.dir <- path.expand(global.dir)
    setwd(global.dir)
    global.dir <- getwd() # get absolute path
    setwd(oldwd)

    if (!is.null(user.dir)) {
        user.dir <- path.expand(user.dir)
        setwd(user.dir)
        user.dir <- getwd() # get absolute path
        setwd(oldwd)

        if (global.dir == user.dir) {
              stop("Global space root directory should differ from user space root directory", call. = F)
          }
    }

    EMR_GROOT <<- global.dir
    EMR_UROOT <<- user.dir
    success <- F
    tryCatch(
        {
            .emr_call("emr_dbinit", global.dir, user.dir, global.load.on.demand, user.load.on.demand, do.reload, new.env(parent = parent.frame()), silent = TRUE)
            success <- T
        },
        finally = {
            if (!success) {
                remove("EMR_GROOT", envir = .GlobalEnv)
                remove("EMR_UROOT", envir = .GlobalEnv)
            }
        }
    )
    retv <- NULL
}

emr_db.init_examples <- function() {
    emr_db.init(paste(.EMR_LIBDIR, "naryndb/test", sep = "/"))
}

emr_db.reload <- function() {
    success <- F
    tryCatch(
        {
            .emr_call("emr_dbreload", silent = TRUE)
            success <- T
        },
        finally = {
            if (!success) {
                remove("EMR_GROOT", envir = .GlobalEnv)
                remove("EMR_UROOT", envir = .GlobalEnv)
            }
        }
    )
    retv <- NULL
}

emr_db.subset <- function(src = "", fraction = NULL, complementary = NULL) {
    if (!is.null(src) && src == "") {
          stop("Usage: emr_db.subset(src, fraction, complementary)", call. = F)
      }
    .emr_checkroot()

    .emr_call("emr_db_subset", src, fraction, complementary, new.env(parent = parent.frame()))
    retv <- NULL
}

emr_db.subset.ids <- function() {
    .emr_checkroot()
    .emr_call("emr_db_subset_ids", new.env(parent = parent.frame()))
}

emr_db.subset.info <- function() {
    .emr_checkroot()
    .emr_call("emr_db_subset_info", new.env(parent = parent.frame()))
}
