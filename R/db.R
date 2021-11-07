#' Deprecated - please user emr_db.connect
#' 
#' @export emr_db.init
emr_db.init <- function(global.dir = NULL, user.dir = NULL, global.load.on.demand = T, user.load.on.demand = T, do.reload = F) {
    db.dirs <- c(global.dir, user.dir)

    if (is.null(user.dir)) {
        load.on.demand <- c(global.load.on.demand)    
    } else {
        load.on.demand <- c(global.load.on.demand, user.load.on.demand)    
    }

    emr_db.connect(db.dirs=db.dirs, load.on.demand=load.on.demand, do.reload=do.reload)
}

#' Initializes connection with Naryn Database
#'
#' Initializes connection with Naryn Database.
#'
#' 'emr_db.connect' initializes the connection with Naryn Database facilitating
#' tracks access. The first db in the db.dirs vector is seen as the Global DB 
#' and as the root directory of the global tracks. The last db in the db.dirs
#' is seed as the User DB and the directory of the user's tracks. Unlike
#' global tracks user tracks may be deleted ('emr_track.rm') or new ones can be
#' created ('emr_track.create'). User db is not required, in which case
#' operations on user tracks will be disabled. Any other db in the db.dirs
#' vector is seen as an extention to the global tracks, the permissions
#' of these extentions are up to the user.
#' 
#' Overriding mechanism: 
#' 
#' The order of the dirs in the db.dirs vector sets their namespace priority.
#' This means that when a track with the same name appears in two or more
#' db.dirs, the considered track will be the one from the db.dir with the 
#' higher position in the vector. When an overriding track is removed, naryn
#' will update the track list such that the once overridden track will be available
#'
#' When the package is attached it internally calls 'emr_db.init_examples'
#' which sets Global DB to 'PKGDIR/naryndb/test' and Global DB to NULL
#' ('PKGDIR' is the directory where the package is installed).
#'
#' Physical files in the database are supposed to be managed exclusively by
#' Naryn itself. Manual modification, addition or deletion of track files may
#' be done, yet it must be ratified via running 'emr_db.reload'. Some of these
#' manual changes however (like moving a track from global space to user or
#' vice versa) might cause 'emr_db.init' to fail. 'emr_db.reload' cannot be
#' invoked then as it requires first the connection to the DB be established.
#' To break the deadlock use 'do.reload=True' parameter within 'emr_db.init'.
#' This will connect to the DB and rebuild the DB index files in one step.
#'
#' If 'load.on.demand' is 'TRUE' a track is loaded into memory only when it is
#' accessed and it is unloaded from memory as R sessions ends or the package is
#' unloaded.
#'
#' If 'load.on.demand' parameter is 'FALSE', all the tracks from the specified
#' space (global / user) are pre-loaded into memory making subsequent track
#' access significantly faster. As loaded tracks reside in shared memory, other
#' R sessions running on the same machine, may also enjoy significant run-time
#' boost. On the flip side, pre-loading all the tracks prolongs the execution
#' of 'emr_db.init' and requires enough memory to accommodate all the data.
#'
#' Choosing between the two modes depends on the specific needs. While
#' 'load.on.demand=TRUE' seems to be a solid default choice, in an enviroment
#' where there are frequent short-living R sessions, each accessing a track one
#' might opt for running a "daemon" - an additional permanent R session. The
#' daemon would pre-load all the tracks in advance and stay alive thus boosting
#' the run-time of the later emerging sessions.
#'
#' Upon completion the connection is established with the database and a few
#' global variables are added to the environment. These variables should not be
#' modified by the user!
#'
#' \tabular{lll}{ 
#' EMR_GROOT \tab Root directory of global tracks \cr 
#' EMR_UROOT \tab Root directory of user tracks \cr 
#' EMR_ROOTS \tab Vector of directories (db.dirs) \cr 
#' }
#'
#' @aliases emr_db.init emr_db.init_examples
#' @param db.dirs vector of db directories
#' @param load.on.demand vector of booleans, same length as db.dirs, if load.on.demand[i] is FALSE, tracks from db.dirs[i] will be pre-loaded
#' @param do.reload If 'TRUES', rebuilds DB index files
#' @return None.
#' @seealso \code{\link{emr_db.reload}}, \code{\link{emr_track.import}},
#' \code{\link{emr_track.create}}, \code{\link{emr_track.rm}},
#' \code{\link{emr_track.ls}}, \code{\link{emr_vtrack.ls}},
#' \code{\link{emr_filter.ls}}
#' @keywords ~db ~data ~database
#' @export emr_db.connect

emr_db.connect <- function(db.dirs = NULL, load.on.demand = NULL, do.reload = F){
    if (is.null(db.dirs)) {
        stop("Usage: emr_db.init(db.dirs, load.on.demand = NULL, do.reload = F)", call. = FALSE)
    }

    db.dirs <- normalizePath(db.dirs) # get absolute path

    if (length(unique(db.dirs)) != length(db.dirs)) {
        stop("DB directories should differ from one another", call. = FALSE)
    }

    if (!is.null(load.on.demand) && (length(db.dirs) != length(load.on.demand))) {
        stop("load.on.demand must be in the same length of db.dirs", call. = FALSE)
    }

    # We set the groot to be the first
    # directory in the vector
    EMR_GROOT <<- db.dirs[1]

    # We set the uroot to be the last
    if (length(db.dirs) > 1) {
        EMR_UROOT <<- tail(db.dirs, n=1)
    }

    EMR_ROOTS <<- db.dirs

    if (is.null(load.on.demand)) {
        load.on.demand <- !logical(length(db.dirs))
    }
    
    success <- FALSE

    tryCatch(
        {
            .emr_call("emr_dbinit", db.dirs, load.on.demand, do.reload, new.env(parent = parent.frame()), silent = TRUE)
            success <- TRUE
        },
        finally = {
            if (!success) {
                remove("EMR_GROOT", envir = .GlobalEnv)
                remove("EMR_UROOT", envir = .GlobalEnv)
                remove("EMR_ROOTS", envir = .GlobalEnv)
            }
        }
    )
    retv <- NULL
}


emr_db.init_examples <- function() {
    emr_db.init(system.file("naryndb/test", package = "naryn"))
}



#' Reloads database
#'
#' Reloads database
#'
#' Rebuilds Naryn database index files. Use this function if you manually
#' add/delete/move/modify track files or if you suspect that the database is
#' corrupted: existing tracks cannot be found, deleted ones continue to appear
#' or a warning message is issued by Naryn itself recommending to run
#' 'emr_db.reload'.
#'
#' @seealso \code{\link{emr_db.init}}, \code{\link{emr_track.ls}},
#' \code{\link{emr_vtrack.ls}}
#' @keywords ~db
#' @export emr_db.reload
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



#' Defines an ids subset
#'
#' Defines an ids subset.
#'
#' 'emr_db.subset' creates an ids subset" ("viewport") of data of "fraction *
#' sizeof('src')" size by sampling the ids from 'src'. Once the subset is
#' defined only the ids that are in the subset are used by various functions
#' and iterators. Other ids are ignored.
#'
#' 'src' can be a track name or an ids table. If 'complementary' is 'TRUE' the
#' complementary set of sampled ids is used as a subset.
#'
#' If 'src' is 'NULL' the current subset is annihilated.
#'
#' @param src track name or ids table or 'NULL'
#' @param fraction fraction of data to be sampled from 'src' in [0,1] range
#' @param complementary 'TRUE' for a complementary subset, otherwise 'FALSE'
#' @return None.
#' @seealso \code{\link{emr_db.init}}, \code{\link{emr_db.subset.ids}},
#' \code{\link{emr_db.subset.info}}
#' @keywords ~db ~data ~database ~subset
#' @export emr_db.subset
emr_db.subset <- function(src = "", fraction = NULL, complementary = NULL) {
    if (!is.null(src) && src == "") {
        stop("Usage: emr_db.subset(src, fraction, complementary)", call. = F)
    }
    .emr_checkroot()

    .emr_call("emr_db_subset", src, fraction, complementary, new.env(parent = parent.frame()))
    retv <- NULL
}



#' Returns the ids that constitute the current ids subset
#'
#' Returns the ids that constitute the current ids subset.
#'
#' 'emr_db.subset.ids' returns the ids that constitute the current ids subset.
#' The ids are returned in "ids table" format.
#'
#' If no ids subset is defined, 'emr_db.subset.ids' returns 'NULL'.
#'
#' @return Ids table or 'NULL'
#' @seealso \code{\link{emr_db.subset}}
#' @keywords ~db ~data ~database ~subset
#' @export emr_db.subset.ids
emr_db.subset.ids <- function() {
    .emr_checkroot()
    .emr_call("emr_db_subset_ids", new.env(parent = parent.frame()))
}



#' Returns information about the current subset
#'
#' Returns information about the current subset.
#'
#' 'emr_db.subset.info' returns the parameters that were used to define the
#' current subset or 'NULL' if no subset has been defined.
#'
#' @return Information about the current subset or 'NULL'.
#' @seealso \code{\link{emr_db.subset}}, \code{\link{emr_db.subset.ids}}
#' @keywords ~db ~data ~database ~subset
#' @export emr_db.subset.info
emr_db.subset.info <- function() {
    .emr_checkroot()
    .emr_call("emr_db_subset_info", new.env(parent = parent.frame()))
}
