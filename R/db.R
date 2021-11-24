#' Deprecated - please user emr_db.connect
#'
#' @export emr_db.init
#' @rdname emr_db.connect
emr_db.init <- function(global.dir = NULL, user.dir = NULL, global.load.on.demand = T, user.load.on.demand = T, do.relaod = F) {
    lifecycle::deprecate_soft(
        when = "2.6.2",
        what = "emr_db.init()",
        with = "emr_db.connect()",
    )

    db_dirs <- c(global.dir, user.dir)

    if (is.null(user.dir)) {
        load_on_demand <- c(global.load.on.demand)
    } else {
        load_on_demand <- c(global.load.on.demand, user.load.on.demand)
    }

    emr_db.connect(db_dirs = db_dirs, load_on_demand = load_on_demand, do_relaod = do.relaod)
}

#' Initializes connection with Naryn Database
#'
#' Call `emr_db.connect` function to establish the access to the tracks in the db_dirs.
#' To establish a connection using `emr_db.connect`, Naryn requires to specify at-least
#' one db dir, optionaly, `emr_db.connect` accepts additional db dirs which can also
#' contain additional tracks.
#' In a case where 2 or more db dirs, contain the same track name (namespace collision),
#' the  track will  be taken from the db dir which  was passed *last* in  the order of
#' connections.  For example, if  we have 2 db dirs `/db1`  and `/db2`  which both contain
#' a track named  `track1`, the call  `emr_db.connect(c('/db1', '/db2'))` will result with
#' Naryn  using `track1` from `/db2`. As  you might expect the overriding is consistent not
#' only for the track's data it self, but also for any other Naryn entity using or pointing
#' to the track.
#'
#' Even though all the db dirs may contain track files their designation is different.
#' All the db dirs except the last dir in the order of connectios are mainly read-only.
#' The directory which  was connected last in  the order, also known as *user dir*, is
#' intended to store volatile data like the results of intermediate calculations.
#' New tracks  can be created only in  the db dir which was last in  the order of
#' connections, using `emr_track.import` or `emr_track.create`. In order to write tracks
#' to a db dir which is not last in the connection order, the user must explicitly
#' reconnect and set the required db dir as the last in order, this should be done for a
#' well justified reason.
#'
#' When the package is attached it internally calls 'emr_db.init_examples'
#' which sets a single example db dir - 'PKGDIR/naryndb/test'.
#' ('PKGDIR' is the directory where the package is installed).
#'
#' Physical files in the database are supposed to be managed exclusively by
#' Naryn itself. Manual modification, addition or deletion of track files may
#' be done, yet it must be ratified via running 'emr_db.reload'. Some of these
#' manual changes however (like moving a track from global space to user or
#' vice versa) might cause 'emr_db.init' to fail. 'emr_db.reload' cannot be
#' invoked then as it requires first the connection to the DB be established.
#' To break the deadlock use 'do_relaod=True' parameter within 'emr_db.init'.
#' This will connect to the DB and rebuild the DB index files in one step.
#'
#' If 'load_on_demand' is 'TRUE' a track is loaded into memory only when it is
#' accessed and it is unloaded from memory as R sessions ends or the package is
#' unloaded.
#'
#' If 'load_on_demand' parameter is 'FALSE', all the tracks from the specified
#' space (global / user) are pre-loaded into memory making subsequent track
#' access significantly faster. As loaded tracks reside in shared memory, other
#' R sessions running on the same machine, may also enjoy significant run-time
#' boost. On the flip side, pre-loading all the tracks prolongs the execution
#' of 'emr_db.init' and requires enough memory to accommodate all the data.
#'
#' Choosing between the two modes depends on the specific needs. While
#' 'load_on_demand=TRUE' seems to be a solid default choice, in an enviroment
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
#' EMR_GROOT \tab First db dir of tracks in the order of connections \cr
#' EMR_UROOT \tab Last db dir of tracks in the order of connection (user dir) \cr
#' EMR_ROOTS \tab Vector of directories (db_dirs) \cr
#' }
#'
#' @aliases emr_db.init emr_db.init_examples
#' @param db_dirs vector of db directories
#' @param load_on_demand vector of booleans, same length as db_dirs, if load_on_demand[i] is FALSE, tracks from db_dirs[i] will be pre-loaded. If NULL is passed, \code{load_on_demand} is set to TRUE on all the databases
#' @param do_reload If \code{TRUE}, rebuilds DB index files.
#' @return None.
#' @seealso \code{\link{emr_db.reload}}, \code{\link{emr_track.import}},
#' \code{\link{emr_track.create}}, \code{\link{emr_track.rm}},
#' \code{\link{emr_track.ls}}, \code{\link{emr_vtrack.ls}},
#' \code{\link{emr_filter.ls}}
#' @keywords ~db ~data ~database
#' @export emr_db.connect
emr_db.connect <- function(db_dirs = NULL, load_on_demand = NULL, do_relaod = F) {
    if (is.null(db_dirs)) {
        stop("Usage: emr_db.connect(db_dirs, load_on_demand = NULL, do_relaod = F)", call. = FALSE)
    }

    db_dirs <- normalizePath(db_dirs) # get absolute path

    if (any(duplicated(db_dirs))) {
        stop("DB directories should differ from one another", call. = FALSE)
    }

    if (!is.null(load_on_demand) && (length(db_dirs) != length(load_on_demand))) {
        stop("load_on_demand must be in the same length of db_dirs", call. = FALSE)
    }

    # We set the groot to be the first
    # directory in the vector
    EMR_GROOT <<- db_dirs[1]

    # We set the uroot to be the last
    if (length(db_dirs) > 1) {
        EMR_UROOT <<- tail(db_dirs, n = 1)
    }

    EMR_ROOTS <<- db_dirs

    if (is.null(load_on_demand)) {
        load_on_demand <- !logical(length(db_dirs))
    }

    success <- FALSE

    tryCatch(
        {
            .emr_call("emr_dbinit", db_dirs, load_on_demand, do_relaod, new.env(parent = parent.frame()), silent = TRUE)
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
    return(NULL)
}

#' Initialize the examples database
#'
#' @export
#' @noRd
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
    return(NULL)
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
    return(NULL)
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
