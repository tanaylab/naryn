.onLoad <- function(lib, pkg) {
}

.onAttach <- function(lib, pkg) {
	Sys.umask("0002")

	assign(".EMR_FUNCS", getNamespaceExports("naryn"), envir = .GlobalEnv)

	if (R.Version()$major >= 3)
	    assign(".EMR_LIBDIR", path.package("naryn"), envir = .GlobalEnv)
	else
	    assign(".EMR_LIBDIR", .path.package("naryn"), envir = .GlobalEnv)	

    options(emr_multitasking = T)
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
	if (exists(".EMR_FUNCS", envir = .GlobalEnv))
		remove(".EMR_FUNCS", envir = .GlobalEnv)
}

.emr_call <- function(...) {
	tryCatch({ res <- .Call(...) },
			 interrupt = function(interrupt){ stop("Command interrupted!", call. = FALSE); } )
	res
}

.emr_checkroot <- function() {
	if (!exists("EMR_GROOT", envir = .GlobalEnv) || is.null(get("EMR_GROOT", envir = .GlobalEnv)))
		stop("Database root directory is not set. Please call emr_db.init().", call. = F)
}

.emr_getOption <- function(x, default = NULL) {
    if (missing(default)) 
        return(options(x)[[1L]])
    if (x %in% names(options())) 
        options(x)[[1L]]
    else default
}

.emr_db_constants_load <- function() {
    assign("MINID", .emr_call("emr_minid", new.env(parent = parent.frame()), silent = TRUE), envir = .GlobalEnv)
    assign("MAXID", .emr_call("emr_maxid", new.env(parent = parent.frame()), silent = TRUE), envir = .GlobalEnv)
    assign("MINTIME", .emr_call("emr_mintime", new.env(parent = parent.frame()), silent = TRUE), envir = .GlobalEnv)
    assign("MAXTIME", .emr_call("emr_maxtime", new.env(parent = parent.frame()), silent = TRUE), envir = .GlobalEnv)
}

.emr_filter <- function(filter) {
    eval(parse(text = sprintf("substitute(%s)", filter)))
}

.emr_filter.get <- function(filterstr) {
    if (!emr_filter.exists(filterstr))
        stop(sprintf("Filter %s does not exist", filterstr), call. = F)

    root <- get("EMR_GROOT", envir = .GlobalEnv)
	filter <- get("EMR_FILTERS", envir = .GlobalEnv)[[root]][[filterstr]]
    if (is.null(filter)) {
        root <- get("EMR_UROOT", envir = .GlobalEnv)
	    filter <- get("EMR_FILTERS", envir = .GlobalEnv)[[root]][[filterstr]]
    }
    filter
}

.emr_track.dir <- function(track) {
    if (is.na(match(track, .emr_call("emr_global_track_names", new.env(parent = parent.frame()), silent = TRUE))))
        dirname <- get("EMR_UROOT", envir = .GlobalEnv)
    else
        dirname <- get("EMR_GROOT", envir = .GlobalEnv)
    paste(dirname, track, sep = "/")
}

.emr_track.var.dir <- function(track) {
    if (is.na(match(track, .emr_call("emr_global_track_names", new.env(parent = parent.frame()), silent = TRUE))))
        dirname <- get("EMR_UROOT", envir = .GlobalEnv)
    else
        dirname <- get("EMR_GROOT", envir = .GlobalEnv)
    paste0(dirname, "/.", track, ".var")
}

.emr_vtrack.get <- function(vtrackstr) {
    if (!emr_vtrack.exists(vtrackstr))
        stop(sprintf("Virtual track %s does not exist", vtrackstr), call. = F)

    root <- get("EMR_GROOT", envir = .GlobalEnv)
    vtrack <- get("EMR_VTRACKS", envir = .GlobalEnv)[[root]][[vtrackstr]]
    if (is.null(vtrack)) {
        root <- get("EMR_UROOT", envir = .GlobalEnv)
        vtrack <- get("EMR_VTRACKS", envir = .GlobalEnv)[[root]][[vtrackstr]]
    }
	vtrack
}

emr_db.init <- function(global.dir = NULL, user.dir = NULL, load.on.demand = T) {
	if (is.null(global.dir))
		stop("Usage: emr_db.init(global.dir, user.dir = NULL, load.on.demand = T)", call. = F);

	# replace ~ mark by full path
	oldwd <- getwd()
	
	global.dir <- path.expand(global.dir)
	setwd(global.dir)
	global.dir <- getwd()  # get absolute path
	setwd(oldwd)

    if (!is.null(user.dir)) {
    	user.dir <- path.expand(user.dir)
    	setwd(user.dir)
    	user.dir <- getwd()  # get absolute path
    	setwd(oldwd)

        if (global.dir == user.dir)
            stop("Global space root directory should differ from user space root directory", call. = F);
    }

	if (!exists("EMR_GROOT", envir = .GlobalEnv) || is.null(get("EMR_GROOT", envir = .GlobalEnv)) || get("EMR_GROOT", envir = .GlobalEnv) != global.dir ||
        (!exists("EMR_UROOT", envir = .GlobalEnv) || is.null(get("EMR_UROOT", envir = .GlobalEnv))) && !is.null(user.dir) ||
        (exists("EMR_UROOT", envir = .GlobalEnv) && !is.null(get("EMR_UROOT", envir = .GlobalEnv)) && (is.null(user.dir) || get("EMR_UROOT", envir = .GlobalEnv) != user.dir)) ||
        !load.on.demand && (!exists("EMR_LOAD_ON_DEMAND", envir = .GlobalEnv) || !identical(get("EMR_LOAD_ON_DEMAND", envir = .GlobalEnv), load.on.demand)))
    {
		EMR_GROOT <<- global.dir
		EMR_UROOT <<- user.dir
        EMR_LOAD_ON_DEMAND <<- load.on.demand
		success <- F
		tryCatch({
        	.emr_call("emr_dbload", global.dir, user.dir, load.on.demand, new.env(parent = parent.frame()), silent = TRUE)
            .emr_db_constants_load()
			success <- T
		},
		finally = {
			if (!success) {
				remove("EMR_GROOT", envir = .GlobalEnv)
				remove("EMR_UROOT", envir = .GlobalEnv)
				remove("EMR_LOAD_ON_DEMAND", envir = .GlobalEnv)
			}
		})
	}
}

emr_db.init_examples <- function() {
	emr_db.init(paste(.EMR_LIBDIR, "naryndb/test", sep = "/"))
}

emr_db.reload <- function() {
	.emr_call("emr_dbload", get("EMR_GROOT", envir = .GlobalEnv), get("EMR_UROOT", envir = .GlobalEnv), F, new.env(parent = parent.frame()), silent = TRUE)
    .emr_db_constants_load()
	retv <- 0 # suppress return value
}

emr_db.subset <- function(src = "", fraction = NULL, complementary = NULL) {
    if (src == "")
        stop("Usage: emr_db.subset(src, fraction, complementary)", call. = F)
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

emr_track.addto <- function(track, src) {
	if (missing(track) || missing(src))
		stop("Usage: emr_track.addto(track, src)", call. = F)
    .emr_checkroot()

	.emr_call("emr_import", track, NULL, NULL, src, T, new.env(parent = parent.frame()))
    .emr_db_constants_load()
    retv <- NULL
}

emr_track.create <- function(track, space, categorical, expr, stime = get("MINTIME", envir = .GlobalEnv), etime = get("MAXTIME", envir = .GlobalEnv),
                             iterator = NULL, keepref = F, filter = NULL)
{
	if (missing(track) || missing(space) || missing(categorical) || missing(expr))
		stop("Usage: emr_track.create(track, space = \"user\", categorical, expr, stime = MINTIME, etime = MAXTIME, iterator = NULL, keepref = F, filter = NULL)", call. = F)
    .emr_checkroot()

    space = tolower(space)
    if (space == "user" && (!exists("EMR_UROOT", envir = .GlobalEnv) || is.null(get("EMR_UROOT", envir = .GlobalEnv))))
        stop(sprintf("User space root directory is not set. Please call emr_db.init(user.dir=...)", track), call. = F)

    if (emr_track.exists(track))
        stop(sprintf("Track %s already exists", track), call. = F)

    if (emr_vtrack.exists(track))
        stop(sprintf("Virtual track %s already exists", track), call. = F)

    if (emr_filter.exists(track))
        stop(sprintf("Filter %s already exists", track), call. = F)

	.emr_call("emr_track_create", track, space, categorical, expr, stime, etime, iterator, keepref, .emr_filter(filter), new.env(parent = parent.frame()))
    .emr_db_constants_load()
    retv <- NULL
}

emr_track.exists <- function(track) {
    if (missing(track))
        stop("Usage: emr_track.exist(track)", call. = F)
    .emr_checkroot()
	!is.na(match(track, .emr_call("emr_track_names", new.env(parent = parent.frame()), silent = TRUE)))
}

emr_track.ids <- function(track) {
	if (missing(track))
		stop("Usage: emr_track.ids(track)", call. = F);
    .emr_checkroot()

	.emr_call("emr_track_ids", track, new.env(parent = parent.frame()))
}

emr_track.import <- function(track, space, categorical, src) {
	if (missing(track) || missing(space) || missing(src) || missing(categorical))
		stop("Usage: emr_track.import(track, space, categorical, src)", call. = F)
    .emr_checkroot()

    space = tolower(space)
    if (space == "user" && (!exists("EMR_UROOT", envir = .GlobalEnv) || is.null(get("EMR_UROOT", envir = .GlobalEnv))))
        stop(sprintf("User space root directory is not set. Please call emr_db.init(user.dir=...)", track), call. = F)

    if (emr_vtrack.exists(track))
        stop(sprintf("Virtual track %s already exists", track), call. = F)

    if (emr_filter.exists(track))
        stop(sprintf("Filter %s already exists", track), call. = F)

	.emr_call("emr_import", track, space, categorical, src, F, new.env(parent = parent.frame()))
    .emr_db_constants_load()
    retv <- NULL
}

emr_track.info <- function(track) {
	if (missing(track))
		stop("Usage: emr_track.info(track)", call. = F);
    .emr_checkroot()

	.emr_call("emr_track_info", track, new.env(parent = parent.frame()))
}

emr_track.ls <- function(pattern = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
    .emr_checkroot()
	tracks <- .emr_call("emr_track_names", new.env(parent = parent.frame()), silent = TRUE)
    if (pattern != "")
	    sort(grep(pattern, tracks, value = TRUE, ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes))
    else
        sort(tracks)
}

emr_track.percentile <- function(track, val, lower = T) {
	if (missing(track) || missing(val))
		stop("Usage: emr_track.percentile(track, val, lower)", call. = F)
    .emr_checkroot()
    .emr_call("emr_track_percentile", track, val, lower, new.env(parent = parent.frame()))
}

emr_track.rm <- function(track, force = F) {
	if (missing(track))
		stop("Usage: emr_track.rm(track, force = F)", call. = F)
    .emr_checkroot()

	if (!emr_track.exists(track)) {
		if (force)
			return(invisible())
		stop(sprintf("Track %s does not exist", track), call. = F)
	}

	if (!is.na(match(track, .emr_call("emr_global_track_names", new.env(parent = parent.frame()), silent = TRUE))))
		stop(sprintf("Cannot remove track %s: it is located in the global space.\n", track), call. = F)

	answer <- "N"
	if (force)
		answer <- "Y"
	else {
		str <- sprintf("Are you sure you want to delete track %s (Y/N)? ", track)
		cat(str)
		answer <- toupper(readLines(n = 1))
	}
	
	if (answer == "Y" || answer == "YES") {
    	.emr_call("emr_track_rm", track, new.env(parent = parent.frame()))

        dirname <- .emr_track.var.dir(track)
        if (file.exists(dirname))
            unlink(dirname, recursive = TRUE)

        .emr_db_constants_load()
    }

    retv <- NULL
}

emr_track.global.ls <- function(pattern = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
    .emr_checkroot()
	tracks <- .emr_call("emr_global_track_names", new.env(parent = parent.frame()), silent = TRUE)
	sort(grep(pattern, tracks, value = TRUE, ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes))
}

emr_track.user.ls <- function(pattern = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
    .emr_checkroot()
	tracks <- .emr_call("emr_user_track_names", new.env(parent = parent.frame()), silent = TRUE)
	sort(grep(pattern, tracks, value = TRUE, ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes))
}

emr_track.unique <- function(track) {
	if (missing(track))
		stop("Usage: emr_track.unique(track)", call. = F);
    .emr_checkroot()

	.emr_call("emr_track_unique", track, new.env(parent = parent.frame()))
}

emr_track.var.get <- function(track, var) {
	if (missing(track) || missing(var))
		stop("Usage: emr_track.var.get(track, var)", call. = F)
	.emr_checkroot()

	if (!emr_track.exists(track))
		stop(sprintf("Track %s does not exist", track), call. = F)

    filename <- paste(.emr_track.var.dir(track), var, sep = "/");
	if (!file.exists(filename))
		stop(sprintf("Track variable %s does not exist", var), call. = F)

	f <- file(filename, "rb")
	val <- unserialize(f)
	close(f)
	val
}

emr_track.var.ls <- function(track, pattern = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
	if (missing(track))
		stop("Usage: emr_track.var.ls(track, pattern = \"\", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)", call. = F)
	.emr_checkroot()

	if (!emr_track.exists(track))
		stop(sprintf("Track %s does not exist", track), call. = F)

    dirname <- .emr_track.var.dir(track)

	options(warn = -1) # disable warnings since dir() on non dir or non existing dir produces warnings
	invisible(files <- dir(dirname))
	options(warn = 0) # restore the warning behavior
	if (length(files) > 0 && pattern != "")
		sort(grep(pattern, files, value = TRUE, ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes))
	else
		sort(files)
}

emr_track.var.rm <- function(track, var) {
	if (missing(track) || missing(var))
		stop("Usage: emr_track.var.rm(track, var)", call. = F)
	.emr_checkroot()

	if (!emr_track.exists(track))
		stop(sprintf("Track %s does not exist", track), call. = F)

    dirname <- .emr_track.var.dir(track)
    filename <- paste(dirname, var, sep = "/");
	if (!file.exists(filename))
		stop(sprintf("Track variable %s does not exist", var), call. = F)

	file.remove(filename)

    if (!length(dir(dirname)))
        unlink(dirname, recursive = TRUE)

	retv <- NULL
}

emr_track.var.set <- function(track, var, value) {
	if (missing(track) || missing(var) || missing(value))
		stop("Usage: emr_track.var.set(track, var, value)", call. = F)
	.emr_checkroot()

	if (!emr_track.exists(track))
		stop(sprintf("Track %s does not exist", track), call. = F)

    dirname <- .emr_track.var.dir(track)

	if (!file.exists(dirname))
		dir.create(dirname, mode = "0777")

    filename <- paste(dirname, var, sep = "/");

	# save the variable
	f <- file(filename, "wb")
	serialize(value, f)
	close(f)
}

emr_filter.create <- function(filter, src, keepref = F, time.shift = NULL, val = NULL, expiration = NULL) {
	if (missing(filter) || missing(src))
		stop("Usage: emr_filter.create(filter, src, keepref = F, time.shift = NULL, val = NULL, expiration = NULL)", call. = F)
    .emr_checkroot()

    if (filter != make.names(filter))
        stop(sprintf("\"%s\" is not a syntactically valid name for a variable", filter), call. = F)

    if (!exists("EMR_FILTERS", envir = .GlobalEnv))
        EMR_FILTERS <<- list()

	if (emr_track.exists(filter))
		stop(sprintf("Track %s already exists", filter), call. = F)

	if (emr_vtrack.exists(filter))
		stop(sprintf("Virtual track %s already exists", filter), call. = F)

    if (is.character(src) && length(src) == 1 && !is.na(match(src, .emr_call("emr_user_track_names", new.env(parent = parent.frame()), silent = TRUE))))
        root <- get("EMR_UROOT", envir = .GlobalEnv)
    else
        root <- get("EMR_GROOT", envir = .GlobalEnv)

    old.filter <- EMR_FILTERS[[root]][[filter]]
    var <- list(src = src, time_shift = time.shift, keepref = keepref, val = val, expiration = expiration)
	EMR_FILTERS[[root]][[filter]] <<- var

    success <- F
    tryCatch({
        .emr_call("emr_check_named_filter", var, filter, new.env(parent = parent.frame()))
        success <- T
    },
    finally = {
        if (!success)
            EMR_FILTERS[[root]][[filter]] <<- old.filter
    })

	retv <- NULL
}

emr_filter.attr.src <- function(filter, src) {
	if (missing(filter))
		stop("Usage: emr_filter.attr.src(filter, src)", call. = F)
    .emr_checkroot()

    root <- get("EMR_GROOT", envir = .GlobalEnv)
    filter.var <- get("EMR_FILTERS", envir = .GlobalEnv)[[root]][[filter]]
    if (is.null(filter.var)) {
        root <- get("EMR_UROOT", envir = .GlobalEnv)
        filter.var <- get("EMR_FILTERS", envir = .GlobalEnv)[[root]][[filter]]
    }

    if (is.null(filter.var))
        stop(sprintf("Filter \"%s\" does not exist", filter), call. = F)

    if (missing(src))
        filter.var$src
    else {
        if (!emr_track.exists(src))
            stop(sprintf("Track \"%s\" does not exist", src), call. = F)

        EMR_FILTERS[[root]][[filter]] <<- NULL

        filter.var$src <- src
        if (!is.na(match(src, .emr_call("emr_user_track_names", new.env(parent = parent.frame()), silent = TRUE))))
            root <- get("EMR_UROOT", envir = .GlobalEnv)
        else
            root <- get("EMR_GROOT", envir = .GlobalEnv)
        EMR_FILTERS[[root]][[filter]] <<- filter.var
        retv <- NULL
    }
}

emr_filter.attr.keepref <- function(filter, keepref) {
	if (missing(filter))
		stop("Usage: emr_filter.attr.keepref(filter, keepref)", call. = F)
    .emr_checkroot()

    root <- get("EMR_GROOT", envir = .GlobalEnv)
    filter.var <- get("EMR_FILTERS", envir = .GlobalEnv)[[root]][[filter]]
    if (is.null(filter.var)) {
        root <- get("EMR_UROOT", envir = .GlobalEnv)
        filter.var <- get("EMR_FILTERS", envir = .GlobalEnv)[[root]][[filter]]
    }

    if (is.null(filter.var))
        stop(sprintf("Filter \"%s\" does not exist", filter), call. = F)

    if (missing(keepref))
        filter.var$keepref
    else {
        if (!is.logical(keepref) || is.na(keepref))
            stop("'keepref' parameter must be logical", call. = F)

        EMR_FILTERS[[root]][[filter]]['keepref'] <<- list(keepref)
        retv <- NULL
    }
}

emr_filter.attr.time.shift <- function(filter, time.shift) {
	if (missing(filter))
		stop("Usage: emr_filter.attr.time.shift(filter, time.shift)", call. = F)
    .emr_checkroot()

    root <- get("EMR_GROOT", envir = .GlobalEnv)
    filter.var <- get("EMR_FILTERS", envir = .GlobalEnv)[[root]][[filter]]
    if (is.null(filter.var)) {
        root <- get("EMR_UROOT", envir = .GlobalEnv)
        filter.var <- get("EMR_FILTERS", envir = .GlobalEnv)[[root]][[filter]]
    }

    if (is.null(filter.var))
        stop(sprintf("Filter \"%s\" does not exist", filter), call. = F)

    if (missing(time.shift))
        filter.var$time_shift
    else {
        .emr_call("emr_check_filter_attr_time_shift", time.shift, new.env(parent = parent.frame()))
        EMR_FILTERS[[root]][[filter]]['time_shift'] <<- list(time.shift)
        retv <- NULL
    }
}

emr_filter.attr.val <- function(filter, val) {
	if (missing(filter))
		stop("Usage: emr_filter.attr.val(filter, val)", call. = F)
    .emr_checkroot()

    root <- get("EMR_GROOT", envir = .GlobalEnv)
    filter.var <- get("EMR_FILTERS", envir = .GlobalEnv)[[root]][[filter]]
    if (is.null(filter.var)) {
        root <- get("EMR_UROOT", envir = .GlobalEnv)
        filter.var <- get("EMR_FILTERS", envir = .GlobalEnv)[[root]][[filter]]
    }

    if (is.null(filter.var))
        stop(sprintf("Filter \"%s\" does not exist", filter), call. = F)

    if (missing(val))
        filter.var$val
    else {
        if (!is.numeric(val))
            stop("'val' parameter must be a numeric vector", call. = F)

        EMR_FILTERS[[root]][[filter]]['val'] <<- list(val)
        retv <- NULL
    }
}

emr_filter.attr.expiration <- function(filter, expiration) {
	if (missing(filter))
		stop("Usage: emr_filter.attr.expiration(filter, expiration)", call. = F)
    .emr_checkroot()

    root <- get("EMR_GROOT", envir = .GlobalEnv)
    filter.var <- get("EMR_FILTERS", envir = .GlobalEnv)[[root]][[filter]]
    if (is.null(filter.var)) {
        root <- get("EMR_UROOT", envir = .GlobalEnv)
        filter.var <- get("EMR_FILTERS", envir = .GlobalEnv)[[root]][[filter]]
    }

    if (is.null(filter.var))
        stop(sprintf("Filter \"%s\" does not exist", filter), call. = F)

    if (missing(expiration))
        filter.var$expiration
    else {
        .emr_call("emr_check_filter_attr_expiration", expiration, new.env(parent = parent.frame()))
        EMR_FILTERS[[root]][[filter]]['expiration'] <<- list(expiration)
        retv <- NULL
    }
}

emr_filter.exists <- function(filter) {
    if (missing(filter))
        stop("Usage: emr_filter.exists(filter)", call. = F)
    .emr_checkroot()

    res <- FALSE
    if (exists("EMR_FILTERS", envir = .GlobalEnv)) {
        filters <- get("EMR_FILTERS", envir = .GlobalEnv)
        res <- !is.null(filters[[get("EMR_GROOT", envir = .GlobalEnv)]][[filter]]) ||
               exists("EMR_UROOT", envir = .GlobalEnv) && !is.null(get("EMR_UROOT", envir = .GlobalEnv)) && !is.null(filters[[get("EMR_UROOT", envir = .GlobalEnv)]][[filter]])
    }
	res
}

emr_filter.info <- function(filter) {
	if (missing(filter))
		stop("Usage: emr_filter.info(filter)", call. = F);
    .emr_checkroot()

	.emr_filter.get(filter)
}

emr_filter.ls <- function(pattern = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
    if (!exists("EMR_FILTERS", envir = .GlobalEnv))
        return(NULL)
    .emr_checkroot()

    emr_filters <- get("EMR_FILTERS", envir = .GlobalEnv)
    emr_roots <- names(emr_filters)
	if (!is.list(emr_filters) || (length(emr_filters) && !is.character(emr_roots)) || length(emr_filters) != length(emr_roots))
		stop("Invalid format of EMR_FILTERS variable.\nTo continue working with filters please remove this variable from the environment.", call. = F)

    filternames <- NULL
    roots <- c("EMR_GROOT", "EMR_UROOT")

    for (root in roots) {
        if (exists(root, envir = .GlobalEnv) && !is.null(get(root, envir = .GlobalEnv))) {
            emr_root <- get(root, envir = .GlobalEnv)
            idx <- match(emr_root, emr_roots)
            if (!is.na(idx)) {
                filters <- emr_filters[[idx]]
                filternames <- names(filters)
        	    if (!is.list(filters) || (length(filters) && !is.character(filternames)) || length(filters) != length(filternames))
        		    stop("Invalid format of EMR_FILTERS variable.\nTo continue working with filters please remove this variable from the environment.", call. = F)
            }
        }
    }

	if (is.null(filternames))
		return(NULL)

	if (pattern != "")
		sort(grep(pattern, filternames, value = TRUE, ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes))
	else
		sort(filternames)
}

emr_filter.rm <- function(filter) {
	if (missing(filter))
		stop("Usage: emr_filter.rm(filter)", call. = F);
    .emr_checkroot()

    if (exists("EMR_FILTERS", envir = .GlobalEnv)) {
        emr_filters <- get("EMR_FILTERS", envir = .GlobalEnv)
        emr_filters[[get("EMR_GROOT", envir = .GlobalEnv)]][[filter]] <- NULL

        if (exists("EMR_UROOT", envir = .GlobalEnv) && !is.null(get("EMR_UROOT", envir = .GlobalEnv)))
            emr_filters[[get("EMR_UROOT", envir = .GlobalEnv)]][[filter]] <- NULL

        assign("EMR_FILTERS", emr_filters, envir = .GlobalEnv)
    }

	retv <- NULL
}

emr_vtrack.create <- function(vtrack, src, func = NULL, params = NULL, keepref = F, time.shift = NULL, id.map = NULL, filter = NULL) {
	if (missing(vtrack) || missing(src))
		stop("Usage: emr_vtrack.create(vtrack, src, func = NULL, params = NULL, keepref = F, time.shift = NULL, id.map = NULL, filter = NULL)", call. = F)
    .emr_checkroot()

    if (vtrack != make.names(vtrack))
        stop(sprintf("\"%s\" is not a syntactically valid name for a variable", vtrack), call. = F)

    if (!exists("EMR_VTRACKS", envir = .GlobalEnv))
        EMR_VTRACKS <<- list()

	if (emr_track.exists(vtrack))
		stop(sprintf("Track %s already exists", vtrack), call. = F)

	if (emr_filter.exists(vtrack))
		stop(sprintf("Filter %s already exists", vtrack), call. = F)

    if (!is.na(match(src, .emr_call("emr_user_track_names", new.env(parent = parent.frame()), silent = TRUE))))
        root <- get("EMR_UROOT", envir = .GlobalEnv)
    else
        root <- get("EMR_GROOT", envir = .GlobalEnv)

    old.vtrack <- EMR_VTRACKS[[root]][[vtrack]]
	EMR_VTRACKS[[root]][[vtrack]] <<- list(src = src, time_shift = time.shift, func = func, params = params, keepref = keepref, id_map = id.map, filter = .emr_filter(filter))
	
    success <- F
    tryCatch({
        .emr_call("emr_check_vtrack", vtrack, new.env(parent = parent.frame()))
        success <- T
    },
    finally = {
        if (!success)
            EMR_VTRACKS[[root]][[vtrack]] <<- old.vtrack
    })

	retv <- NULL
}

emr_vtrack.attr.src <- function(vtrack, src) {
	if (missing(vtrack))
		stop("Usage: emr_vtrack.attr.src(vtrack, src)", call. = F)
    .emr_checkroot()

    root <- get("EMR_GROOT", envir = .GlobalEnv)
    vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[root]][[vtrack]]
    if (is.null(vtrack.var)) {
        root <- get("EMR_UROOT", envir = .GlobalEnv)
        vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[root]][[vtrack]]
    }

    if (is.null(vtrack.var))
        stop(sprintf("Virtual track \"%s\" does not exist", vtrack), call. = F)

    if (missing(src))
        vtrack.var$src
    else {
        if (!emr_track.exists(src))
            stop(sprintf("Track \"%s\" does not exist", src), call. = F)

        EMR_VTRACKS[[root]][[vtrack]] <<- NULL

        vtrack.var$src <- src
        if (!is.na(match(src, .emr_call("emr_user_track_names", new.env(parent = parent.frame()), silent = TRUE))))
            root <- get("EMR_UROOT", envir = .GlobalEnv)
        else
            root <- get("EMR_GROOT", envir = .GlobalEnv)
        EMR_VTRACKS[[root]][[vtrack]] <<- vtrack.var
        retv <- NULL
    }
}

emr_vtrack.attr.func <- function(vtrack, func) {
	if (missing(vtrack))
		stop("Usage: emr_vtrack.attr.func(vtrack, func)", call. = F)
    .emr_checkroot()

    root <- get("EMR_GROOT", envir = .GlobalEnv)
    vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[root]][[vtrack]]
    if (is.null(vtrack.var)) {
        root <- get("EMR_UROOT", envir = .GlobalEnv)
        vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[root]][[vtrack]]
    }

    if (is.null(vtrack.var))
        stop(sprintf("Virtual track \"%s\" does not exist", vtrack), call. = F)

    if (missing(func))
        vtrack.var$func
    else {
        .emr_call("emr_check_vtrack_attr_func", func, new.env(parent = parent.frame()))
        EMR_VTRACKS[[root]][[vtrack]]['func'] <<- list(func)
        retv <- NULL
    }
}

emr_vtrack.attr.params <- function(vtrack, params) {
	if (missing(vtrack))
		stop("Usage: emr_vtrack.attr.params(vtrack, params)", call. = F)
    .emr_checkroot()

    root <- get("EMR_GROOT", envir = .GlobalEnv)
    vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[root]][[vtrack]]
    if (is.null(vtrack.var)) {
        root <- get("EMR_UROOT", envir = .GlobalEnv)
        vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[root]][[vtrack]]
    }

    if (is.null(vtrack.var))
        stop(sprintf("Virtual track \"%s\" does not exist", vtrack), call. = F)

    if (missing(params))
        vtrack.var$params
    else {
        EMR_VTRACKS[[root]][[vtrack]]['params'] <<- list(params)
        retv <- NULL
    }
}

emr_vtrack.attr.keepref <- function(vtrack, keepref) {
	if (missing(vtrack))
		stop("Usage: emr_vtrack.attr.keepref(vtrack, keepref)", call. = F)
    .emr_checkroot()

    root <- get("EMR_GROOT", envir = .GlobalEnv)
    vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[root]][[vtrack]]
    if (is.null(vtrack.var)) {
        root <- get("EMR_UROOT", envir = .GlobalEnv)
        vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[root]][[vtrack]]
    }

    if (is.null(vtrack.var))
        stop(sprintf("Virtual track \"%s\" does not exist", vtrack), call. = F)

    if (missing(keepref))
        vtrack.var$keepref
    else {
        if (!is.logical(keepref) || is.na(keepref))
            stop("'keepref' parameter must be logical", call. = F)

        EMR_VTRACKS[[root]][[vtrack]]['keepref'] <<- list(keepref)
        retv <- NULL
    }
}

emr_vtrack.attr.time.shift <- function(vtrack, time.shift) {
	if (missing(vtrack))
		stop("Usage: emr_vtrack.attr.time.shift(vtrack, time.shift)", call. = F)
    .emr_checkroot()

    root <- get("EMR_GROOT", envir = .GlobalEnv)
    vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[root]][[vtrack]]
    if (is.null(vtrack.var)) {
        root <- get("EMR_UROOT", envir = .GlobalEnv)
        vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[root]][[vtrack]]
    }

    if (is.null(vtrack.var))
        stop(sprintf("Virtual track \"%s\" does not exist", vtrack), call. = F)

    if (missing(time.shift))
        vtrack.var$time_shift
    else {
        .emr_call("emr_check_vtrack_attr_time_shift", time.shift, new.env(parent = parent.frame()))
        EMR_VTRACKS[[root]][[vtrack]]['time_shift'] <<- list(time.shift)
        retv <- NULL
    }
}

emr_vtrack.attr.id.map <- function(vtrack, id.map) {
	if (missing(vtrack))
		stop("Usage: emr_vtrack.attr.id.map(vtrack, id.map)", call. = F)
    .emr_checkroot()

    root <- get("EMR_GROOT", envir = .GlobalEnv)
    vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[root]][[vtrack]]
    if (is.null(vtrack.var)) {
        root <- get("EMR_UROOT", envir = .GlobalEnv)
        vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[root]][[vtrack]]
    }

    if (is.null(vtrack.var))
        stop(sprintf("Virtual track \"%s\" does not exist", vtrack), call. = F)

    if (missing(id.map))
        vtrack.var$id.map
    else {
        .emr_call("emr_check_vtrack_attr_id_map", id.map, new.env(parent = parent.frame()))
        EMR_VTRACKS[[root]][[vtrack]]['id.map'] <<- list(id.map)
        retv <- NULL
    }
}

emr_vtrack.attr.filter <- function(vtrack, filter) {
	if (missing(vtrack))
		stop("Usage: emr_vtrack.attr.filter(vtrack, filter)", call. = F)
    .emr_checkroot()

    root <- get("EMR_GROOT", envir = .GlobalEnv)
    vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[root]][[vtrack]]
    if (is.null(vtrack.var)) {
        root <- get("EMR_UROOT", envir = .GlobalEnv)
        vtrack.var <- get("EMR_VTRACKS", envir = .GlobalEnv)[[root]][[vtrack]]
    }

    if (is.null(vtrack.var))
        stop(sprintf("Virtual track \"%s\" does not exist", vtrack), call. = F)

    if (missing(filter))
        vtrack.var$filter
    else {
        .emr_call("emr_check_vtrack_attr_filter", .emr_filter(filter), new.env(parent = parent.frame()))
        EMR_VTRACKS[[root]][[vtrack]]['filter'] <<- list(.emr_filter(filter))
        retv <- NULL
    }
}

emr_vtrack.exists <- function(vtrack) {
    if (missing(vtrack))
        stop("Usage: emr_vtrack.exists(vtrack)", call. = F)
    .emr_checkroot()

    res <- FALSE
    if (exists("EMR_VTRACKS", envir = .GlobalEnv)) {
        vtracks <- get("EMR_VTRACKS", envir = .GlobalEnv)
        res <- !is.null(vtracks[[get("EMR_GROOT", envir = .GlobalEnv)]][[vtrack]]) ||
               exists("EMR_UROOT", envir = .GlobalEnv) && !is.null(get("EMR_UROOT", envir = .GlobalEnv)) && !is.null(vtracks[[get("EMR_UROOT", envir = .GlobalEnv)]][[vtrack]])
    }
	res
}

emr_vtrack.info <- function(vtrack) {
	if (missing(vtrack))
		stop("Usage: emr_vtrack.info(vtrack)", call. = F);
    .emr_checkroot()

	.emr_vtrack.get(vtrack)
}

emr_vtrack.ls <- function(pattern = "", ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
    .emr_checkroot()

    if (!exists("EMR_VTRACKS", envir = .GlobalEnv))
        return(NULL)

    emr_vtracks <- get("EMR_VTRACKS", envir = .GlobalEnv)
    emr_roots <- names(emr_vtracks)
	if (!is.list(emr_vtracks) || (length(emr_vtracks) && !is.character(emr_roots)) || length(emr_vtracks) != length(emr_roots))
		stop("Invalid format of EMR_VTRACKS variable.\nTo continue working with virtual tracks please remove this variable from the environment.", call. = F)

    vtracknames <- NULL
    roots <- c("EMR_GROOT", "EMR_UROOT")

    for (root in roots) {
        if (exists(root, envir = .GlobalEnv) && !is.null(get(root, envir = .GlobalEnv))) {
            emr_root <- get(root, envir = .GlobalEnv)
            idx <- match(emr_root, emr_roots)
            if (!is.na(idx)) {
                vtracks <- emr_vtracks[[idx]]
                vtracknames <- names(vtracks)
            	if (!is.list(vtracks) || (length(vtracks) && !is.character(vtracknames)) || length(vtracks) != length(vtracknames))
            		stop("Invalid format of EMR_VTRACKS variable.\nTo continue working with virtual tracks please remove this variable from the environment.", call. = F)
            }
        }
    }

	if (is.null(vtracknames))
		return(NULL)

	if (pattern != "")
		sort(grep(pattern, vtracknames, value = TRUE, ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes))
	else
		sort(vtracknames)
}

emr_vtrack.rm <- function(vtrack) {
	if (missing(vtrack))
		stop("Usage: emr_vtrack.rm(vtrack)", call. = F);
    .emr_checkroot()

    if (exists("EMR_VTRACKS", envir = .GlobalEnv)) {
        emr_vtracks <- get("EMR_VTRACKS", envir = .GlobalEnv)
        emr_vtracks[[get("EMR_GROOT", envir = .GlobalEnv)]][[vtrack]] <- NULL

        if (exists("EMR_UROOT", envir = .GlobalEnv) && !is.null(get("EMR_UROOT", envir = .GlobalEnv)))
            emr_vtracks[[get("EMR_UROOT", envir = .GlobalEnv)]][[vtrack]] <- NULL

        assign("EMR_VTRACKS", emr_vtracks, envir = .GlobalEnv)
    }
	retv <- NULL
}

emr_cor <- function(..., cor.exprs = NULL, include.lowest = FALSE, right = TRUE, stime = get("MINTIME", envir = .GlobalEnv), etime = get("MAXTIME", envir = .GlobalEnv), iterator = NULL, keepref = F, filter = NULL) {
	args <- list(...)
	if (length(args) < 2 || (length(args) %% 2 != 0 && (length(args) - 1) %% 2 != 0) || is.null(cor.exprs))
		stop("Usage: emr_cor([factor.expr, breaks]+, cor.exprs, include.lowest = F, right = T, stime = MINTIME, etime = MAXTIME, iterator = NULL, keepref = F, filter = NULL)", call. = F)
    .emr_checkroot()

	exprs <- c()
	breaks <- list()

	for (i in (0 : (length(args) / 2 - 1))) {
		exprs <- append(exprs, args[[i * 2 + 1]])
	    breaks[length(breaks) + 1] <- list(args[[i * 2 + 2]])
	}

    exprs <- append(exprs, cor.exprs)

	res <- .emr_call("emr_covariance", exprs, breaks, include.lowest, right, stime, etime, iterator, keepref, .emr_filter(filter), new.env(parent = parent.frame()))
	res
}

emr_dist <- function(..., include.lowest = FALSE, right = TRUE, stime = get("MINTIME", envir = .GlobalEnv), etime = get("MAXTIME", envir = .GlobalEnv), iterator = NULL, keepref = FALSE, filter = NULL) {
	args <- list(...)
	if (length(args) < 2 || (length(args) %% 2 != 0 && (length(args) - 1) %% 2 != 0))
		stop("Usage: emr_dist([expr, breaks]+, include.lowest = F, right = T, stime = MINTIME, etime = MAXTIME, iterator = NULL, keepref = F, filter = NULL)", call. = F)
    .emr_checkroot()

	exprs <- c()
	breaks <- list()

	for (i in (0 : (length(args) / 2 - 1))) {
		exprs <- append(exprs, args[[i * 2 + 1]])
	    breaks[length(breaks) + 1] <- list(args[[i * 2 + 2]])
	}

	res <- .emr_call("emr_dist", exprs, breaks, include.lowest, right, stime, etime, iterator, keepref, .emr_filter(filter), new.env(parent = parent.frame()))
	res
}

emr_extract <- function(expr, tidy = F, sort = F, names = NULL, stime = get("MINTIME", envir = .GlobalEnv), etime = get("MAXTIME", envir = .GlobalEnv), iterator = NULL, keepref = F, filter = NULL) {
    if (missing(expr))
        stop("Usage: emr_extract(expr, tidy = F, sort = F, names = NULL, tidy = F, stime = MINTIME, etime = MAXTIME, iterator = NULL, keepref = F, filter = NULL)", call. = F)
    .emr_checkroot()

    .emr_call("emr_extract", expr, names, tidy, sort, stime, etime, iterator, keepref, .emr_filter(filter), new.env(parent = parent.frame()))
}

emr_ids_coverage <- function(ids, tracks, stime = get("MINTIME", envir = .GlobalEnv), etime = get("MAXTIME", envir = .GlobalEnv), filter = NULL) {
	if (missing(ids) || missing(tracks))
		stop("Usage: emr_ids_coverage(ids, tracks, stime = MINTIME, etime = MAXTIME, filter = NULL)", call. = F)
    .emr_checkroot()

    if (stime == get("MINTIME", envir = .GlobalEnv) && etime == get("MAXTIME", envir = .GlobalEnv) && is.null(filter))
	    .emr_call("emr_ids_dist", ids, tracks, new.env(parent = parent.frame()))
    else {
    	if (is.null(filter))
    	    filter <- "..emr_tmp_filter"
    	else
     	    filter <- paste0("(", filter, ")", "& ..emr_tmp_filter")

        if (is.character(ids))   # ids is a name of the track
            assign("..emr_tmp_filter", emr_track.ids(ids), envir = .GlobalEnv)
        else
            assign("..emr_tmp_filter", data.frame(id = unique(ids$id)), envir = .GlobalEnv)
    
    	tryCatch({
            .emr_call("emr_ids_dist_with_iterator", ids, tracks, stime, etime, .emr_filter(filter), new.env(parent = parent.frame()))
    	},
    	finally = {
            rm("..emr_tmp_filter", envir = .GlobalEnv)
        })
    }
}

emr_ids_vals_coverage <- function(ids, tracks, stime = get("MINTIME", envir = .GlobalEnv), etime = get("MAXTIME", envir = .GlobalEnv), filter = NULL) {
	if (missing(ids) || missing(tracks))
		stop("Usage: emr_ids_vals_coverage(ids, tracks, stime = MINTIME, etime = MAXTIME, filter = NULL)", call. = F)
    .emr_checkroot()

	if (is.null(filter))
	    filter <- "..emr_tmp_filter"
	else
 	    filter <- paste0("(", filter, ")", "& ..emr_tmp_filter")

    if (is.character(ids))   # ids is a name of the track
        assign("..emr_tmp_filter", emr_track.ids(ids), envir = .GlobalEnv)
    else
        assign("..emr_tmp_filter", data.frame(id = unique(ids$id)), envir = .GlobalEnv)
    	
	tryCatch({
	    .emr_call("emr_ids_vals_dist", ids, tracks, stime, etime, .emr_filter(filter), new.env(parent = parent.frame()))
	},
	finally = {
        rm("..emr_tmp_filter", envir = .GlobalEnv)
    })
}

emr_quantiles <- function(expr, percentiles = 0.5, stime = get("MINTIME", envir = .GlobalEnv), etime = get("MAXTIME", envir = .GlobalEnv), iterator = NULL, keepref = F, filter = NULL) {
	if (missing(expr))
		stop("Usage: emr_quantiles(expr, percentiles = 0.5, stime = MINTIME, etime = MAXTIME, iterator = NULL, keepref = F, filter = NULL)", call. = F)
    .emr_checkroot()

	.emr_call("emr_quantiles", expr, percentiles, stime, etime, iterator, keepref, .emr_filter(filter), new.env(parent = parent.frame()))
}

emr_screen <- function(expr, sort = F, stime = get("MINTIME", envir = .GlobalEnv), etime = get("MAXTIME", envir = .GlobalEnv), iterator = NULL, keepref = F, filter = NULL) {
    if (missing(expr))
        stop("Usage: emr_screen(expr, sort = F, stime = MINTIME, etime = MAXTIME, iterator = NULL, keepref = F, filter = NULL)", call. = F)
    .emr_checkroot()

    .emr_call("emr_screen", expr, sort, stime, etime, iterator, keepref, .emr_filter(filter), new.env(parent = parent.frame()))
}

emr_summary <- function(expr, stime = get("MINTIME", envir = .GlobalEnv), etime = get("MAXTIME", envir = .GlobalEnv), iterator = NULL, keepref = F, filter = NULL) {
    if (missing(expr))
        stop("Usage: emr_summary(expr, stime = MINTIME, etime = MAXTIME, iterator = NULL, keepref = F, filter = NULL)", call. = F)
    .emr_checkroot()

    .emr_call("emr_summary", expr, stime, etime, iterator, keepref, .emr_filter(filter), new.env(parent = parent.frame()))
}

emr_time2hour <- function(time) {
    if (missing(time))
        stop("Usage: emr_time2hour(time)", call. = F)

    .emr_call("emr_time2hour", time, new.env(parent = parent.frame()))
}

emr_time2dayofmonth <- function(time) {
    if (missing(time))
        stop("Usage: emr_time2dayofmonth(time)", call. = F)

    .emr_call("emr_time2dayofmonth", time, new.env(parent = parent.frame()))
}

emr_time2month <- function(time) {
    if (missing(time))
        stop("Usage: emr_time2month(time)", call. = F)

    .emr_call("emr_time2month", time, new.env(parent = parent.frame()))
}

emr_time2year <- function(time) {
    if (missing(time))
        stop("Usage: emr_time2year(time)", call. = F)

    .emr_call("emr_time2year", time, new.env(parent = parent.frame()))
}

emr_date2time <- function(day, month, year, hour = 0) {
    if (missing(day) || missing(month) || missing(year))
        stop("Usage: emr_date2time(day, month, year, hour = 0)", call. = F)

    .emr_call("emr_date2time", data.frame(hour, day, month, year), new.env(parent = parent.frame()))
}

emr_annotate <- function(x, y) {
    if (missing(x) || missing(y))
        stop("Usage: emr_annotate(x, y)", call. = F)

    .emr_call("emr_annotate", x, y, new.env(parent = parent.frame()))
}

emr_traceback <- function(x = NULL, max.lines = getOption("deparse.max.lines")) {
	x <- NULL

    if (is.null(x) && (exists(".Traceback", envir = baseenv()))) 
        x <- get(".Traceback", envir = baseenv())
	
	if (!is.null(x) && length(x) > 0) {
		# get the call stack and concatenate all complex commands together
		x <- sapply(x, paste, collapse="")

		# extract call stack function names
		fnames <- gsub("^(\\S+)\\s*\\(.*\\)$", "\\1", x, perl = T)

		# get the indices of lib functions
		libindices <- which(fnames %in% get(".EMR_FUNCS", envir = .GlobalEnv))

		# cut whatever comes after the first lib function
		if (length(libindices) > 0) {
			x <- get(".Traceback")[libindices[length(libindices)] : length(get(".Traceback"))]
		}
	}

	traceback(x, max.lines)
}

emr_test_pipe <- function(num_processes = 1, duration = 5) {
    .emr_call("emr_test_pipe", num_processes, duration, new.env(parent = parent.frame()))
}

