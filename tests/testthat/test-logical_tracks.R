load_test_db()

clean_logical_tracks()

test_that("emr_track.logical.create tracks works", {
    withr::defer(clean_logical_tracks())

    emr_track.logical.create("logical_track", "ph1", c(15, 16))
    logical_track_ok("logical_track", "ph1", c(15, 16))

    emr_track.logical.create("logical_track_numeric", "track0")
    logical_track_ok("logical_track_numeric", "track0")

    expect_false(emr_track.logical.exists("track1"))
})

test_that("emr_track.logical.exists works", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track1", "ph1", c(15, 16))
    emr_track.logical.create("logical_track2", "ph1", c(15, 16))
    expect_equal(emr_track.logical.exists(emr_track.logical.ls()), c(TRUE, TRUE))
})

test_that("emr_track.logical.create tracks works in batch mode", {
    withr::defer(clean_logical_tracks())
    tracks <- c("logical_track1", "logical_track2", "logical_track3")
    sources <- c(rep("ph1", 2), "physical_track_subset_15")
    values <- list(
        c(11, 12),
        16:19,
        15
    )

    emr_track.logical.create(tracks, sources, values)

    purrr::pwalk(list(tracks, sources, values), function(tr, sr, v) {
        logical_track_ok(tr, sr, v)
    })
})

test_that("emr_track.logical.create tracks works in batch mode when some ltracks have NULL values", {
    withr::defer(clean_logical_tracks())
    tracks <- c("logical_track1", "logical_track2", "logical_track3")
    sources <- c(rep("ph1", 2), "physical_track_subset_15")
    values <- list(
        c(11, 12),
        NULL,
        15
    )

    emr_track.logical.create(tracks, sources, values)

    purrr::pwalk(list(tracks, sources, values), function(tr, sr, v) {
        logical_track_ok(tr, sr, v)
    })
})

test_that("emr_track.logical.create tracks works in batch mode when some ltracks do not have values", {
    withr::defer(clean_logical_tracks())
    tracks <- c("logical_track1", "logical_track2", "logical_track3")
    sources <- c(rep("ph1", 2), "physical_track_subset_15")
    values <- list(
        c(11, 12),
        c(),
        15
    )

    emr_track.logical.create(tracks, sources, values)

    purrr::pwalk(list(tracks, sources, values), function(tr, sr, v) {
        logical_track_ok(tr, sr, v)
    })
})

test_that("emr_track.logical.create tracks works in batch mode length of values list is 1", {
    withr::defer(clean_logical_tracks())
    tracks <- c("logical_track1")
    sources <- c("ph1")
    values <- list(
        c(11, 12)
    )

    emr_track.logical.create("logical_track1", "ph1", values)
    logical_track_ok("logical_track1", "ph1", c(11, 12))
})



test_that("emr_track.logical.create fails when track length do not equal names length", {
    expect_error(emr_track.logical.create(c("a", "b"), c("ph1")))
})

test_that("emr_track.logical.create fails when track length do not equal values length", {
    expect_error(emr_track.logical.create(c("a", "b"), c("ph1", "ph1"), values = list(c(15, 16))))
})

test_that("emr_track.logical.create fails when values is not a list", {
    expect_error(emr_track.logical.create(c("a", "b"), c("ph1", "ph1"), values = c(15, 16)))
})

test_that("emr_track.logical.create fails when there are duplicated tracks", {
    expect_error(emr_track.logical.create(c("a", "a"), c("ph1", "ph1")))
})

test_that("emr_track.logical.create tracks works with integer values", {
    withr::defer(clean_logical_tracks())

    emr_track.logical.create("logical_track", "ph1", as.integer(c(15, 16)))
    logical_track_ok("logical_track", "ph1", c(15, 16))

    expect_false(emr_track.logical.exists("track1"))
})

test_that("emr_track.logical.create tracks works without values", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track1", "ph1")
    logical_track_ok("logical_track1", "ph1")
})

test_that("emr_track.logical.create tracks fails with illegal values", {
    withr::defer(clean_logical_tracks())
    expect_error(emr_track.logical.create("logical_track", "ph1", "savta"))
})

test_that("emr_track.logical.create tracks fails with existing tracks", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track", "ph1", c(15, 16))
    emr_track.logical.create("logical_track_numeric", "track0")

    expect_error(emr_track.logical.create("logical_track", "ph1", c(15, 16)))
    expect_error(emr_track.logical.create("logical_track", "track0"))
    expect_error(emr_track.logical.create("logical_track_numeric", "ph1", c(15, 16)))
    expect_error(emr_track.logical.create("track1", "ph1", c(15, 16)))
})

test_that("emr_track.logical.create tracks fails with non-categorical tracks and values", {
    expect_error(emr_track.logical.create("logical_track", "track2", c(15, 16)))
})

test_that("emr_track.logical.create tracks fails with illegal track names", {
    expect_error(emr_track.logical.create(".logical_track", "ph1", c(15, 16)))
    expect_error(emr_track.logical.create(".logical_track", "track0"))
})

test_that("emr_track.logical.info returns correct value", {
    withr::defer(clean_logical_tracks())

    emr_track.logical.create("logical_track", "ph1", c(15, 16))
    res <- emr_track.logical.info("logical_track")

    expect_equal(names(res), c("source", "values"))
    expect_equal(res$source, "ph1")
    expect_equal(res$values, c(15, 16))

    emr_track.logical.create("logical_track_numeric", "track0")
    res <- emr_track.logical.info("logical_track_numeric")

    expect_equal(names(res), c("source", "values"))
    expect_equal(res$source, "track0")
    expect_equal(res$values, NULL)

    expect_error(emr_track.logical.info("track1"))
    expect_error(emr_track.logical.info("blahblah"))
})

test_that("emr_track.logical.rm works ", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track_test", "ph1", c(15, 16))
    emr_track.logical.create("logical_track_test_numeric", "track0")

    logical_track_ok("logical_track_test", "ph1", c(15, 16))
    logical_track_ok("logical_track_test_numeric", "track0")

    emr_track.logical.rm("logical_track_test", force = TRUE)
    emr_track.logical.rm("logical_track_test_numeric", force = TRUE)

    expect_false("logical_track_test" %in% emr_track.logical.ls())
    expect_false("logical_track_test_numeric" %in% emr_track.logical.ls())
    expect_false("logical_track_test" %in% emr_track.ls())
    expect_false("logical_track_test_numeric" %in% emr_track.ls())
    expect_false("logical_track_test" %in% emr_track.global.ls())
    expect_false("logical_track_test_numeric" %in% emr_track.global.ls())

    expect_false(file.exists(logical_track_path("logical_track_test")))
    expect_false(file.exists(logical_track_path("logical_track_test_numeric")))

    expect_false(emr_track.exists("logical_track_test"))
    expect_false(emr_track.exists("logical_track_test_numeric"))

    expect_error(emr_extract("logical_track_test"))
    expect_error(emr_extract("logical_track_test_numeric"))

    expect_error(emr_track.logical.info("logical_track_test"))
    expect_error(emr_track.logical.info("logical_track_test_numeric"))
})

test_that("emr_track.logical.rm works in batch mode", {
    withr::defer(clean_logical_tracks())
    ltracks <- paste0("ltrack", 1:10)
    purrr::walk(ltracks, emr_track.logical.create, "ph1", c(15, 16))

    emr_track.logical.rm(ltracks, force = TRUE)
    purrr::walk(ltracks, ~ {
        expect_false(.x %in% emr_track.logical.ls())
        expect_false(.x %in% emr_track.ls())
        expect_false(.x %in% emr_track.global.ls())
        expect_false(file.exists(logical_track_path(.x)))
        expect_false(emr_track.exists(.x))
        expect_error(emr_extract(.x))
        expect_error(emr_track.logical.info(.x))
    })
})

test_that("emr_track.logical.rm fails when track doesn't exist ", {
    expect_error(emr_track.logical.rm("blahblahblah"))
})

test_that("emr_track.logical.rm fails when track is physical", {
    expect_error(emr_track.logical.rm("ph1"))
})

# test multiple processes
test_that("logical tracks creation persists between R sessions", {
    skip_on_cran()
    skip_on_ci()
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track_test1", "ph1", c(15, 16))
    emr_track.logical.create("logical_track_test2", "ph1")
    emr_track.logical.create("logical_track_test_numeric", "track0")
    res <- callr::r(
        function(root) {
            devtools::load_all()
            emr_db.connect(db_dirs = root)
            return(emr_track.logical.ls())
        },
        args = list(root = .naryn$EMR_GROOT)
    )
    expect_equal(res, c("logical_track_test_numeric", "logical_track_test1", "logical_track_test2"))
})

test_that("logical tracks creation persists between R sessions for existing sessions", {
    skip_on_cran()
    skip_on_ci()
    withr::defer(clean_logical_tracks())
    callr::r(
        function(root) {
            devtools::load_all()
            emr_db.connect(db_dirs = root)
            emr_track.logical.create("logical_track_test1", "ph1", c(15, 16))
            emr_track.logical.create("logical_track_test2", "ph1")
            emr_track.logical.create("logical_track_test_numeric", "track0")
        },
        args = list(root = .naryn$EMR_GROOT)
    )
    expect_equal(emr_track.logical.ls(), c("logical_track_test1", "logical_track_test2", "logical_track_test_numeric"))
    a <- emr_extract("logical_track_test2", keepref = TRUE)
    b <- emr_extract("ph1", names = c("logical_track_test2"), keepref = TRUE)
    c <- emr_extract("logical_track_test_numeric", names = c("val"), keepref = TRUE)
    d <- emr_extract("track0", names = c("val"), keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)
})

test_that("logical tracks creation persists between R sessions", {
    skip_on_cran()
    skip_on_ci()
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track_test1", "ph1", c(15, 16))
    emr_track.logical.create("logical_track_test_numeric1", "track0")
    emr_track.logical.create("logical_track_test2", "ph1")
    emr_track.logical.create("logical_track_test_numeric2", "track0")
    emr_track.logical.rm("logical_track_test1", force = TRUE)
    emr_track.logical.rm("logical_track_test_numeric1", force = TRUE)
    res <- callr::r(
        function(root) {
            devtools::load_all()
            emr_db.connect(db_dirs = root)
            return(emr_track.logical.ls())
        },
        args = list(root = .naryn$EMR_GROOT)
    )
    expect_equal(res, c("logical_track_test_numeric2", "logical_track_test2"))
})

test_that("logical tracks deletion persists between R sessions for existing sessions", {
    skip_on_cran()
    skip_on_ci()
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track_test1", "ph1", c(15, 16))
    emr_track.logical.create("logical_track_test2", "ph1")
    res <- callr::r(
        function(root) {
            devtools::load_all()
            emr_db.connect(db_dirs = root)
            emr_track.logical.rm("logical_track_test1", force = TRUE)
        },
        args = list(root = .naryn$EMR_GROOT)
    )
    expect_equal(emr_track.logical.ls(), "logical_track_test2")
})

test_that("removing a logical tracks file and running emr_db.reload() updates the db", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track_test1", "ph1", c(15, 16))
    emr_track.logical.create("logical_track_test_numeric", "track0")
    file.remove(logical_track_path("logical_track_test1"))
    file.remove(logical_track_path("logical_track_test_numeric"))
    expect_false(file.exists(logical_track_path("logical_track_test1")))
    expect_false(file.exists(logical_track_path("logical_track_test_numeric")))
    emr_db.reload()
    expect_false(file.exists(logical_track_path("logical_track_test1")))
    expect_false(file.exists(logical_track_path("logical_track_test_numeric")))
    expect_false("logical_track_test1" %in% emr_track.logical.ls())
    expect_false("logical_track_test_numeric" %in% emr_track.logical.ls())
    expect_false("logical_track_test1" %in% emr_track.ls())
    expect_false("logical_track_test_numeric" %in% emr_track.ls())
    expect_false("logical_track_test1" %in% emr_track.global.ls())
    expect_false("logical_track_test_numeric" %in% emr_track.global.ls())
    expect_false(file.exists(logical_track_path("logical_track_test1")))
    expect_false(file.exists(logical_track_path("logical_track_test_numeric")))
    expect_false(emr_track.exists("logical_track_test1"))
    expect_false(emr_track.exists("logical_track_test_numeric"))
    expect_error(emr_extract("logical_track_test1"))
    expect_error(emr_extract("logical_track_test_numeric"))
    expect_error(emr_track.logical.info("logical_track_test1"))
    expect_error(emr_track.logical.info("logical_track_test_numeric"))
})

test_that("adding a logical tracks file and running emr_db.reload() updates the db", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track_test1", "ph1", c(15, 16))
    temp_file <- tempfile()
    file.copy(from = logical_track_path("logical_track_test1"), to = temp_file)
    emr_track.rm("logical_track_test1", force = TRUE)
    expect_false(emr_track.exists("logical_track_test1"))
    expect_false(file.exists(logical_track_path("logical_track_test1")))
    file.copy(temp_file, logical_track_path("logical_track_test1"))
    emr_db.reload()
    logical_track_ok("logical_track_test1", "ph1", c(15, 16))
})

test_that("emr_track.logical.ls works", {
    withr::defer(clean_logical_tracks())
    for (i in 1:10) {
        emr_track.logical.create(paste0("logical_track", i), "ph1", c(15, 16))
    }
    for (i in 11:20) {
        emr_track.logical.create(paste0("logical_track", i), "ph1")
    }
    for (i in 1:10) {
        emr_track.logical.create(paste0("logical_track_numeric", i), "track0")
    }

    expect_setequal(emr_track.logical.ls(), c(paste0("logical_track", 1:20), paste0("logical_track_numeric", 1:10)))
    expect_true(all(paste0("logical_track", 1:20) %in% emr_track.ls()))
    expect_true(all(paste0("logical_track_numeric", 1:10) %in% emr_track.ls()))
    expect_true(all(paste0("logical_track", 1:20) %in% emr_track.global.ls()))
    expect_true(all(paste0("logical_track_numeric", 1:10) %in% emr_track.global.ls()))
})

# test implicit vtrack creation
test_that("logical track returns a valid vtrack R object without values", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track1", "ph1")
    emr_track.logical.create("logical_track_numeric", "track0")
    res <- .emr_call("logical_track_vtrack", "logical_track1", .emr_env(), silent = TRUE)
    res_numeric <- .emr_call("logical_track_vtrack", "logical_track_numeric", .emr_env(), silent = TRUE)
    emr_vtrack.create("vt", "ph1", keepref = TRUE)
    emr_vtrack.create("vt_numeric", "track0", keepref = TRUE)
    # commented because logical field was added to local R object
    # vt <- .naryn$EMR_VTRACKS$vt
    vt <- emr_vtrack.info("vt")
    vt_numeric <- emr_vtrack.info("vt_numeric")
    expect_equal(vt, res)
    expect_equal(vt_numeric, res_numeric)
    withr::defer(emr_vtrack.rm("vt"))
    withr::defer(emr_vtrack.rm("vt_numeric"))
})

test_that("logical track returns a valid vtrack R object with values", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track1", "ph1", c(15, 16))
    res <- .emr_call("logical_track_vtrack", "logical_track1", .emr_env(), silent = TRUE)
    emr_vtrack.create("vt", "ph1", params = c(15, 16), keepref = TRUE)
    # commented because logical field was added to local R object
    # vt <- .naryn$EMR_VTRACKS$vt
    vt <- emr_vtrack.info("vt")
    expect_equal(vt, res)

    emr_track.logical.create("logical_track_numeric", "track0")
    res <- .emr_call("logical_track_vtrack", "logical_track_numeric", .emr_env(), silent = TRUE)
    emr_vtrack.create("vt", "track0", keepref = TRUE)
    vt <- emr_vtrack.info("vt")
    expect_equal(vt, res)

    withr::defer(emr_vtrack.rm("vt"))
})

# track.readonly
test_that("emr_track.readonly works for logical tracks", {
    emr_track.logical.create("logical_track1", "ph1", c(15, 16))
    expect_false(emr_track.readonly("logical_track1"))

    emr_track.readonly("logical_track1", TRUE)
    expect_true(emr_track.readonly("logical_track1"))
    expect_error(emr_track.rm("logical_track1"), "Cannot remove track logical_track1: it is read-only.")
    expect_true(emr_track.exists("logical_track1"))

    emr_track.readonly("logical_track1", FALSE)
    emr_track.rm("logical_track1", TRUE)
    expect_false(emr_track.exists("logical_track1"))
})

test_that("emr_track.readonly works on logical tracks", {
    withr::defer(clean_logical_tracks())

    emr_filter.create("f1", src = "ph1", val = seq(4, 16, 1), keepref = TRUE)
    df <- emr_extract("ph1", names = c("value"), keepref = TRUE, filter = "f1")
    emr_track.import("l1_ph", space = "global", categorical = TRUE, src = df)

    emr_track.logical.create("l1", "l1_ph", seq(4, 16, 1))
    emr_track.readonly("l1", TRUE)

    expect_true(emr_track.readonly("l1"))
    expect_false(emr_track.readonly("l1_ph"))

    expect_error(emr_track.var.set("l1", "a", TRUE))
    expect_error(emr_track.rm("l1", force = TRUE))
    expect_error(emr_track.addto("l1", data.frame(id = 6, time = 6, value = 6), force = TRUE))

    withr::defer(emr_track.rm("l1_ph", force = TRUE))
})

# addto
test_that("emr_track.addto works with logical tracks", {
    withr::defer(clean_logical_tracks())
    a <- emr_extract("ph1", keepref = TRUE, names = "value")
    a1 <- a[1:250000, ]
    a2 <- a[250001:500001, ]
    emr_track.import("temp_track", "global", categorical = TRUE, src = a1)
    withr::defer(emr_track.rm("temp_track", force = TRUE))
    emr_track.logical.create("logical_track1", "temp_track", 15)

    fn <- tempfile()
    readr::write_tsv(a2, fn, col_names = FALSE)
    expect_error(emr_track.addto("logical_track1", fn), "Cannot add to a logical track when src is a file name. Please load the file to a data frame and rerun emr_track.addto with src as the data frame.")

    expect_error(emr_track.addto("logical_track1", a2))

    emr_track.addto("logical_track1", a2 %>% dplyr::filter(value == 15), force = TRUE)


    b <- emr_extract("temp_track", keepref = TRUE, names = "value")
    expect_equal(
        b,
        dplyr::bind_rows(
            a1,
            a2 %>% dplyr::filter(value == 15)
        ) %>% dplyr::arrange(id, time, ref, value)
    )
})

# percentile
test_that("emr_track.percentile fails on logical tracks with values", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track1", "ph1", c(15, 16))
    expect_error(emr_track.percentile("logical_track1"))
})

test_that("emr_track.percentile fails on logical tracks with categorical source", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track1", "ph1")
    expect_error(emr_track.percentile("logical_track1"))
})

test_that("emr_track.percentile works on numerical logical tracks", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track1", "track0")
    expect_equal(emr_track.percentile("logical_track1", 0.5), emr_track.percentile("track0", 0.5))
})

# attributes

# track.mv
test_that("emr_track.mv works with logical tracks", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track1", "ph1", c(15, 16))
    emr_track.mv("logical_track1", "logical_track2")
    expect_false(emr_track.exists("logical_track1"))
    expect_true(emr_track.exists("logical_track2"))
    expect_false("logical_track1" %in% emr_track.ls())
    expect_true("logical_track2" %in% emr_track.ls())
})

test_that("emr_track.mv fails when trying to move to user space", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track1", "ph1", c(15, 16))
    expect_error(emr_track.mv("logical_track1", "logical_track2", "user"))
})

test_that("emr_track.mv fails when track exists", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track1", "ph1", c(15, 16))
    emr_track.logical.create("logical_track2", "ph1", c(15, 16))
    expect_error(emr_track.mv("logical_track1", "logical_track2"))
    expect_error(emr_track.mv("logical_track1", "track1"))
    emr_filter.create("f1", "ph1")
    withr::defer(emr_filter.rm("f1"))
    expect_error(emr_track.mv("logical_track1", "f1"))

    emr_vtrack.create("vt1", "ph1")
    withr::defer(emr_vtrack.rm("vt1"))
    expect_error(emr_track.mv("logical_track1", "vt1"))
})


# emr_extract
test_that("emr_extract works with logical track as expression and implicit iterator", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track", "ph1")
    emr_track.logical.create("logical_track_numeric", "track0")

    a <- emr_extract("logical_track", keepref = TRUE)
    b <- emr_extract("ph1", names = "logical_track", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_extract("logical_track*2", names = "logical_track", keepref = TRUE)
    b <- emr_extract("ph1*2", names = "logical_track", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_extract("logical_track_numeric*2", names = "logical_track", keepref = TRUE)
    b <- emr_extract("track0*2", names = "logical_track", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)
})

test_that("emr_extract works with logical track with values as expression and implicit iterator", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track", "ph1", c(15, 16))
    a <- emr_extract("logical_track", keepref = TRUE)
    emr_filter.create("ltrack_filter", src = "ph1", val = c(15, 16), keepref = TRUE)
    withr::defer(emr_filter.rm("ltrack_filter"))
    b <- emr_extract("ph1", filter = "ltrack_filter", names = "logical_track", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_extract("logical_track*2", names = "logical_track", keepref = TRUE)
    b <- emr_extract("ph1*2", filter = "ltrack_filter", names = "logical_track", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)
})

test_that("emr_extract works with logical track as expression and explicit iterator", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track", "ph1")
    emr_track.logical.create("logical_track_numeric", "track0")

    a <- emr_extract("logical_track", iterator = "logical_track", keepref = TRUE)
    b <- emr_extract("ph1", iterator = "logical_track", names = "logical_track", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_extract("logical_track*2", iterator = "logical_track", names = "logical_track", keepref = TRUE)
    b <- emr_extract("ph1*2", iterator = "ph1", names = "logical_track", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_extract("logical_track_numeric*2", iterator = "logical_track_numeric", names = "logical_track_numeric", keepref = TRUE)
    b <- emr_extract("track0*2", iterator = "track0", , names = "logical_track_numeric", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)
})

test_that("emr_extract works with logical track with values as expression and explicit iterator", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track", "ph1", c(15, 16))
    a <- emr_extract("logical_track", iterator = "logical_track", keepref = TRUE)
    emr_filter.create("ltrack_filter", src = "ph1", val = c(15, 16), keepref = TRUE)
    withr::defer(emr_filter.rm("ltrack_filter"))
    b <- emr_extract("ph1", iterator = "ph1", filter = "ltrack_filter", names = "logical_track", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)
})

test_that("emr_extract works with logical track and multiple expressions", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track1", "ph1", c(15, 16))
    emr_track.logical.create("logical_track2", "ph1", c(13, 14))
    a <- emr_extract(c("logical_track1", "track1"), iterator = "logical_track1", keepref = TRUE)
    emr_filter.create("ltrack_filter", src = "ph1", val = c(15, 16), keepref = TRUE)
    withr::defer(emr_filter.rm("ltrack_filter"))
    b <- emr_extract(c("ph1", "track1"), iterator = "ph1", filter = "ltrack_filter", names = c("logical_track1", "track1"), keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)
})

test_that("emr_extract works with numeric logical track and multiple expressions", {
    withr::defer(clean_logical_tracks())

    emr_track.logical.create("logical_track1", "track0")

    a <- emr_extract(c("logical_track1", "track0"), iterator = "logical_track1", keepref = TRUE)
    b <- emr_extract(c("track0", "track0"), iterator = "track0", names = c("logical_track1", "track0"), keepref = TRUE)

    expect_equal(a, b, ignore_attr = TRUE)
})


test_that("emr_extract with keepref=FALSE works", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("l15", "ph1", 15)
    a <- emr_extract("l15", keepref = FALSE)
    expect_true(!any(a$l15 == -1))
    all_data <- emr_extract("ph1", keepref = TRUE, names = "ph1")
    ptd_with_multiple_values <- all_data %>%
        dplyr::filter(ref != 0, ph1 == 15) %>%
        dplyr::select(id, time)
    susp_values <- a %>%
        dplyr::inner_join(ptd_with_multiple_values, by = c("id", "time")) %>%
        dplyr::pull(l15)
    expect_true(all(susp_values == 15))
})

# emr_cor
test_that("emr_cor works with logical track as expression", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track1", "ph1", c(15, 16))
    emr_track.logical.create("logical_track2", "ph1")
    emr_filter.create("ltrack_filter", src = "ph1", val = c(15, 16), keepref = TRUE)
    withr::defer(emr_filter.rm("ltrack_filter"))

    a <- emr_cor("logical_track1", c(14, 15, 16), cor.exprs = c("track0", "track1", "track2"), dataframe = TRUE, keepref = TRUE, iterator = "logical_track1")
    b <- emr_cor("ph1", c(14, 15, 16), cor.exprs = c("track0", "track1", "track2"), dataframe = TRUE, keepref = TRUE, names = "logical_track", iterator = "ph1")
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_cor("logical_track1", c(14, 15, 16), cor.exprs = c("logical_track1", "logical_track2"), dataframe = TRUE, keepref = TRUE, iterator = "logical_track1")
    b <- emr_cor("ph1", c(14, 15, 16), cor.exprs = c("logical_track1", "logical_track2"), dataframe = TRUE, keepref = TRUE, names = "logical_track", iterator = "ph1")
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_cor("logical_track2", c(14, 15, 16), cor.exprs = c("track0", "track1", "track2"), dataframe = TRUE, keepref = TRUE, iterator = "logical_track2")
    b <- emr_cor("ph1", c(14, 15, 16), cor.exprs = c("track0", "track1", "track2"), dataframe = TRUE, keepref = TRUE, names = "logical_track", iterator = "ph1")
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_cor("track2", c(100, 300, 500, 900, 2000, 3000), "logical_track1", c(14, 15, 16), "logical_track2",
        c(11:16),
        cor.exprs = c("track0", "track1", "track2"), dataframe = TRUE, keepref = TRUE, iterator = "ph1"
    )
    b <- emr_cor("track2", c(100, 300, 500, 900, 2000, 3000), "ph1", c(14, 15, 16), "ph1",
        c(11:16),
        cor.exprs = c("track0", "track1", "track2"), dataframe = TRUE, keepref = TRUE, iterator = "ph1", names = c("track2", "logical_track1", "logical_track2")
    )
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_cor("track2", c(100, 300, 500, 900, 2000, 3000), "logical_track1", c(14, 15, 16), "logical_track2",
        c(11:16),
        cor.exprs = c("track0", "track1", "track2"), dataframe = TRUE, keepref = TRUE, iterator = "logical_track1"
    )
    b <- emr_cor("track2", c(100, 300, 500, 900, 2000, 3000), "ph1", c(14, 15, 16), "ph1",
        c(11:16),
        cor.exprs = c("track0", "track1", "track2"), dataframe = TRUE, keepref = TRUE, iterator = "logical_track1", names = c("track2", "logical_track1", "logical_track2")
    )
    expect_equal(a, b, ignore_attr = TRUE)
})

test_that("emr_cor works with logical track as iterator", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track1", "ph1", c(15, 16))
    emr_filter.create("ltrack_filter", src = "ph1", val = c(15, 16), keepref = TRUE)
    withr::defer(emr_filter.rm("ltrack_filter"))

    a <- emr_cor("ph1", c(14, 15, 16), cor.exprs = c("track0", "track1", "track2"), iterator = "logical_track1", dataframe = TRUE, keepref = TRUE)
    b <- emr_cor("ph1", c(14, 15, 16), filter = "ltrack_filter", cor.exprs = c("track0", "track1", "track2"), dataframe = TRUE, keepref = TRUE, names = "logical_track", iterator = "ph1")
    expect_equal(a, b, ignore_attr = TRUE)

    emr_track.logical.create("logical_track2", "ph1")
    a <- emr_cor("ph1", c(14, 15, 16), cor.exprs = c("track0", "track1", "track2"), iterator = "logical_track1", dataframe = TRUE, keepref = TRUE)
    b <- emr_cor("ph1", c(14, 15, 16), cor.exprs = c("track0", "track1", "track2"), dataframe = TRUE, keepref = TRUE, iterator = "ph1")
    expect_equal(a, b, ignore_attr = TRUE)
})

test_that("emr_cor works with logical track as expression and iterator", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track1", "ph1", c(15, 16))
    emr_filter.create("ltrack_filter", src = "ph1", val = c(15, 16), keepref = TRUE)
    withr::defer(emr_filter.rm("ltrack_filter"))

    a <- emr_cor("logical_track1", c(14, 15, 16), cor.exprs = c("track0", "track1", "track2"), iterator = "logical_track1", dataframe = TRUE, keepref = TRUE)
    b <- emr_cor("ph1", c(14, 15, 16), cor.exprs = c("track0", "track1", "track2"), filter = "ltrack_filter", dataframe = TRUE, keepref = TRUE, names = "logical_track", iterator = "ph1")
    expect_equal(a, b, ignore_attr = TRUE)

    emr_track.logical.create("logical_track2", "ph1")
    a <- emr_cor("logical_track2", c(14, 15, 16), cor.exprs = c("track0", "track1", "track2"), iterator = "logical_track1", dataframe = TRUE, keepref = TRUE)
    b <- emr_cor("ph1", c(14, 15, 16), cor.exprs = c("track0", "track1", "track2"), dataframe = TRUE, keepref = TRUE, names = "logical_track", iterator = "ph1")
    expect_equal(a, b, ignore_attr = TRUE)
})

test_that("emr_cor works with logical track as cor.expr", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track1", "ph1", c(15, 16))
    emr_track.logical.create("logical_track2", "ph1")
    emr_filter.create("ltrack_filter", src = "ph1", val = c(15, 16), keepref = TRUE)
    withr::defer(emr_filter.rm("ltrack_filter"))
    a <- emr_cor("track0", c(0, 10, 500, 1000), cor.exprs = c("logical_track1"), dataframe = TRUE, keepref = TRUE, iterator = "track0")
    b <- emr_cor("track0", c(0, 10, 500, 1000), cor.exprs = c("ph1"), filter = "ltrack_filter", dataframe = TRUE, keepref = TRUE, iterator = "track0")
    expect_equal(a %>% select(-i, -j), b %>% select(-i, -j), ignore_attr = TRUE)
})

# emr_dist

test_that("emr_dist works with logical track as expression", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track1", "ph1", c(15, 16))
    a <- emr_dist("logical_track1", c(14, 15, 16), dataframe = TRUE, keepref = TRUE)
    b <- emr_dist("ph1", c(14, 15, 16), dataframe = TRUE, keepref = TRUE, names = "logical_track")
    expect_equal(a, b, ignore_attr = TRUE)

    emr_track.logical.create("logical_track2", "ph1")
    a <- emr_dist("logical_track2", c(14, 15, 16), dataframe = TRUE, keepref = TRUE)
    b <- emr_dist("ph1", c(14, 15, 16), dataframe = TRUE, keepref = TRUE, names = "logical_track")
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_dist("track2", c(100, 300, 500, 900, 2000, 3000), "logical_track1", c(14, 15, 16), "logical_track2",
        c(11:16),
        dataframe = TRUE, keepref = TRUE, iterator = "ph1"
    )
    b <- emr_dist("track2", c(100, 300, 500, 900, 2000, 3000), "ph1", c(14, 15, 16), "ph1",
        c(11:16),
        dataframe = TRUE, keepref = TRUE, iterator = "ph1", names = c("track2", "logical_track1", "logical_track2")
    )
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_dist("track2", c(100, 300, 500, 900, 2000, 3000), "logical_track1", c(14, 15, 16), "logical_track2",
        c(11:16),
        dataframe = TRUE, keepref = TRUE, iterator = "logical_track1"
    )
    b <- emr_dist("track2", c(100, 300, 500, 900, 2000, 3000), "ph1", c(14, 15, 16), "ph1",
        c(11:16),
        dataframe = TRUE, keepref = TRUE, iterator = "logical_track1", names = c("track2", "logical_track1", "logical_track2")
    )
    expect_equal(a, b, ignore_attr = TRUE)
})

test_that("emr_dist works with logical track as iterator", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track1", "ph1", c(15, 16))
    a <- emr_dist("ph1", c(14, 15, 16), iterator = "logical_track1", dataframe = TRUE, keepref = TRUE)
    b <- emr_dist("ph1", c(14, 15, 16), dataframe = TRUE, keepref = TRUE, names = "logical_track")
    expect_equal(a, b, ignore_attr = TRUE)

    emr_track.logical.create("logical_track2", "ph1")
    a <- emr_dist("ph1", c(14, 15, 16), iterator = "logical_track1", dataframe = TRUE, keepref = TRUE)
    b <- emr_dist("ph1", c(14, 15, 16), dataframe = TRUE, keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)
})

test_that("emr_dist works with logical track as expression and iterator", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track1", "ph1", c(15, 16))
    a <- emr_dist("logical_track1", c(14, 15, 16), iterator = "logical_track1", dataframe = TRUE, keepref = TRUE)
    b <- emr_dist("ph1", c(14, 15, 16), dataframe = TRUE, keepref = TRUE, names = "logical_track")
    expect_equal(a, b, ignore_attr = TRUE)

    emr_track.logical.create("logical_track2", "ph1")
    a <- emr_dist("logical_track2", c(14, 15, 16), iterator = "logical_track1", dataframe = TRUE, keepref = TRUE)
    b <- emr_dist("ph1", c(14, 15, 16), dataframe = TRUE, keepref = TRUE, names = "logical_track")
    expect_equal(a, b, ignore_attr = TRUE)
})

test_that("emr_dist works with numeric logical track", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track1", "track0")
    a <- emr_dist("logical_track1", c(300, 605, 825), iterator = "logical_track1", dataframe = TRUE, keepref = TRUE)
    b <- emr_dist("track0", c(300, 605, 825), dataframe = TRUE, keepref = TRUE, names = "logical_track1")
    expect_equal(a, b)
})

# emr_screen
test_that("emr_screen works with logical track as expression", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track1", "ph1", c(15, 16))
    a <- emr_screen("logical_track1 == 15", keepref = TRUE)
    b <- emr_screen("ph1 == 15", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a1 <- emr_screen("logical_track1*2 == 30", keepref = TRUE)
    b1 <- emr_screen("ph1*2 == 30", keepref = TRUE)
    expect_equal(a1, b1, ignore_attr = TRUE)
    expect_equal(a1, a, ignore_attr = TRUE)
    expect_equal(b1, b, ignore_attr = TRUE)

    emr_track.logical.create("logical_track2", "ph1")
    a <- emr_screen("logical_track2 == 13", keepref = TRUE)
    b <- emr_screen("ph1 == 13", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_screen("logical_track1 == 15 & ph1 == 13", iterator = "ph1", keepref = TRUE)
    b <- emr_screen("ph1 == 15 & ph1 == 13", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_screen("logical_track1 == 15 & logical_track1 == 16", keepref = TRUE)
    b <- emr_screen("ph1 == 15 & ph1 == 16", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_screen("logical_track1 == 15 & logical_track2 == 16", iterator = "ph1", keepref = TRUE)
    b <- emr_screen("ph1 == 15 & ph1 == 16", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)
})

test_that("emr_screen works with logical track as iterator", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track1", "ph1", c(15, 16))
    a <- emr_screen("ph1 == 15", iterator = "logical_track1", keepref = TRUE)
    b <- emr_screen("ph1 == 15", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    emr_track.logical.create("logical_track2", "ph1")
    a <- emr_screen("ph1 == 13", iterator = "logical_track2", keepref = TRUE)
    b <- emr_screen("ph1 == 13", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_screen("ph1 == 15 & ph1 == 13", iterator = "logical_track1", keepref = TRUE)
    b <- emr_screen("ph1 == 15 & ph1 == 13", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)
})

test_that("emr_screen works with logical track as expression and iterator", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track1", "ph1", c(15, 16))
    a <- emr_screen("logical_track1 == 15", iterator = "logical_track1", keepref = TRUE)
    b <- emr_screen("ph1 == 15", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    emr_track.logical.create("logical_track2", "ph1")
    a <- emr_screen("logical_track2 == 13", iterator = "logical_track2", keepref = TRUE)
    b <- emr_screen("ph1 == 13", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_screen("logical_track1 == 15 & ph1 == 13", iterator = "logical_track1", keepref = TRUE)
    b <- emr_screen("ph1 == 15 & ph1 == 13", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)
})

test_that("emr_screen works with numeric logical track as expression and iterator", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track1", "track0")
    a <- emr_screen("logical_track1 == 300", iterator = "logical_track1", keepref = TRUE)
    b <- emr_screen("track0 == 300", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_screen("logical_track1 == 300 & track0 == 630", iterator = "logical_track1", keepref = TRUE)
    b <- emr_screen("track0 == 300 & track0 == 630", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)
})

test_that("emr_screen works with logical tracks when expr doesn't return data", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track1", "ph1", c(15, 16))
    expect_equal(
        emr_screen("logical_track1 == 11"),
        structure(list(id = integer(0), time = integer(0), ref = integer(0)), class = "data.frame", row.names = integer(0))
    )
})

# emr_quantiles
test_that("emr_quantiles works with logical track", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track1", "ph1", c(15, 16))
    emr_track.logical.create("logical_track2", "ph1")

    a <- emr_quantiles("logical_track1", c(0.1, 0.2, 0.5, 0.9), keepref = TRUE)
    emr_filter.create("ltrack_filter", src = "ph1", val = c(15, 16), keepref = TRUE)
    withr::defer(emr_filter.rm("ltrack_filter"))

    b <- emr_quantiles("ph1", c(0.1, 0.2, 0.5, 0.9), filter = "ltrack_filter", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_quantiles("logical_track1*2", c(0.1, 0.2, 0.5, 0.9), keepref = TRUE)
    b <- emr_quantiles("ph1*2", c(0.1, 0.2, 0.5, 0.9), filter = "ltrack_filter", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_quantiles("logical_track2", c(0.1, 0.2, 0.5, 0.9), keepref = TRUE)
    b <- emr_quantiles("ph1", c(0.1, 0.2, 0.5, 0.9), keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    # as both expression and iterator
    a <- emr_quantiles("logical_track1", c(0.1, 0.2, 0.5, 0.9), iterator = "logical_track1", keepref = TRUE)
    b <- emr_quantiles("ph1", c(0.1, 0.2, 0.5, 0.9), filter = "ltrack_filter", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_quantiles("logical_track1*2", c(0.1, 0.2, 0.5, 0.9), iterator = "logical_track1", keepref = TRUE)
    b <- emr_quantiles("ph1*2", c(0.1, 0.2, 0.5, 0.9), filter = "ltrack_filter", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_quantiles("logical_track2", c(0.1, 0.2, 0.5, 0.9), iterator = "logical_track1", keepref = TRUE)
    b <- emr_quantiles("ph1", c(0.1, 0.2, 0.5, 0.9), filter = "ltrack_filter", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    # as iterator
    a <- emr_quantiles("ph1*2", c(0.1, 0.2, 0.5, 0.9), iterator = "logical_track1", keepref = TRUE)
    b <- emr_quantiles("ph1*2", c(0.1, 0.2, 0.5, 0.9), filter = "ltrack_filter", , keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_quantiles("ph1", c(0.1, 0.2, 0.5, 0.9), iterator = "logical_track2", keepref = TRUE)
    b <- emr_quantiles("ph1", c(0.1, 0.2, 0.5, 0.9), keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)
})

# emr_summary
test_that("emr_summary works with logical track", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("logical_track1", "ph1", c(15, 16))
    emr_track.logical.create("logical_track2", "ph1")

    a <- emr_summary("logical_track1", keepref = TRUE)
    emr_filter.create("ltrack_filter", src = "ph1", val = c(15, 16), keepref = TRUE)
    withr::defer(emr_filter.rm("ltrack_filter"))

    b <- emr_summary("ph1", filter = "ltrack_filter", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_summary("logical_track1*2", keepref = TRUE)
    b <- emr_summary("ph1*2", filter = "ltrack_filter", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_summary("logical_track2", keepref = TRUE)
    b <- emr_summary("ph1", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    # as both expression and iterator
    a <- emr_summary("logical_track1", iterator = "logical_track1", keepref = TRUE)
    b <- emr_summary("ph1", filter = "ltrack_filter", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_summary("logical_track1*2", iterator = "logical_track1", keepref = TRUE)
    b <- emr_summary("ph1*2", filter = "ltrack_filter", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_summary("logical_track2", iterator = "logical_track1", keepref = TRUE)
    b <- emr_summary("ph1", filter = "ltrack_filter", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    # as iterator
    a <- emr_summary("ph1*2", iterator = "logical_track1", keepref = TRUE)
    b <- emr_summary("ph1*2", filter = "ltrack_filter", , keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_summary("ph1", iterator = "logical_track2", keepref = TRUE)
    b <- emr_summary("ph1", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)
})


# emr_track.unique
test_that("emr_track.unique works on logical tracks", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("l", "ph1")
    emr_track.logical.create("logical_track1", "ph1", c(15, 16))
    expect_equal(emr_track.unique("logical_track1"), c(15, 16))

    emr_track.logical.create("logical_track2", "ph1")
    expect_equal(emr_track.unique("logical_track2"), emr_track.unique("ph1"))

    expect_equal(emr_track.unique("l"), emr_track.unique("ph1"))
})

# ids_coverage
test_that("emr_ids_coverage works", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("l", "ph1")
    emr_track.logical.create("l15", "ph1", 15)
    emr_track.logical.create("l18", "ph1", 18)

    df <- emr_extract("ph1", keepref = TRUE)
    emr_track.addto("patients.dob", src = data.frame(id = 2510, time = 0, ref = -1, value = 0))
    emr_track.import("p15", space = "global", categorical = TRUE, src = df %>% dplyr::filter(ph1 == 15) %>% dplyr::select(id, time, ref, value = ph1))
    emr_track.import("p18", space = "global", categorical = TRUE, src = df %>% dplyr::filter(ph1 == 18) %>% dplyr::select(id, time, ref, value = ph1))
    withr::defer(emr_track.rm("p15", force = TRUE))
    withr::defer(emr_track.rm("p18", force = TRUE))

    set.seed(60427)
    ids <- data.frame(id = sample(df$id, 300))

    a <- emr_ids_coverage(ids, c("l"))
    b <- emr_ids_coverage(ids, c("ph1"))
    names(b) <- c("l")
    expect_equal(a, b)

    a <- emr_ids_coverage(ids, c("l15", "track4"))
    expect_equal(names(a), c("l15", "track4"))
    names(a) <- c("p15", "track4")
    b <- emr_ids_coverage(ids, c("p15", "track4"))
    expect_equal(a, b)

    a <- emr_ids_coverage(ids, c("l15"))
    expect_equal(names(a), c("l15"))
    names(a) <- c("p15")
    b <- emr_ids_coverage(ids, c("p15"))
    expect_equal(a, b)

    a <- emr_ids_coverage(ids, c("l15", "track4"), filter = "track2")
    expect_equal(names(a), c("l15", "track4"))
    names(a) <- c("p15", "track4")
    b <- emr_ids_coverage(ids, c("p15", "track4"), filter = "track2")
    expect_equal(a, b)

    a <- emr_ids_coverage(ids, c("l15", "track4"), filter = "l15")
    expect_equal(names(a), c("l15", "track4"))
    names(a) <- c("p15", "track4")
    b <- emr_ids_coverage(ids, c("p15", "track4"), filter = "l15")
    expect_equal(a, b)

    a <- emr_ids_coverage(ids, c("l15", "track4"), filter = "l15")
    expect_equal(names(a), c("l15", "track4"))
    names(a) <- c("p15", "track4")
    b <- emr_ids_coverage(ids, c("p15", "track4"), filter = "p15")
    expect_equal(a, b)

    a <- emr_ids_coverage(ids, c("l15", "l18", "track4"), filter = "l15")
    expect_equal(names(a), c("l15", "l18", "track4"))
    names(a) <- c("p15", "p18", "track4")
    b <- emr_ids_coverage(ids, c("p15", "p18", "track4"), filter = "p15")
    expect_equal(a, b)

    # test when ids is a name of a (logical) track
    a <- emr_ids_coverage("l15", c("l15", "l18", "track4"), filter = "l15")
    expect_equal(names(a), c("l15", "l18", "track4"))
    names(a) <- c("p15", "p18", "track4")
    b <- emr_ids_coverage("p15", c("p15", "p18", "track4"), filter = "p15")
    expect_equal(a, b)

    a <- emr_ids_coverage("l15", c("l15", "l18", "track4"), filter = "l15")
    expect_equal(names(a), c("l15", "l18", "track4"))
    names(a) <- c("p15", "p18", "track4")
    b <- emr_ids_coverage("l15", c("p15", "p18", "track4"), filter = "p15")
    expect_equal(a, b)

    a <- emr_ids_coverage("p18", c("l15", "l18", "track4"), filter = "l15")
    expect_equal(names(a), c("l15", "l18", "track4"))
    names(a) <- c("p15", "p18", "track4")
    b <- emr_ids_coverage("p18", c("p15", "p18", "track4"), filter = "p15")
    expect_equal(a, b)
})

test_that("emr_ids_vals_coverage works", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("l", "ph1")
    emr_track.logical.create("l15", "ph1", 15)
    emr_track.logical.create("l18", "ph1", 18)

    df <- emr_extract("ph1", keepref = TRUE)
    emr_track.addto("patients.dob", src = data.frame(id = 2510, time = 0, ref = -1, value = 0))
    emr_track.import("p15", space = "global", categorical = TRUE, src = df %>% dplyr::filter(ph1 == 15) %>% dplyr::select(id, time, ref, value = ph1))
    emr_track.import("p18", space = "global", categorical = TRUE, src = df %>% dplyr::filter(ph1 == 18) %>% dplyr::select(id, time, ref, value = ph1))
    withr::defer(emr_track.rm("p15", force = TRUE))
    withr::defer(emr_track.rm("p18", force = TRUE))

    set.seed(60427)
    ids <- data.frame(id = sample(df$id, 300))

    a <- emr_ids_vals_coverage(ids, c("l"))
    b <- emr_ids_vals_coverage(ids, c("ph1")) %>%
        mutate(track = forcats::fct_recode(track, l = "ph1"))
    expect_equal(a, b)

    a <- emr_ids_vals_coverage(ids, c("l15", "track7"))
    b <- emr_ids_vals_coverage(ids, c("p15", "track7")) %>%
        mutate(track = forcats::fct_recode(track, l15 = "p15"))
    expect_equal(a, b)

    a <- emr_ids_vals_coverage(ids, c("l15"))
    b <- emr_ids_vals_coverage(ids, c("p15")) %>%
        mutate(track = forcats::fct_recode(track, l15 = "p15"))
    expect_equal(a, b)

    a <- emr_ids_vals_coverage(ids, c("l15", "track7"), filter = "track2")
    b <- emr_ids_vals_coverage(ids, c("p15", "track7"), filter = "track2") %>%
        mutate(track = forcats::fct_recode(track, l15 = "p15"))
    expect_equal(a, b)

    a <- emr_ids_vals_coverage(ids, c("l15", "track7"), filter = "l15")
    b <- emr_ids_vals_coverage(ids, c("p15", "track7"), filter = "l15") %>%
        mutate(track = forcats::fct_recode(track, l15 = "p15"))
    expect_equal(a, b)

    a <- emr_ids_vals_coverage(ids, c("l15", "track7"), filter = "l15")
    b <- emr_ids_vals_coverage(ids, c("p15", "track7"), filter = "p15") %>%
        mutate(track = forcats::fct_recode(track, l15 = "p15"))
    expect_equal(a, b)

    a <- emr_ids_vals_coverage(ids, c("l15", "l18", "track7"), filter = "l15")
    b <- emr_ids_vals_coverage(ids, c("p15", "p18", "track7"), filter = "p15") %>%
        mutate(track = forcats::fct_recode(track, l15 = "p15", l18 = "p18"))
    expect_equal(a, b)


    # test when ids is a name of a (logical) track
    a <- emr_ids_vals_coverage("l15", c("l15", "l18", "track7"), filter = "l15")
    b <- emr_ids_vals_coverage("p15", c("p15", "p18", "track7"), filter = "p15") %>%
        mutate(track = forcats::fct_recode(track, l15 = "p15", l18 = "p18"))
    expect_equal(a, b)

    a <- emr_ids_vals_coverage("l15", c("l15", "l18", "track7"), filter = "l15")
    b <- emr_ids_vals_coverage("l15", c("p15", "p18", "track7"), filter = "p15") %>%
        mutate(track = forcats::fct_recode(track, l15 = "p15", l18 = "p18"))
    expect_equal(a, b)

    a <- emr_ids_vals_coverage("p18", c("l15", "l18", "track7"), filter = "l15")
    b <- emr_ids_vals_coverage("p18", c("p15", "p18", "track7"), filter = "p15") %>%
        mutate(track = forcats::fct_recode(track, l15 = "p15", l18 = "p18"))
    expect_equal(a, b)
})

# track_ids
test_that("emr_track.ids works on logical tracks", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("l15", "ph1", 15)
    emr_track.logical.create("l18", "ph1", 18)

    emr_track.addto("patients.dob", src = data.frame(id = 2510, time = 0, ref = -1, value = 0))
    df <- emr_extract("ph1", keepref = TRUE)
    emr_track.import("p15", space = "global", categorical = TRUE, src = df %>% dplyr::filter(ph1 == 15) %>% dplyr::select(id, time, ref, value = ph1))
    emr_track.import("p18", space = "global", categorical = TRUE, src = df %>% dplyr::filter(ph1 == 18) %>% dplyr::select(id, time, ref, value = ph1))
    withr::defer(emr_track.rm("p15", force = TRUE))
    withr::defer(emr_track.rm("p18", force = TRUE))

    a <- emr_track.ids("p15")
    b <- emr_track.ids("l15")
    expect_equal(a, b)

    a1 <- emr_track.ids("p18")
    b1 <- emr_track.ids("l18")
    expect_equal(a1, b1)
})

test_that("emr_track.ids works on logical track with no values", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("l", "ph1")
    a1 <- emr_track.ids("ph1")
    b1 <- emr_track.ids("l")
    expect_equal(a1, b1)
})

test_that("emr_track.ids works on logical tracks with dense source", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("l9", "track8", 9)
    emr_track.logical.create("l2", "track8", 2)

    emr_track.addto("patients.dob", src = data.frame(id = 2510, time = 0, ref = -1, value = 0))
    df <- emr_extract("track8", keepref = TRUE)
    emr_track.import("p9", space = "global", categorical = TRUE, src = df %>% dplyr::filter(track8 == 9) %>% dplyr::select(id, time, ref, value = track8))
    emr_track.import("p2", space = "global", categorical = TRUE, src = df %>% dplyr::filter(track8 == 2) %>% dplyr::select(id, time, ref, value = track8))
    withr::defer(emr_track.rm("p9", force = TRUE))
    withr::defer(emr_track.rm("p2", force = TRUE))

    a <- emr_track.ids("p9")
    b <- emr_track.ids("l9")
    expect_equal(a, b)

    a1 <- emr_track.ids("p2")
    b1 <- emr_track.ids("l2")
    expect_equal(a1, b1)
})

test_that("emr_track.ids works on logical tracks with sparse source", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("l9", "track8_sparse", 9)
    emr_track.logical.create("l2", "track8_sparse", 2)

    df <- emr_extract("track8_sparse", keepref = TRUE)
    emr_track.import("p9", space = "global", categorical = TRUE, src = df %>% dplyr::filter(track8_sparse == 9) %>% dplyr::select(id, time, ref, value = track8_sparse))
    emr_track.import("p2", space = "global", categorical = TRUE, src = df %>% dplyr::filter(track8_sparse == 2) %>% dplyr::select(id, time, ref, value = track8_sparse))
    withr::defer(emr_track.rm("p9", force = TRUE))
    withr::defer(emr_track.rm("p2", force = TRUE))

    a <- emr_track.ids("p9")
    b <- emr_track.ids("l9")
    expect_equal(a, b)

    a1 <- emr_track.ids("p2")
    b1 <- emr_track.ids("l2")
    expect_equal(a1, b1)
})

# db_subset
test_that("emr_db.subset works with logical track", {
    set.seed(60427)
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("l9", "track8_sparse", 9)
    emr_track.logical.create("l2", "track8_sparse", 2)

    df <- emr_extract("track8_sparse", keepref = TRUE)
    emr_track.import("p9", space = "global", categorical = TRUE, src = df %>% dplyr::filter(track8_sparse == 9) %>% dplyr::select(id, time, ref, value = track8_sparse))
    emr_track.import("p2", space = "global", categorical = TRUE, src = df %>% dplyr::filter(track8_sparse == 2) %>% dplyr::select(id, time, ref, value = track8_sparse))
    withr::defer(emr_track.rm("p9", force = TRUE))
    withr::defer(emr_track.rm("p2", force = TRUE))

    emr_db.subset("l9", fraction = 0.8, complementary = FALSE)
    withr::defer(emr_db.subset(NULL))
    ids <- emr_db.subset.ids()
    expect_equal(nrow(ids), 800)
    a <- emr_extract("l9")
    expect_true(all(a$id %in% ids$id))
    a <- emr_extract("track8_sparse")
    expect_true(all(a$id %in% ids$id))

    expect_equal(
        emr_db.subset.info(),
        list(src = "l9", fraction = 0.8, complementary = FALSE)
    )
})

test_that("emr_track.unique ignores current subset with logical tracks", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("l9", "track8_sparse", 9)
    a <- emr_track.unique("l9")
    emr_db.subset(data.frame(id = 2510), fraction = 1, complementary = FALSE)
    withr::defer(emr_db.subset(NULL))
    b <- emr_track.unique("l9")
    expect_equal(a, b)
})

test_that("emr_track.ids ignores current subset with logical tracks", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("l9", "track8_sparse", 9)
    a <- emr_track.ids("l9")
    emr_db.subset(data.frame(id = 2510), fraction = 1, complementary = FALSE)
    withr::defer(emr_db.subset(NULL))
    b <- emr_track.ids("l9")
    expect_equal(a, b)
})

test_that("emr_track.info ignores current subset with logical tracks", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("l9", "track8_sparse", 9)
    a <- emr_track.info("l9")
    emr_db.subset(data.frame(id = 2510), fraction = 1, complementary = FALSE)
    withr::defer(emr_db.subset(NULL))
    b <- emr_track.info("l9")
    expect_equal(a, b)
})

# filters

test_that("logical track can be used as filter", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("l15", "ph1", 15)
    emr_track.logical.create("l16", "ph1", 16)

    df <- emr_extract("ph1", keepref = TRUE)
    emr_track.import("p15", space = "global", categorical = TRUE, src = df %>% dplyr::filter(ph1 == 15) %>% dplyr::select(id, time, ref, value = ph1))
    emr_track.import("p16", space = "global", categorical = TRUE, src = df %>% dplyr::filter(ph1 == 16) %>% dplyr::select(id, time, ref, value = ph1))
    withr::defer(emr_track.rm("p15", force = TRUE))
    withr::defer(emr_track.rm("p16", force = TRUE))

    a <- emr_extract("ph1", filter = "p15 & p16", keepref = TRUE)
    b <- emr_extract("ph1", filter = "l15 & l16", keepref = TRUE)
    expect_equal(a, b)

    a <- emr_extract("ph1", filter = "p15", keepref = TRUE)
    b <- emr_extract("ph1", filter = "l15", keepref = TRUE)
    expect_equal(a, b)

    a <- emr_extract("ph1", filter = "p15 | p16", keepref = TRUE)
    b <- emr_extract("ph1", filter = "p15 | p16", keepref = TRUE)
    expect_equal(a, b)

    a <- emr_extract("ph1", filter = "p15 & p16", keepref = TRUE)
    b <- emr_extract("ph1", filter = "p15 & l16", keepref = TRUE)
    expect_equal(a, b)

    a <- emr_extract("ph1", filter = "p15 | p16", keepref = TRUE)
    b <- emr_extract("ph1", filter = "p15 | l16", keepref = TRUE)
    expect_equal(a, b)

    a <- emr_extract("l15", filter = "l15", keepref = TRUE)
    b <- emr_extract("p15", filter = "l15", keepref = TRUE, names = "l15")
    expect_equal(a, b)

    a <- emr_extract("l15", filter = "l15", keepref = TRUE)
    b <- emr_extract("p15", filter = "p15", keepref = TRUE, names = "l15")
    expect_equal(a, b)

    a <- emr_extract("l15", filter = "l16", keepref = TRUE)
    b <- emr_extract("p15", filter = "l16", keepref = TRUE, names = "l15")
    expect_equal(a, b)

    a <- emr_extract("l15", filter = "l16", keepref = TRUE)
    b <- emr_extract("p15", filter = "p16", keepref = TRUE, names = "l15")
    expect_equal(a, b)

    a <- emr_extract("l15", filter = "l15 & l16", keepref = TRUE)
    b <- emr_extract("p15", filter = "l15 & l16", keepref = TRUE, names = "l15")
    expect_equal(a, b)
})

test_that("numeric logical track can be used as filter", {
    withr::defer(clean_logical_tracks())

    emr_track.logical.create("ltrack", "track0")
    withr::defer(emr_track.rm("ltrack", force = TRUE))

    a <- emr_extract("track0", filter = "ltrack", keepref = TRUE)
    b <- emr_extract("track0", keepref = TRUE)
    expect_equal(a, b)
})

# emr_filter.create
test_that("emr_filter.create works as expected", {
    emr_filter.clear()
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("ltrack", "ph1", c(15, 16))

    emr_filter.create("f1", src = "ltrack", val = c(14, 15), keepref = TRUE)
    filter_info <- emr_filter.info("f1")
    expect_equal(filter_info$val, c(14, 15))

    emr_filter.create("f2", src = "ltrack", keepref = TRUE)
    filter_info <- emr_filter.info("f2")
    expect_null(filter_info$val)

    emr_filter.create("f3", src = "ltrack", val = c(17), keepref = TRUE)
    filter_info <- emr_filter.info("f3")
    expect_equal(filter_info$val, c(17))
})

test_that("emr_filter.create works when logical tracks are without values", {
    emr_filter.clear()
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("ltrack", "ph1")

    emr_filter.create("f1", src = "ltrack", keepref = TRUE)
    emr_filter.create("p1", src = "ph1", keepref = TRUE)
    filter_info <- emr_filter.info("f1")
    expect_null(filter_info$val)

    a <- emr_extract("ph1", filter = "f1", keepref = TRUE)
    b <- emr_extract("ph1", filter = "p1", keepref = TRUE)
    expect_equal(a, b)
})


test_that("emr_filter.create works on logical track", {
    emr_filter.clear()
    withr::defer(clean_logical_tracks())

    emr_track.logical.create("ltrack", "ph1", c(15, 16))

    emr_filter.create("f1", src = "ltrack", val = c(15), keepref = TRUE)
    t1 <- emr_extract("ltrack", names = c("vals"), keepref = TRUE) %>%
        dplyr::filter(vals == 15) %>%
        dplyr::select(-ref)
    t2 <- emr_extract("ltrack", names = c("vals"), filter = "f1", keepref = TRUE) %>% dplyr::select(-ref)
    expect_equal(t1, t2)

    emr_filter.create("f2", src = "ltrack", keepref = TRUE)
    t1 <- emr_extract("ltrack", names = c("vals"), keepref = TRUE)
    t2 <- emr_extract("ltrack", names = c("vals"), filter = "f2", keepref = TRUE)
    expect_equal(t1, t2)

    emr_filter.create("f3", src = "ltrack", val = c(17), keepref = TRUE)
    t1 <- emr_extract("ltrack", names = c("vals"), keepref = TRUE) %>%
        dplyr::filter(vals == 17) %>%
        dplyr::select(-ref)
    t2 <- emr_extract("ltrack", names = c("vals"), filter = "f3", keepref = TRUE) %>% dplyr::select(-ref)
    expect_equal(t1, t2)
})

test_that("empty emr_filter.create works on logical track", {
    emr_filter.clear()
    withr::defer(clean_logical_tracks())

    emr_track.logical.create("ltrack", "ph1", c(15, 16))

    emr_filter.create("f_ltrack", src = "ltrack")

    t1 <- emr_extract("ltrack", names = c("vals"), filter = "f_ltrack", keepref = TRUE) %>% dplyr::select(-ref)
    t2 <- emr_extract("ltrack", names = c("vals"), keepref = TRUE) %>% dplyr::select(-ref)

    expect_equal(t1, t2)
})

test_that("multiple emr_filter.create works on logical track", {
    emr_filter.clear()
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("ltrack", "ph1", seq(1, 16, 1))

    emr_filter.create("f1", src = "ltrack", val = c(14), keepref = TRUE)
    emr_filter.create("f2", src = "ltrack", val = c(15, 16), keepref = TRUE)
    emr_filter.create("f3", src = "ltrack", val = c(14, 15, 16), keepref = TRUE)

    t1 <- emr_extract("ltrack", names = c("vals"), filter = "f1 || f2", keepref = TRUE)
    t2 <- emr_extract("ltrack", names = c("vals"), filter = "f3", keepref = TRUE)

    expect_equal(t1, t2)

    t1 <- emr_extract("ltrack", names = c("vals"), filter = "f1 && f2", keepref = TRUE)
    expect_equal(t1 %>% nrow(), 0)
})

test_that("emr_filter works on numeric logical tracks", {
    emr_filter.clear()
    withr::defer(clean_logical_tracks())

    emr_track.logical.create("ltrack", "track0")

    emr_filter.create("f_ltrack", src = "ltrack")

    t1 <- emr_extract("ltrack", names = c("vals"), filter = "f_ltrack", keepref = TRUE) %>% dplyr::select(-ref)
    t2 <- emr_extract("ltrack", names = c("vals"), keepref = TRUE) %>% dplyr::select(-ref)

    expect_equal(t1, t2)

    t1 <- emr_extract("ltrack", names = c("vals"), filter = "f_ltrack", keepref = TRUE) %>% dplyr::select(-ref)
    t2 <- emr_extract("track0", names = c("vals"), keepref = TRUE) %>% dplyr::select(-ref)

    expect_equal(t1, t2)
})

# emr_filter.info

test_that("emr_filter.info works with filters on logical tracks", {
    emr_filter.clear()
    withr::defer(clean_logical_tracks())

    emr_track.logical.create("ltrack", "ph1", c(15, 16))

    emr_filter.create("f1", src = "ltrack", val = c(14, 15), keepref = TRUE)
    info <- emr_filter.info("f1")
    expect_equal(info$val, c(14, 15))
    expect_equal(info$src, "ltrack")
    expect_equal(info$keepref, TRUE)

    emr_filter.create("f2", src = "ltrack", val = c(17), keepref = TRUE)
    info <- emr_filter.info("f2")
    expect_equal(info$val, c(17))
    expect_equal(info$src, "ltrack")
    expect_equal(info$keepref, TRUE)

    emr_filter.create("f3", src = "ltrack", keepref = TRUE)
    info <- emr_filter.info("f3")
    expect_null(info$val)
    expect_equal(info$src, "ltrack")
    expect_equal(info$keepref, TRUE)
})

test_that("emr_filter.info works with filters on numeric logical tracks", {
    emr_filter.clear()
    withr::defer(clean_logical_tracks())

    emr_track.logical.create("ltrack", "track0")
    emr_filter.create("f1", src = "ltrack", time.shift = c(14, 15), keepref = FALSE)

    info <- emr_filter.info("f1")
    expect_null(info$val)
    expect_equal(info$src, "ltrack")
    expect_equal(info$time_shift, c(14, 15))
    expect_equal(info$keepref, FALSE)
})

# emr_filter.attr.src

test_that("emr_filter.attr.src works no input", {
    emr_filter.clear()
    withr::defer(clean_logical_tracks())

    emr_track.logical.create("ltrack", "ph1", c(15, 16))

    emr_filter.create("f1", src = "ltrack", val = c(14, 15), keepref = TRUE)
    src <- emr_filter.attr.src("f1")
    expect_equal("ltrack", src)
})

test_that("emr_filter.attr.src works change from logical to logical", {
    emr_filter.clear()
    withr::defer(clean_logical_tracks())

    emr_track.logical.create("l1", "ph1", c(15, 16))
    emr_track.logical.create("l2", "ph1", c(14, 15, 16))

    emr_filter.create("f1", src = "l1", val = c(14, 15), keepref = TRUE)
    emr_filter.attr.src("f1", src = "l2")

    filter_info <- emr_filter.info("f1")

    expect_equal("l2", filter_info$src)
    expect_equal(c(14, 15), filter_info$val)

    t1 <- emr_extract("l2", names = c("vals"), filter = "f1", keepref = TRUE)
    t2 <- emr_extract("l2", names = c("vals"), keepref = TRUE) %>% dplyr::filter(vals == 15 | vals == 14)

    expect_equal(t1, t2)
})

test_that("emr_filter.attr.src works change from logical to physical", {
    emr_filter.clear()
    withr::defer(clean_logical_tracks())

    emr_track.logical.create("l1", "ph1", c(15, 16))

    emr_filter.create("f1", src = "l1", val = c(14, 15), keepref = TRUE)
    emr_filter.attr.src("f1", src = "ph1")

    filter_info <- emr_filter.info("f1")

    expect_equal("ph1", filter_info$src)
    expect_equal(c(14, 15), filter_info$val)

    t1 <- emr_extract("ph1", names = c("vals"), filter = "f1", keepref = TRUE)
    t2 <- emr_extract("ph1", names = c("vals"), keepref = TRUE) %>% dplyr::filter(vals == 15 | vals == 14)

    expect_equal(t1, t2)
})

test_that("emr_filter.attr.src works when changed from physical to logical", {
    emr_filter.clear()
    withr::defer(clean_logical_tracks())

    emr_track.logical.create("l1", "ph1", c(15, 16))

    emr_filter.create("f1", src = "ph1", val = c(14, 15), keepref = TRUE)
    emr_filter.attr.src("f1", src = "l1")

    filter_info <- emr_filter.info("f1")

    expect_equal("l1", filter_info$src)
    expect_equal(c(14, 15), filter_info$val)

    t1 <- emr_extract("l1", names = c("vals"), filter = "f1", keepref = TRUE)
    t2 <- emr_extract("l1", names = c("vals"), keepref = TRUE) %>% dplyr::filter(vals == 15 | vals == 14)

    expect_equal(t1, t2)
})

# emr_filter.attr.val

test_that("emr_filter.attr.val works no input", {
    emr_filter.clear()
    withr::defer(clean_logical_tracks())

    emr_track.logical.create("ltrack", "ph1", c(15, 16))

    emr_filter.create("f1", src = "ltrack", val = c(14, 15), keepref = TRUE)
    val <- emr_filter.attr.val("f1")
    expect_equal(c(14, 15), val)
})

test_that("emr_filter.attr.val changes work on logical track", {
    emr_filter.clear()
    withr::defer(clean_logical_tracks())

    emr_track.logical.create("l1", "ph1", c(15, 16))
    emr_filter.create("f1", src = "l1", val = c(14, 15), keepref = TRUE)

    emr_filter.attr.val("f1", val = c(10, 15, 20))

    filter_info <- emr_filter.info("f1")

    expect_equal("l1", filter_info$src)
    expect_equal(c(10, 15, 20), filter_info$val)

    t1 <- emr_extract("l1", names = c("vals"), filter = "f1", keepref = TRUE)
    t2 <- emr_extract("l1", names = c("vals"), keepref = TRUE) %>% dplyr::filter(vals == 10 | vals == 15 | vals == 20)

    expect_equal(t1, t2)
})

# emr_track.info
test_that("emr_track.info works for logical tracks", {
    withr::defer(clean_logical_tracks())
    emr_filter.create("f1", src = "ph1", val = seq(4, 16, 1), keepref = TRUE)
    df <- emr_extract("ph1", names = c("value"), keepref = TRUE, filter = "f1")
    emr_track.import("l1_ph", space = "global", categorical = TRUE, src = df)

    emr_track.logical.create("l1", "ph1", seq(4, 16, 1))

    info_p <- emr_track.info("l1_ph")
    info_l <- emr_track.info("l1")
    expect_true(info_l$type %in% c("sparse", "dense"))
    info_p$type <- NULL
    info_l$type <- NULL
    expect_equal(info_l$path, logical_track_path("l1"))
    info_p$path <- NULL
    info_l$path <- NULL


    expect_equal(info_p, info_l)

    withr::defer(emr_track.rm("l1_ph", force = TRUE))
})

# emr_track.var

test_that("emr_track.var set and get works on logical tracks", {
    withr::defer(clean_logical_tracks())

    emr_track.logical.create("l1", "ph1", seq(4, 16, 1))
    emr_track.var.set("l1", "var", 1:10)

    var <- emr_track.var.get("l1", "var")

    expect_equal(1:10, var)
    expect_true("var" %in% emr_track.var.ls("l1"))
})

test_that("emr_track.var rm works on logical tracks", {
    withr::defer(clean_logical_tracks())

    emr_track.logical.create("l1", "ph1", seq(4, 16, 1))
    emr_track.var.set("l1", "var", 1:10)

    var <- emr_track.var.get("l1", "var")

    expect_equal(1:10, var)

    emr_track.var.rm("l1", "var")

    expect_null(emr_track.var.get("l1", "var"))
    expect_false("var" %in% emr_track.var.ls("l1"))
})

test_that("emr_track.var ls works on logical tracks", {
    withr::defer(clean_logical_tracks())

    emr_track.logical.create("l1", "ph1", seq(4, 16, 1))

    emr_track.var.set("l1", "var", 1:10)
    expect_true("var" %in% emr_track.var.ls("l1"))

    emr_track.var.rm("l1", "var")
    expect_false("var" %in% emr_track.var.ls("l1"))
})


test_that("emr_track.mv when physical track is moved, all dependent logical are moved with vars", {
    withr::defer(clean_logical_tracks())

    emr_filter.create("f1", src = "ph1", val = seq(4, 16, 1), keepref = TRUE)
    df <- emr_extract("ph1", names = c("value"), keepref = TRUE, filter = "f1")
    emr_track.import("l1_ph", space = "global", categorical = TRUE, src = df)

    emr_track.logical.create("l1", "l1_ph", seq(4, 16, 1))
    emr_track.var.set("l1", "var", 1:10)
    emr_track.mv("l1_ph", "ph_l1")

    ltrack_info <- emr_track.logical.info("l1")
    var <- emr_track.var.get("l1", "var")

    expect_equal("ph_l1", ltrack_info$source)
    expect_equal(1:10, var)

    withr::defer(emr_track.rm("ph_l1", force = TRUE))
})

test_that("emr_track.rm when physical track is removed, all dependent logical are removed", {
    withr::defer(clean_logical_tracks())

    emr_filter.create("f1", src = "ph1", val = seq(4, 16, 1), keepref = TRUE)
    df <- emr_extract("ph1", names = c("value"), keepref = TRUE, filter = "f1")
    emr_track.import("l1_ph", space = "global", categorical = TRUE, src = df)

    emr_track.logical.create("l1", "l1_ph", seq(4, 16, 1))
    emr_track.var.set("l1", "var", 1:10)

    emr_track.rm("l1_ph", force = TRUE)

    expect_error(emr_track.var.get("l1", "var"))
    expect_false(emr_track.exists("l1"))

    withr::defer(emr_track.rm("l1_ph", force = TRUE))
})

test_that("emr_track.rm with multiple tracks when physical track are removed, all dependent logical are removed", {
    withr::defer(clean_logical_tracks())

    emr_filter.create("f1", src = "ph1", val = seq(4, 16, 1), keepref = TRUE)
    df <- emr_extract("ph1", names = c("value"), keepref = TRUE, filter = "f1")
    emr_track.import("l1_ph", space = "global", categorical = TRUE, src = df)
    emr_track.import("l2_ph", space = "global", categorical = TRUE, src = df)

    emr_track.logical.create("l1", "l1_ph", seq(4, 16, 1))
    emr_track.logical.create("l2", "l2_ph", seq(4, 16, 1))
    emr_track.var.set("l1", "var", 1:10)
    emr_track.var.set("l2", "var", 1:10)

    emr_track.rm(c("l1_ph", "l2_ph"), force = TRUE)

    expect_error(emr_track.var.get("l1", "var"))
    expect_false(emr_track.exists("l1"))
    expect_error(emr_track.var.get("l2", "var"))
    expect_false(emr_track.exists("l2"))

    withr::defer(emr_track.rm("l1_ph", force = TRUE))
    withr::defer(emr_track.rm("l2_ph", force = TRUE))
})

test_that("emr_track.attr.get returns correct output on a logical track", {
    withr::defer(clean_attributes())
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("l1", "track1")
    emr_track.attr.set("l1", "var1", "val1")
    expect_equal(emr_track.attr.get("l1", "var1"), "val1")
})

test_that("emr_track.attr.set works multiple times on logical tracks", {
    withr::defer(clean_attributes())
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("l1", "track1")
    emr_track.logical.create("l2", "track2")
    emr_track.logical.create("l7", "track7")
    emr_track.attr.set("track1", "var1", "val1")
    emr_track.attr.set("track1", "var2", "val2")
    emr_track.attr.set("track1", "var3", "val3")
    emr_track.attr.set("track2", "var2", "baba")
    emr_track.attr.set("track7", "var1", "val3")
    emr_track.attr.set("track7", "var2", "")
    emr_track.attr.set("l1", "var1", "val1")
    emr_track.attr.set("l1", "var2", "val2")
    emr_track.attr.set("l1", "var3", "val3")
    emr_track.attr.set("l2", "var2", "baba")
    emr_track.attr.set("l7", "var1", "val3")
    emr_track.attr.set("l7", "var2", "")
    expect_equal(emr_track.attr.export(), data.frame(
        track = c(
            "l1", "l1", "l1", "l2", "l7", "l7",
            "track1", "track1", "track1", "track2", "track7", "track7"
        ),
        attr = c(
            "var1", "var2", "var3", "var2", "var1", "var2",
            "var1", "var2", "var3", "var2", "var1", "var2"
        ), value = c(
            "val1",
            "val2", "val3", "baba", "val3", "", "val1", "val2", "val3",
            "baba", "val3", ""
        )
    ))
})

test_that("emr_track.attr.export works for multiple tracks on logical tracks", {
    withr::defer(clean_attributes())
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("l1", "track1")
    emr_track.logical.create("l2", "track2")
    emr_track.logical.create("l7", "track7")

    emr_track.attr.set("track1", "var1", "val1")
    emr_track.attr.set("track1", "var2", "val2")
    emr_track.attr.set("track1", "var3", "val3")
    emr_track.attr.set("track2", "var2", "baba")
    emr_track.attr.set("track7", "var1", "val3")
    emr_track.attr.set("track7", "var2", "")

    emr_track.attr.set("l1", "var1", "val1")
    emr_track.attr.set("l1", "var2", "val2")
    emr_track.attr.set("l1", "var3", "val3")
    emr_track.attr.set("l2", "var2", "baba")
    emr_track.attr.set("l7", "var1", "val3")
    emr_track.attr.set("l7", "var2", "")
    expect_equal(
        emr_track.attr.export(c("l1", "l7", "track1")),
        structure(list(track = c(
            "l1", "l1", "l1", "l7", "l7", "track1",
            "track1", "track1"
        ), attr = c(
            "var1", "var2", "var3", "var1",
            "var2", "var1", "var2", "var3"
        ), value = c(
            "val1", "val2", "val3",
            "val3", "", "val1", "val2", "val3"
        )), row.names = c(NA, 8L), class = "data.frame")
    )
})

test_that("emr_track.attr.export works for multiple tracks and vars on logical tracks", {
    withr::defer(clean_attributes())
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("l1", "track1")
    emr_track.logical.create("l2", "track2")
    emr_track.logical.create("l7", "track7")

    emr_track.attr.set("l1", "var1", "val1")
    emr_track.attr.set("l1", "var2", "val2")
    emr_track.attr.set("l1", "var3", "val3")
    emr_track.attr.set("l2", "var2", "baba")
    emr_track.attr.set("l7", "var1", "val3")
    emr_track.attr.set("l7", "var2", "")
    expect_equal(
        emr_track.attr.export(c("l1", "l7"), c("var2", "var3")),
        structure(list(track = c("l1", "l1", "l7"), attr = c(
            "var2",
            "var3", "var2"
        ), value = c("val2", "val3", "")), row.names = c(
            NA,
            3L
        ), class = "data.frame")
    )
})

test_that("emr_track.attr.export works by attributes on logical tracks", {
    withr::defer(clean_attributes())
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("l1", "track1")
    emr_track.logical.create("l2", "track2")
    emr_track.logical.create("l7", "track7")

    emr_track.attr.set("l1", "var1", "val1")
    emr_track.attr.set("l1", "var2", "val2")
    emr_track.attr.set("l1", "var3", "val3")
    emr_track.attr.set("l2", "var2", "baba")
    emr_track.attr.set("l7", "var1", "val3")
    emr_track.attr.set("l7", "var2", "")

    expect_equal(
        emr_track.attr.export(attr = c("var2", "var3")),
        structure(list(track = c("l1", "l1", "l2", "l7"), attr = c("var2", "var3", "var2", "var2"), value = c(
            "val2",
            "val3", "baba", ""
        )), row.names = c(NA, 4L), class = "data.frame")
    )
})


test_that("emr_track.ls finds tracks by var on logical tracks", {
    withr::defer(clean_attributes())
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("l1", "track1")
    emr_track.logical.create("l2", "track2")
    emr_track.logical.create("l7", "track7")

    emr_track.attr.set("l1", "var1", "val1")
    emr_track.attr.set("l1", "var2", "val2")
    emr_track.attr.set("l1", "var3", "val3")
    emr_track.attr.set("l2", "var2", "baba")
    emr_track.attr.set("l7", "var1", "val3")
    emr_track.attr.set("l7", "var2", "")
    expect_equal(emr_track.ls(var1 = ""), c("l1", "l7"))
})

test_that("emr_track.ls finds tracks by var and value on logical tracks", {
    withr::defer(clean_attributes())
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("l1", "track1")
    emr_track.logical.create("l2", "track2")
    emr_track.logical.create("l7", "track7")

    emr_track.attr.set("l1", "var1", "val1")
    emr_track.attr.set("l1", "var2", "val2")
    emr_track.attr.set("l1", "var3", "val3")
    emr_track.attr.set("l2", "var2", "baba")
    emr_track.attr.set("l7", "var1", "kuku")
    emr_track.attr.set("l7", "var2", "")
    expect_equal(emr_track.ls(var1 = "kuku"), "l7")
})

test_that("emr_track.ls finds tracks by multiple vars and values with regex on logical tracks", {
    withr::defer(clean_attributes())
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("l1", "track1")
    emr_track.logical.create("l2", "track2")
    emr_track.logical.create("l7", "track7")

    emr_track.attr.set("l1", "var1", "val1")
    emr_track.attr.set("l1", "var2", "val2")
    emr_track.attr.set("l1", "var3", "val3")
    emr_track.attr.set("l2", "var2", "baba")
    emr_track.attr.set("l7", "var1", "val3")
    emr_track.attr.set("l7", "var2", "")
    expect_equal(emr_track.ls(var1 = "val*", var2 = ""), c("l1", "l7"))
})

test_that("emr_track.rm removes the track attributes for logical tracks", {
    withr::defer(clean_attributes())
    withr::defer(clean_logical_tracks())
    initial_attrs <- emr_track.attr.export()
    emr_track.logical.create("l1", "track1")
    emr_track.attr.set("l1", "var1", "val1")
    attrs_file <- file.path(.naryn$EMR_GROOT, "logical", ".l1.attrs")
    expect_true(file.exists(attrs_file))
    expect_equal(emr_track.attr.get("l1", "var1"), "val1")
    emr_track.rm("l1", force = TRUE)
    expect_error(emr_track.attr.get("l1", "var1"))
    expect_equal(emr_track.attr.export(), initial_attrs)
    expect_false(file.exists(attrs_file))
})

test_that("emr_track.rm of multiple tracks removes the track attributes for logical tracks", {
    withr::defer(clean_attributes())
    withr::defer(clean_logical_tracks())
    initial_attrs <- emr_track.attr.export()
    emr_track.logical.create("l1", "track1")
    emr_track.attr.set("l1", "var1", "val1")
    emr_track.logical.create("l2", "track1")
    emr_track.attr.set("l2", "var1", "val1")
    attrs_file1 <- file.path(.naryn$EMR_GROOT, "logical", ".l1.attrs")
    attrs_file2 <- file.path(.naryn$EMR_GROOT, "logical", ".l2.attrs")
    expect_true(file.exists(attrs_file1))
    expect_true(file.exists(attrs_file2))
    expect_equal(emr_track.attr.get("l1", "var1"), "val1")
    expect_equal(emr_track.attr.get("l2", "var1"), "val1")
    emr_track.rm(c("l1", "l2"), force = TRUE)
    expect_error(emr_track.attr.get("l1", "var1"))
    expect_error(emr_track.attr.get("l2", "var1"))
    expect_equal(emr_track.attr.export(), initial_attrs)
    expect_false(file.exists(attrs_file1))
    expect_false(file.exists(attrs_file2))
})

test_that("logical tracks are reloaded after switching dbs", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("l1", "track1")
    first_db <- emr_db.ls()[1]
    load_test_db()
    expect_length(emr_track.logical.ls(), 0)
    emr_db.connect(first_db)
    expect_equal(emr_track.logical.ls(), "l1")
})
