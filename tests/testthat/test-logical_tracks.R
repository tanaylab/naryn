
clean_logical_tracks <- function() {
    purrr::walk(emr_track.logical.ls(), emr_track.logical.rm, force = TRUE)
}
clean_logical_tracks()

logical_track_ok <- function(track, source, values = NULL) {
    expect_true(track %in% emr_track.logical.ls())
    expect_true(track %in% emr_track.ls())
    expect_true(track %in% emr_track.global.ls())
    expect_true(emr_track.logical.exists(track))
    expect_equal(emr_logical_track.info(track)$source, source)
    if (is.null(values)) {
        expect_null(emr_logical_track.info(track)$values)
    } else {
        expect_equal(emr_logical_track.info(track)$values, values)
    }

    expect_true(emr_track.exists(track))
}

test_that("emr_track.create_logical tracks works", {
    withr::defer(clean_logical_tracks())
    emr_track.create_logical("logical_track", "physical_track1", c(15, 16))
    logical_track_ok("logical_track", "physical_track1", c(15, 16))
    expect_false(emr_track.logical.exists("track1"))
})

test_that("emr_track.create_logical tracks works without values", {
    withr::defer(clean_logical_tracks())
    emr_track.create_logical("logical_track1", "physical_track1")
    logical_track_ok("logical_track1", "physical_track1")
})

test_that("emr_track.create_logical tracks fails with existing tracks", {
    withr::defer(clean_logical_tracks())
    emr_track.create_logical("logical_track", "physical_track1", c(15, 16))
    expect_error(emr_track.create_logical("logical_track", "physical_track1", c(15, 16)))
    expect_error(emr_track.create_logical("track1", "physical_track1", c(15, 16)))
})

test_that("emr_track.create_logical tracks fails with non-categorical tracks", {
    expect_error(emr_track.create_logical("logical_track", "track2", c(15, 16)))
})

test_that("emr_track.create_logical tracks fails with illegal track names", {
    expect_error(emr_track.create_logical(".logical_track", "physical_track1", c(15, 16)))
})

test_that("emr_logical_track.info returns correct value", {
    withr::defer(clean_logical_tracks())
    emr_track.create_logical("logical_track", "physical_track1", c(15, 16))
    res <- emr_logical_track.info("logical_track")
    expect_equal(names(res), c("source", "values"))
    expect_equal(res$source, "physical_track1")
    expect_equal(res$values, c(15, 16))
    expect_error(emr_logical_track.info("track1"))
    expect_error(emr_logical_track.info("blahblah"))
})

test_that("emr_track.logical.rm works ", {
    withr::defer(clean_logical_tracks())
    emr_track.create_logical("logical_track_test", "physical_track1", c(15, 16))
    logical_track_ok("logical_track_test", "physical_track1", c(15, 16))
    emr_track.logical.rm("logical_track_test", force = TRUE)
    expect_false("logical_track_test" %in% emr_track.logical.ls())
    expect_false("logical_track_test" %in% emr_track.ls())
    expect_false("logical_track_test" %in% emr_track.global.ls())
    expect_false(emr_track.exists("logical_track_test"))
    expect_error(emr_extract("logical_track_test"))
    expect_error(emr_logical_track.info("logical_track_test"))
})

test_that("emr_track.logical.rm fails when track doesn't exist ", {
    expect_error(emr_track.logical.rm("blahblahblah"))
})

test_that("emr_track.logical.rm fails when track is physical", {
    expect_error(emr_track.logical.rm("physical_track1"))
})

# test multiple processes
test_that("logical tracks creation persists between R sessions", {
    withr::defer(clean_logical_tracks())
    emr_track.create_logical("logical_track_test1", "physical_track1", c(15, 16))
    emr_track.create_logical("logical_track_test2", "physical_track1")
    res <- callr::r(
        function(root) {
            devtools::load_all()
            emr_db.init(global.dir = root)
            return(emr_track.logical.ls())
        },
        args = list(root = EMR_GROOT)
    )
    expect_equal(res, c("logical_track_test1", "logical_track_test2"))
})

test_that("logical tracks creation persists between R sessions for existing sessions", {
    withr::defer(clean_logical_tracks())
    callr::r(
        function(root) {
            devtools::load_all()
            emr_db.init(global.dir = root)
            emr_track.create_logical("logical_track_test1", "physical_track1", c(15, 16))
            emr_track.create_logical("logical_track_test2", "physical_track1")
        },
        args = list(root = EMR_GROOT)
    )
    expect_equal(emr_track.logical.ls(), c("logical_track_test1", "logical_track_test2"))
    a <- emr_extract("logical_track_test2", keepref = TRUE)
    b <- emr_extract("physical_track1", names = c("logical_track_test2"), keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)
})

test_that("logical tracks creation persists between R sessions", {
    withr::defer(clean_logical_tracks())
    emr_track.create_logical("logical_track_test1", "physical_track1", c(15, 16))
    emr_track.create_logical("logical_track_test2", "physical_track1")
    emr_track.logical.rm("logical_track_test1", force = TRUE)
    res <- callr::r(
        function(root) {
            devtools::load_all()
            emr_db.init(global.dir = root)
            return(emr_track.logical.ls())
        },
        args = list(root = EMR_GROOT)
    )
    expect_equal(res, "logical_track_test2")
})

test_that("logical tracks deletion persists between R sessions for existing sessions", {
    withr::defer(clean_logical_tracks())
    emr_track.create_logical("logical_track_test1", "physical_track1", c(15, 16))
    emr_track.create_logical("logical_track_test2", "physical_track1")
    res <- callr::r(
        function(root) {
            devtools::load_all()
            emr_db.init(global.dir = root)
            emr_track.logical.rm("logical_track_test1", force = TRUE)
        },
        args = list(root = EMR_GROOT)
    )
    expect_equal(emr_track.logical.ls(), "logical_track_test2")
})

test_that("emr_track.logical.ls works", {
    withr::defer(clean_logical_tracks())
    for (i in 1:10) {
        emr_track.create_logical(paste0("logical_track", i), "physical_track1", c(15, 16))
    }
    for (i in 11:20) {
        emr_track.create_logical(paste0("logical_track", i), "physical_track1")
    }

    expect_setequal(emr_track.logical.ls(), paste0("logical_track", 1:20))
    expect_true(all(paste0("logical_track", 1:20) %in% emr_track.ls()))
    expect_true(all(paste0("logical_track", 1:20) %in% emr_track.global.ls()))
})

# detect logical tracks in expression
test_that("detect_expr_logical_tracks works", {
    withr::defer(clean_logical_tracks())
    emr_track.create_logical("logical_track6", "physical_track1", c(15, 16))
    emr_track.create_logical("logical_track", "physical_track1", c(15, 16))
    emr_track.create_logical("savta", "physical_track1", c(15, 16))
    expect_setequal(detect_expr_logical_tracks("logical_track+logical_track6/log(savta)"), c("logical_track", "logical_track6", "savta"))
    expect_equal(detect_expr_logical_tracks("logical_track*2"), "logical_track")
    expect_equal(detect_expr_logical_tracks("blah/blah+logical"), character(0))
    expect_equal(detect_expr_logical_tracks("logical_track-sababa"), "logical_track")
})

# detect physical tracks in expression
test_that("detect_expr_physical_tracks works", {
    expect_setequal(detect_expr_physical_tracks("track1+track2/log(track3)"), c("track1", "track2", "track3"))
    expect_equal(detect_expr_physical_tracks("track1*2"), "track1")
    expect_equal(detect_expr_physical_tracks("blah/blah+logical"), character(0))
    expect_equal(detect_expr_physical_tracks("track1-sababa"), "track1")
})

# detect virtual tracks in expression
test_that("detect_expr_virtual_tracks works", {
    emr_vtrack.create("vt1", "physical_track1")
    emr_vtrack.create("vt2", "track1")
    emr_vtrack.create("vt3", "track2")
    withr::defer(EMR_VTRACKS <<- list())
    expect_setequal(detect_expr_virtual_tracks("vt1+vt2/log(vt3)"), c("vt1", "vt2", "vt3"))
    expect_equal(detect_expr_virtual_tracks("vt1*2"), "vt1")
    expect_equal(detect_expr_virtual_tracks("blah/blah+logical"), character(0))
    expect_equal(detect_expr_virtual_tracks("vt1-sababa"), "vt1")
})

# test implicit vtrack creation
test_that("logical track returns a valid vtrack R object without values", {
    withr::defer(clean_logical_tracks())
    emr_track.create_logical("logical_track1", "physical_track1")
    res <- .emr_call("logical_track_vtrack", "logical_track1", new.env(parent = parent.frame()), silent = TRUE)
    emr_vtrack.create("vt", "physical_track1", keepref = TRUE)
    vt <- EMR_VTRACKS[[1]]$vt
    expect_equal(vt, res)
    withr::defer(emr_vtrack.rm("vt"))
})

test_that("logical track returns a valid vtrack R object with values", {
    withr::defer(clean_logical_tracks())
    emr_track.create_logical("logical_track1", "physical_track1", c(15, 16))
    res <- .emr_call("logical_track_vtrack", "logical_track1", new.env(parent = parent.frame()), silent = TRUE)
    emr_vtrack.create("vt", "physical_track1", params = c(15, 16), keepref = TRUE)
    vt <- EMR_VTRACKS[[1]]$vt
    expect_equal(vt, res)
    withr::defer(emr_vtrack.rm("vt"))
})

# addto

# attributes

# create


# ids


# emr_extract

test_that("emr_extract works with logical track as expression and implicit iterator", {
    withr::defer(clean_logical_tracks())
    emr_track.create_logical("logical_track", "physical_track1")
    a <- emr_extract("logical_track", keepref = TRUE)
    b <- emr_extract("physical_track1", names = "logical_track", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_extract("logical_track*2", names = "logical_track", keepref = TRUE)
    b <- emr_extract("physical_track1*2", names = "logical_track", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)
})

test_that("emr_extract works with logical track with values as expression and implicit iterator", {
    withr::defer(clean_logical_tracks())
    emr_track.create_logical("logical_track", "physical_track1", c(15, 16))
    a <- emr_extract("logical_track", keepref = TRUE)
    emr_filter.create("ltrack_filter", src = "physical_track1", val = c(15, 16), keepref = TRUE)
    withr::defer(emr_filter.rm("ltrack_filter"))
    b <- emr_extract("physical_track1", filter = "ltrack_filter", names = "logical_track", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_extract("logical_track*2", names = "logical_track", keepref = TRUE)
    b <- emr_extract("physical_track1*2", filter = "ltrack_filter", names = "logical_track", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)
})

test_that("emr_extract works with logical track as expression and explicit iterator", {
    withr::defer(clean_logical_tracks())
    emr_track.create_logical("logical_track", "physical_track1")
    a <- emr_extract("logical_track", iterator = "logical_track", keepref = TRUE)
    b <- emr_extract("physical_track1", iterator = "logical_track", names = "logical_track", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_extract("logical_track*2", iterator = "logical_track", names = "logical_track", keepref = TRUE)
    b <- emr_extract("physical_track1*2", iterator = "physical_track1", , names = "logical_track", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)
})

test_that("emr_extract works with logical track with values as expression and explicit iterator", {
    withr::defer(clean_logical_tracks())
    emr_track.create_logical("logical_track", "physical_track1", c(15, 16))
    a <- emr_extract("logical_track", iterator = "logical_track", keepref = TRUE)
    emr_filter.create("ltrack_filter", src = "physical_track1", val = c(15, 16), keepref = TRUE)
    withr::defer(emr_filter.rm("ltrack_filter"))
    b <- emr_extract("physical_track1", iterator = "physical_track1", filter = "ltrack_filter", names = "logical_track", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)
})

test_that("emr_extract works with logical track and multiple expressions", {
    withr::defer(clean_logical_tracks())
    emr_track.create_logical("logical_track1", "physical_track1", c(15, 16))
    emr_track.create_logical("logical_track2", "physical_track1", c(13, 14))
    a <- emr_extract(c("logical_track1", "track1"), iterator = "logical_track1", keepref = TRUE)
    emr_filter.create("ltrack_filter", src = "physical_track1", val = c(15, 16), keepref = TRUE)
    withr::defer(emr_filter.rm("ltrack_filter"))
    b <- emr_extract(c("physical_track1", "track1"), iterator = "physical_track1", filter = "ltrack_filter", names = c("logical_track1", "track1"), keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)
})

# emr_cor
test_that("emr_cor works with logical track as expression", {
    withr::defer(clean_logical_tracks())
    emr_track.create_logical("logical_track1", "physical_track1", c(15, 16))
    emr_track.create_logical("logical_track2", "physical_track1")
    emr_filter.create("ltrack_filter", src = "physical_track1", val = c(15, 16), keepref = TRUE)
    withr::defer(emr_filter.rm("ltrack_filter"))

    a <- emr_cor("logical_track1", c(14, 15, 16), cor.exprs = c("track0", "track1", "track2"), dataframe = TRUE, keepref = TRUE)
    b <- emr_cor("physical_track1", c(14, 15, 16), cor.exprs = c("track0", "track1", "track2"), dataframe = TRUE, keepref = TRUE, names = "logical_track")
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_cor("logical_track1", c(14, 15, 16), cor.exprs = c("logical_track1", "logical_track2"), dataframe = TRUE, keepref = TRUE)
    b <- emr_cor("physical_track1", c(14, 15, 16), cor.exprs = c("logical_track1", "logical_track2"), dataframe = TRUE, keepref = TRUE, names = "logical_track")
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_cor("logical_track2", c(14, 15, 16), cor.exprs = c("track0", "track1", "track2"), dataframe = TRUE, keepref = TRUE)
    b <- emr_cor("physical_track1", c(14, 15, 16), cor.exprs = c("track0", "track1", "track2"), dataframe = TRUE, keepref = TRUE, names = "logical_track")
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_cor("track2", c(100, 300, 500, 900, 2000, 3000), "logical_track1", c(14, 15, 16), "logical_track2",
        c(11:16),
        cor.exprs = c("track0", "track1", "track2"), dataframe = TRUE, keepref = TRUE, iterator = "physical_track1"
    )
    b <- emr_cor("track2", c(100, 300, 500, 900, 2000, 3000), "physical_track1", c(14, 15, 16), "physical_track1",
        c(11:16),
        cor.exprs = c("track0", "track1", "track2"), dataframe = TRUE, keepref = TRUE, iterator = "physical_track1", names = c("track2", "logical_track1", "logical_track2")
    )
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_cor("track2", c(100, 300, 500, 900, 2000, 3000), "logical_track1", c(14, 15, 16), "logical_track2",
        c(11:16),
        cor.exprs = c("track0", "track1", "track2"), dataframe = TRUE, keepref = TRUE, iterator = "logical_track1"
    )
    b <- emr_cor("track2", c(100, 300, 500, 900, 2000, 3000), "physical_track1", c(14, 15, 16), "physical_track1",
        c(11:16),
        cor.exprs = c("track0", "track1", "track2"), dataframe = TRUE, keepref = TRUE, iterator = "logical_track1", names = c("track2", "logical_track1", "logical_track2")
    )
    expect_equal(a, b, ignore_attr = TRUE)
})

test_that("emr_cor works with logical track as iterator", {
    withr::defer(clean_logical_tracks())
    emr_track.create_logical("logical_track1", "physical_track1", c(15, 16))
    emr_filter.create("ltrack_filter", src = "physical_track1", val = c(15, 16), keepref = TRUE)
    withr::defer(emr_filter.rm("ltrack_filter"))

    a <- emr_cor("physical_track1", c(14, 15, 16), cor.exprs = c("track0", "track1", "track2"), iterator = "logical_track1", dataframe = TRUE, keepref = TRUE)
    b <- emr_cor("physical_track1", c(14, 15, 16), filter = "ltrack_filter", cor.exprs = c("track0", "track1", "track2"), dataframe = TRUE, keepref = TRUE, names = "logical_track")
    expect_equal(a, b, ignore_attr = TRUE)

    emr_track.create_logical("logical_track2", "physical_track1")
    a <- emr_cor("physical_track1", c(14, 15, 16), cor.exprs = c("track0", "track1", "track2"), iterator = "logical_track1", dataframe = TRUE, keepref = TRUE)
    b <- emr_cor("physical_track1", c(14, 15, 16), cor.exprs = c("track0", "track1", "track2"), dataframe = TRUE, keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)
})

test_that("emr_cor works with logical track as expression and iterator", {
    withr::defer(clean_logical_tracks())
    emr_track.create_logical("logical_track1", "physical_track1", c(15, 16))
    emr_filter.create("ltrack_filter", src = "physical_track1", val = c(15, 16), keepref = TRUE)
    withr::defer(emr_filter.rm("ltrack_filter"))

    a <- emr_cor("logical_track1", c(14, 15, 16), cor.exprs = c("track0", "track1", "track2"), iterator = "logical_track1", dataframe = TRUE, keepref = TRUE)
    b <- emr_cor("physical_track1", c(14, 15, 16), cor.exprs = c("track0", "track1", "track2"), filter = "ltrack_filter", dataframe = TRUE, keepref = TRUE, names = "logical_track")
    expect_equal(a, b, ignore_attr = TRUE)

    emr_track.create_logical("logical_track2", "physical_track1")
    a <- emr_cor("logical_track2", c(14, 15, 16), cor.exprs = c("track0", "track1", "track2"), iterator = "logical_track1", dataframe = TRUE, keepref = TRUE)
    b <- emr_cor("physical_track1", c(14, 15, 16), cor.exprs = c("track0", "track1", "track2"), dataframe = TRUE, keepref = TRUE, names = "logical_track")
    expect_equal(a, b, ignore_attr = TRUE)
})

test_that("emr_cor works with logical track as cor.expr", {
    withr::defer(clean_logical_tracks())
    emr_track.create_logical("logical_track1", "physical_track1", c(15, 16))
    emr_track.create_logical("logical_track2", "physical_track1")
    emr_filter.create("ltrack_filter", src = "physical_track1", val = c(15, 16), keepref = TRUE)
    withr::defer(emr_filter.rm("ltrack_filter"))
    a <- emr_cor("track0", c(0, 10, 500, 1000), cor.exprs = c("logical_track1"), dataframe = TRUE, keepref = TRUE)
    b <- emr_cor("track0", c(0, 10, 500, 1000), cor.exprs = c("physical_track1"), filter = "ltrack_filter", dataframe = TRUE, keepref = TRUE)
    expect_equal(a %>% select(-i, -j), b %>% select(-i, -j), ignore_attr = TRUE)
})

# emr_dist

test_that("emr_dist works with logical track as expression", {
    withr::defer(clean_logical_tracks())
    emr_track.create_logical("logical_track1", "physical_track1", c(15, 16))
    a <- emr_dist("logical_track1", c(14, 15, 16), dataframe = TRUE, keepref = TRUE)
    b <- emr_dist("physical_track1", c(14, 15, 16), dataframe = TRUE, keepref = TRUE, names = "logical_track")
    expect_equal(a, b, ignore_attr = TRUE)

    emr_track.create_logical("logical_track2", "physical_track1")
    a <- emr_dist("logical_track2", c(14, 15, 16), dataframe = TRUE, keepref = TRUE)
    b <- emr_dist("physical_track1", c(14, 15, 16), dataframe = TRUE, keepref = TRUE, names = "logical_track")
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_dist("track2", c(100, 300, 500, 900, 2000, 3000), "logical_track1", c(14, 15, 16), "logical_track2",
        c(11:16),
        dataframe = TRUE, keepref = TRUE, iterator = "physical_track1"
    )
    b <- emr_dist("track2", c(100, 300, 500, 900, 2000, 3000), "physical_track1", c(14, 15, 16), "physical_track1",
        c(11:16),
        dataframe = TRUE, keepref = TRUE, iterator = "physical_track1", names = c("track2", "logical_track1", "logical_track2")
    )
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_dist("track2", c(100, 300, 500, 900, 2000, 3000), "logical_track1", c(14, 15, 16), "logical_track2",
        c(11:16),
        dataframe = TRUE, keepref = TRUE, iterator = "logical_track1"
    )
    b <- emr_dist("track2", c(100, 300, 500, 900, 2000, 3000), "physical_track1", c(14, 15, 16), "physical_track1",
        c(11:16),
        dataframe = TRUE, keepref = TRUE, iterator = "logical_track1", names = c("track2", "logical_track1", "logical_track2")
    )
    expect_equal(a, b, ignore_attr = TRUE)
})

test_that("emr_dist works with logical track as iterator", {
    withr::defer(clean_logical_tracks())
    emr_track.create_logical("logical_track1", "physical_track1", c(15, 16))
    a <- emr_dist("physical_track1", c(14, 15, 16), iterator = "logical_track1", dataframe = TRUE, keepref = TRUE)
    b <- emr_dist("physical_track1", c(14, 15, 16), dataframe = TRUE, keepref = TRUE, names = "logical_track")
    expect_equal(a, b, ignore_attr = TRUE)

    emr_track.create_logical("logical_track2", "physical_track1")
    a <- emr_dist("physical_track1", c(14, 15, 16), iterator = "logical_track1", dataframe = TRUE, keepref = TRUE)
    b <- emr_dist("physical_track1", c(14, 15, 16), dataframe = TRUE, keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)
})

test_that("emr_dist works with logical track as expression and iterator", {
    withr::defer(clean_logical_tracks())
    emr_track.create_logical("logical_track1", "physical_track1", c(15, 16))
    a <- emr_dist("logical_track1", c(14, 15, 16), iterator = "logical_track1", dataframe = TRUE, keepref = TRUE)
    b <- emr_dist("physical_track1", c(14, 15, 16), dataframe = TRUE, keepref = TRUE, names = "logical_track")
    expect_equal(a, b, ignore_attr = TRUE)

    emr_track.create_logical("logical_track2", "physical_track1")
    a <- emr_dist("logical_track2", c(14, 15, 16), iterator = "logical_track1", dataframe = TRUE, keepref = TRUE)
    b <- emr_dist("physical_track1", c(14, 15, 16), dataframe = TRUE, keepref = TRUE, names = "logical_track")
    expect_equal(a, b, ignore_attr = TRUE)
})

# emr_screen
test_that("emr_screen works with logical track as expression", {
    withr::defer(clean_logical_tracks())
    emr_track.create_logical("logical_track1", "physical_track1", c(15, 16))
    a <- emr_screen("logical_track1 == 15", keepref = TRUE)
    b <- emr_screen("physical_track1 == 15", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a1 <- emr_screen("logical_track1*2 == 30", keepref = TRUE)
    b1 <- emr_screen("physical_track1*2 == 30", keepref = TRUE)
    expect_equal(a1, b1, ignore_attr = TRUE)
    expect_equal(a1, a, ignore_attr = TRUE)
    expect_equal(b1, b, ignore_attr = TRUE)

    emr_track.create_logical("logical_track2", "physical_track1")
    a <- emr_screen("logical_track2 == 13", keepref = TRUE)
    b <- emr_screen("physical_track1 == 13", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_screen("logical_track1 == 15 & physical_track1 == 13", iterator = "physical_track1", keepref = TRUE)
    b <- emr_screen("physical_track1 == 15 & physical_track1 == 13", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_screen("logical_track1 == 15 & logical_track1 == 16", keepref = TRUE)
    b <- emr_screen("physical_track1 == 15 & physical_track1 == 16", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_screen("logical_track1 == 15 & logical_track2 == 16", iterator = "physical_track1", keepref = TRUE)
    b <- emr_screen("physical_track1 == 15 & physical_track1 == 16", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)
})

test_that("emr_screen works with logical track as iterator", {
    withr::defer(clean_logical_tracks())
    emr_track.create_logical("logical_track1", "physical_track1", c(15, 16))
    a <- emr_screen("physical_track1 == 15", iterator = "logical_track1", keepref = TRUE)
    b <- emr_screen("physical_track1 == 15", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    emr_track.create_logical("logical_track2", "physical_track1")
    a <- emr_screen("physical_track1 == 13", iterator = "logical_track2", keepref = TRUE)
    b <- emr_screen("physical_track1 == 13", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_screen("physical_track1 == 15 & physical_track1 == 13", iterator = "logical_track1", keepref = TRUE)
    b <- emr_screen("physical_track1 == 15 & physical_track1 == 13", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)
})

test_that("emr_screen works with logical track as expression and iterator", {
    withr::defer(clean_logical_tracks())
    emr_track.create_logical("logical_track1", "physical_track1", c(15, 16))
    a <- emr_screen("logical_track1 == 15", iterator = "logical_track1", keepref = TRUE)
    b <- emr_screen("physical_track1 == 15", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    emr_track.create_logical("logical_track2", "physical_track1")
    a <- emr_screen("logical_track2 == 13", iterator = "logical_track2", keepref = TRUE)
    b <- emr_screen("physical_track1 == 13", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_screen("logical_track1 == 15 & physical_track1 == 13", iterator = "logical_track1", keepref = TRUE)
    b <- emr_screen("physical_track1 == 15 & physical_track1 == 13", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)
})

test_that("emr_screen works with logical tracks when expr doesn't return data", {
    withr::defer(clean_logical_tracks())
    emr_track.create_logical("logical_track1", "physical_track1", c(15, 16))
    expect_equal(
        emr_screen("logical_track1 == 11"),
        structure(list(id = integer(0), time = integer(0), ref = integer(0)), class = "data.frame", row.names = integer(0))
    )
})

# emr_quantiles
test_that("emr_quantiles works with logical track", {
    withr::defer(clean_logical_tracks())
    emr_track.create_logical("logical_track1", "physical_track1", c(15, 16))
    emr_track.create_logical("logical_track2", "physical_track1")

    a <- emr_quantiles("logical_track1", c(0.1, 0.2, 0.5, 0.9), keepref = TRUE)
    emr_filter.create("ltrack_filter", src = "physical_track1", val = c(15, 16), keepref = TRUE)
    withr::defer(emr_filter.rm("ltrack_filter"))

    b <- emr_quantiles("physical_track1", c(0.1, 0.2, 0.5, 0.9), filter = "ltrack_filter", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_quantiles("logical_track1*2", c(0.1, 0.2, 0.5, 0.9), keepref = TRUE)
    b <- emr_quantiles("physical_track1*2", c(0.1, 0.2, 0.5, 0.9), filter = "ltrack_filter", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_quantiles("logical_track2", c(0.1, 0.2, 0.5, 0.9), keepref = TRUE)
    b <- emr_quantiles("physical_track1", c(0.1, 0.2, 0.5, 0.9), keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    # as both expression and iterator
    a <- emr_quantiles("logical_track1", c(0.1, 0.2, 0.5, 0.9), iterator = "logical_track1", keepref = TRUE)
    b <- emr_quantiles("physical_track1", c(0.1, 0.2, 0.5, 0.9), filter = "ltrack_filter", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_quantiles("logical_track1*2", c(0.1, 0.2, 0.5, 0.9), iterator = "logical_track1", keepref = TRUE)
    b <- emr_quantiles("physical_track1*2", c(0.1, 0.2, 0.5, 0.9), filter = "ltrack_filter", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_quantiles("logical_track2", c(0.1, 0.2, 0.5, 0.9), iterator = "logical_track1", keepref = TRUE)
    b <- emr_quantiles("physical_track1", c(0.1, 0.2, 0.5, 0.9), filter = "ltrack_filter", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    # as iterator
    a <- emr_quantiles("physical_track1*2", c(0.1, 0.2, 0.5, 0.9), iterator = "logical_track1", keepref = TRUE)
    b <- emr_quantiles("physical_track1*2", c(0.1, 0.2, 0.5, 0.9), filter = "ltrack_filter", , keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_quantiles("physical_track1", c(0.1, 0.2, 0.5, 0.9), iterator = "logical_track2", keepref = TRUE)
    b <- emr_quantiles("physical_track1", c(0.1, 0.2, 0.5, 0.9), keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)
})

# emr_summary
test_that("emr_summary works with logical track", {
    withr::defer(clean_logical_tracks())
    emr_track.create_logical("logical_track1", "physical_track1", c(15, 16))
    emr_track.create_logical("logical_track2", "physical_track1")

    a <- emr_summary("logical_track1", keepref = TRUE)
    emr_filter.create("ltrack_filter", src = "physical_track1", val = c(15, 16), keepref = TRUE)
    withr::defer(emr_filter.rm("ltrack_filter"))

    b <- emr_summary("physical_track1", filter = "ltrack_filter", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_summary("logical_track1*2", keepref = TRUE)
    b <- emr_summary("physical_track1*2", filter = "ltrack_filter", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_summary("logical_track2", keepref = TRUE)
    b <- emr_summary("physical_track1", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    # as both expression and iterator
    a <- emr_summary("logical_track1", iterator = "logical_track1", keepref = TRUE)
    b <- emr_summary("physical_track1", filter = "ltrack_filter", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_summary("logical_track1*2", iterator = "logical_track1", keepref = TRUE)
    b <- emr_summary("physical_track1*2", filter = "ltrack_filter", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_summary("logical_track2", iterator = "logical_track1", keepref = TRUE)
    b <- emr_summary("physical_track1", filter = "ltrack_filter", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    # as iterator
    a <- emr_summary("physical_track1*2", iterator = "logical_track1", keepref = TRUE)
    b <- emr_summary("physical_track1*2", filter = "ltrack_filter", , keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)

    a <- emr_summary("physical_track1", iterator = "logical_track2", keepref = TRUE)
    b <- emr_summary("physical_track1", keepref = TRUE)
    expect_equal(a, b, ignore_attr = TRUE)
})


# emr_track.unique
test_that("emr_track.unique works on logical tracks", {
    withr::defer(clean_logical_tracks())
    emr_track.create_logical("logical_track1", "physical_track1", c(15, 16))
    expect_equal(emr_track.unique("logical_track1"), c(15, 16))

    emr_track.create_logical("logical_track2", "physical_track1")
    expect_equal(emr_track.unique("logical_track2"), emr_track.unique("physical_track1"))
})
