load_test_db()

clean_logical_tracks()

# emr_vtrack.info
test_that("emr_vtrack.info works on logical tracks", {
    emr_vtrack.clear()
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("l1", "ph1", c(15, 16))

    emr_vtrack.create("vt_l", src = "l1", func = "earliest.time", time.shift = c(-100, 50), params = c(-1, 14, 16, 30))

    vt_info <- emr_vtrack.info("vt_l")

    expect_equal(vt_info, list(src = "l1", time_shift = c(-100, 50), func = "earliest.time", params = c(-1, 14, 16, 30), keepref = FALSE, id_map = NULL, filter = NULL))
})

# emr_vtrack.create

test_that("empty emr_vtrack.create works on logical track with all keepref combinations", {
    emr_vtrack.clear()
    withr::defer(clean_logical_tracks())

    emr_filter.create("f1", src = "ph1", val = c(15, 16), keepref = TRUE)
    df <- emr_extract("ph1", names = c("value"), keepref = TRUE, filter = "f1")
    emr_track.import("l1_ph", space = "global", categorical = TRUE, src = df)
    emr_track.logical.create("l1", "ph1", c(15, 16))

    emr_vtrack.create("vt_p", src = "l1_ph")
    emr_vtrack.create("vt_l", src = "l1")

    t1 <- emr_extract("vt_p", names = c("val"))
    t2 <- emr_extract("vt_l", names = c("val"))

    expect_equal(t1, t2)

    emr_vtrack.create("vt_p", src = "l1_ph", keepref = TRUE)
    emr_vtrack.create("vt_l", src = "l1", keepref = TRUE)

    t1 <- emr_extract("vt_p", names = c("val"), keepref = TRUE)
    t2 <- emr_extract("vt_l", names = c("val"), keepref = TRUE)

    expect_equal(t1, t2)

    emr_vtrack.create("vt_p", src = "l1_ph", keepref = FALSE)
    emr_vtrack.create("vt_l", src = "l1", keepref = FALSE)

    t1 <- emr_extract("vt_p", names = c("val"), keepref = TRUE)
    t2 <- emr_extract("vt_l", names = c("val"), keepref = TRUE)

    expect_equal(t1, t2)

    emr_vtrack.create("vt_p", src = "l1_ph", keepref = TRUE)
    emr_vtrack.create("vt_l", src = "l1", keepref = TRUE)

    t1 <- emr_extract("vt_p", names = c("val"), keepref = FALSE)
    t2 <- emr_extract("vt_l", names = c("val"), keepref = FALSE)

    expect_equal(t1, t2)

    withr::defer(emr_vtrack.rm("vt_p"))
    withr::defer(emr_vtrack.rm("vt_l"))
    withr::defer(emr_track.rm("l1_ph", force = TRUE))
})

test_that("emr_vtrack.create works on logical track without values", {
    emr_vtrack.clear()
    withr::defer(clean_logical_tracks())

    emr_track.logical.create("l1", src = "ph1", values = NULL)
    emr_vtrack.create("vt", src = "l1")

    t1 <- emr_extract("l1", names = c("val"))
    t2 <- emr_extract("vt", names = c("val"))

    expect_equal(t1, t2)

    emr_vtrack.create("vt", src = "l1", params = c(15, 16))
    emr_vtrack.create("vt_ph", src = "l1", params = c(15, 16))

    t1 <- emr_extract("l1", names = c("val"))
    t2 <- emr_extract("vt_ph", names = c("val"))

    expect_equal(t1, t2)
})


test_that("empty emr_vtrack.create works on numeric logical track with all keepref combinations", {
    emr_vtrack.clear()
    withr::defer(clean_logical_tracks())

    df <- emr_extract("track0", names = c("value"), keepref = TRUE)
    emr_track.import("l1_ph", space = "global", categorical = FALSE, src = df)
    emr_track.logical.create("l1", "track0")

    emr_vtrack.create("vt_p", src = "l1_ph")
    emr_vtrack.create("vt_l", src = "l1")

    t1 <- emr_extract("vt_p", names = c("val"))
    t2 <- emr_extract("vt_l", names = c("val"))

    expect_equal(t1, t2)

    emr_vtrack.create("vt_p", src = "l1_ph", keepref = TRUE)
    emr_vtrack.create("vt_l", src = "l1", keepref = TRUE)

    t1 <- emr_extract("vt_p", names = c("val"), keepref = TRUE)
    t2 <- emr_extract("vt_l", names = c("val"), keepref = TRUE)

    expect_equal(t1, t2)

    emr_vtrack.create("vt_p", src = "l1_ph", keepref = FALSE)
    emr_vtrack.create("vt_l", src = "l1", keepref = FALSE)

    t1 <- emr_extract("vt_p", names = c("val"), keepref = TRUE)
    t2 <- emr_extract("vt_l", names = c("val"), keepref = TRUE)

    expect_equal(t1, t2)

    emr_vtrack.create("vt_p", src = "l1_ph", keepref = TRUE)
    emr_vtrack.create("vt_l", src = "l1", keepref = TRUE)

    t1 <- emr_extract("vt_p", names = c("val"), keepref = FALSE)
    t2 <- emr_extract("vt_l", names = c("val"), keepref = FALSE)

    expect_equal(t1, t2)

    withr::defer(emr_vtrack.rm("vt_p"))
    withr::defer(emr_vtrack.rm("vt_l"))
    withr::defer(emr_track.rm("l1_ph", force = TRUE))
})

test_that("emr_vtrack functions work on numeric logical tracks as expected", {
    emr_vtrack.clear()
    withr::defer(clean_logical_tracks())

    df <- emr_extract("track0", names = c("value"), keepref = TRUE)
    emr_track.import("l1_ph", space = "global", categorical = FALSE, src = df)
    emr_track.logical.create("l1", "track0")

    emr_vtrack.create("vt_p", src = "l1_ph", func = "quantile", params = 0.5)
    emr_vtrack.create("vt_l", src = "l1", func = "quantile", params = 0.5)

    t1 <- emr_extract("vt_p", names = c("val"))
    t2 <- emr_extract("vt_l", names = c("val"))

    expect_equal(t1, t2)

    emr_vtrack.create("vt_p", src = "l1_ph", func = "stddev")
    emr_vtrack.create("vt_l", src = "l1", func = "stddev")

    t1 <- emr_extract("vt_p", names = c("val"))
    t2 <- emr_extract("vt_l", names = c("val"))

    expect_equal(t1, t2)

    emr_vtrack.create("vt_p", src = "l1_ph", func = "size")
    emr_vtrack.create("vt_l", src = "l1", func = "size")

    t1 <- emr_extract("vt_p", names = c("val"))
    t2 <- emr_extract("vt_l", names = c("val"))

    expect_equal(t1, t2)

    emr_vtrack.create("vt_p", src = "l1_ph", func = "lm.intercept")
    emr_vtrack.create("vt_l", src = "l1", func = "lm.intercept")

    t1 <- emr_extract("vt_p", names = c("val"))
    t2 <- emr_extract("vt_l", names = c("val"))

    expect_equal(t1, t2)

    withr::defer(emr_vtrack.rm("vt_p"))
    withr::defer(emr_vtrack.rm("vt_l"))
    withr::defer(emr_track.rm("l1_ph", force = TRUE))
})


test_that("emr_vtrack functions work on logical tracks as expected", {
    emr_vtrack.clear()
    withr::defer(clean_logical_tracks())

    emr_filter.create("f1", src = "ph1", val = c(15, 16), keepref = TRUE)
    df <- emr_extract("ph1", names = c("value"), keepref = TRUE, filter = "f1")
    emr_track.import("l1_ph", space = "global", categorical = TRUE, src = df)
    emr_track.logical.create("l1", "ph1", c(15, 16))

    emr_vtrack.create("vt_p", src = "l1_ph", params = c(15))
    emr_vtrack.create("vt_l", src = "l1", params = c(15))

    t1 <- emr_extract("vt_p", names = c("val"))
    t2 <- emr_extract("vt_l", names = c("val"))

    expect_equal(t1, t2)

    emr_vtrack.create("vt_p", src = "l1_ph", func = "closest.earlier.time", params = c(15))
    emr_vtrack.create("vt_l", src = "l1", func = "closest.earlier.time", params = c(15))

    t1 <- emr_extract("vt_p", names = c("val"))
    t2 <- emr_extract("vt_l", names = c("val"))

    expect_equal(t1, t2)

    # will not work with 'exists'!!!
    emr_vtrack.create("vt_p", src = "l1_ph", func = "size", params = c(19))
    emr_vtrack.create("vt_l", src = "l1", func = "size", params = c(19))

    t1 <- emr_extract("vt_p", names = c("val"))
    t2 <- emr_extract("vt_l", names = c("val"))

    expect_equal(t1, t2)

    emr_vtrack.create("vt_p", src = "l1_ph", func = "earliest.time", time.shift = c(-100, 50))
    emr_vtrack.create("vt_l", src = "l1", func = "earliest.time", time.shift = c(-100, 50))

    t1 <- emr_extract("vt_p", names = c("val"))
    t2 <- emr_extract("vt_l", names = c("val"))

    expect_equal(t1, t2)

    withr::defer(emr_vtrack.rm("vt_p"))
    withr::defer(emr_vtrack.rm("vt_l"))
    withr::defer(emr_track.rm("l1_ph", force = TRUE))
})

test_that("emr_vtrack functions work on numeric logical tracks with keepref combinations", {
    emr_vtrack.clear()
    withr::defer(clean_logical_tracks())

    df <- emr_extract("track0", names = c("value"), keepref = TRUE)
    emr_track.import("l1_ph", space = "global", categorical = FALSE, src = df)
    emr_track.logical.create("l1", "track0")

    emr_vtrack.create("vt_p", src = "l1_ph", func = "closest.earlier.time", keepref = TRUE)
    emr_vtrack.create("vt_l", src = "l1", func = "closest.earlier.time", keepref = TRUE)

    t1 <- emr_extract("vt_p", names = c("val"), keepref = FALSE)
    t2 <- emr_extract("vt_l", names = c("val"), keepref = FALSE)

    expect_equal(t1, t2)

    emr_vtrack.create("vt_p", src = "l1_ph", func = "lm.slope", keepref = FALSE)
    emr_vtrack.create("vt_l", src = "l1", func = "lm.slope", keepref = FALSE)

    t1 <- emr_extract("vt_p", names = c("val"), keepref = TRUE)
    t2 <- emr_extract("vt_l", names = c("val"), keepref = TRUE)

    expect_equal(t1, t2)

    emr_vtrack.create("vt_p", src = "l1_ph", func = "earliest.time", time.shift = c(-100, 50))
    emr_vtrack.create("vt_l", src = "l1", func = "earliest.time", time.shift = c(-100, 50))

    t1 <- emr_extract("vt_p", names = c("val"), keepref = TRUE)
    t2 <- emr_extract("vt_l", names = c("val"), keepref = TRUE)

    expect_equal(t1, t2)

    withr::defer(emr_vtrack.rm("vt_p"))
    withr::defer(emr_vtrack.rm("vt_l"))
    withr::defer(emr_track.rm("l1_ph", force = TRUE))
})


test_that("emr_vtrack functions work on logical tracks with keepref combinations", {
    emr_vtrack.clear()
    withr::defer(clean_logical_tracks())

    emr_filter.create("f1", src = "ph1", val = c(15, 16), keepref = TRUE)
    df <- emr_extract("ph1", names = c("value"), keepref = TRUE, filter = "f1")
    emr_track.import("l1_ph", space = "global", categorical = TRUE, src = df)
    emr_track.logical.create("l1", "ph1", c(15, 16))

    emr_vtrack.create("vt_p", src = "l1_ph", func = "closest.earlier.time", params = c(15), keepref = TRUE)
    emr_vtrack.create("vt_l", src = "l1", func = "closest.earlier.time", params = c(15), keepref = TRUE)

    t1 <- emr_extract("vt_p", names = c("val"), keepref = FALSE)
    t2 <- emr_extract("vt_l", names = c("val"), keepref = FALSE)

    expect_equal(t1, t2)

    emr_vtrack.create("vt_p", src = "l1_ph", func = "frequent", time.shift = c(-100, 200), keepref = FALSE)
    emr_vtrack.create("vt_l", src = "l1", func = "frequent", time.shift = c(-100, 200), keepref = FALSE)

    t1 <- emr_extract("vt_p", names = c("val"), keepref = TRUE)
    t2 <- emr_extract("vt_l", names = c("val"), keepref = TRUE)

    expect_equal(t1, t2)

    emr_vtrack.create("vt_p", src = "l1_ph", func = "value", params = 16, keepref = TRUE)
    emr_vtrack.create("vt_l", src = "l1", func = "value", params = 16, keepref = TRUE)

    t1 <- emr_extract("vt_p", names = c("val"), keepref = TRUE)
    t2 <- emr_extract("vt_l", names = c("val"), keepref = TRUE)

    expect_equal(t1, t2)

    withr::defer(emr_vtrack.rm("vt_p"))
    withr::defer(emr_vtrack.rm("vt_l"))
    withr::defer(emr_track.rm("l1_ph", force = TRUE))
})

test_that("emr_vtrack.create with filter works on numeric logical tracks", {
    emr_vtrack.clear()
    emr_filter.clear()
    withr::defer(clean_logical_tracks())

    df <- emr_extract("track0", names = c("value"), keepref = TRUE)
    emr_track.import("l1_ph", space = "global", categorical = FALSE, src = df)
    emr_track.logical.create("l1", "track0")

    emr_filter.create("f1", src = "track0")

    emr_vtrack.create("vt_p", src = "l1_ph", filter = "f1")
    emr_vtrack.create("vt_l", src = "l1", filter = "f1")

    t1 <- emr_extract("vt_p", names = c("val"))
    t2 <- emr_extract("vt_l", names = c("val"))

    expect_equal(t1, t2)

    emr_filter.create("f1", src = "l1_ph")
    emr_filter.create("f2", src = "l1")

    emr_vtrack.create("vt_p", src = "l1_ph", filter = "f1 && !f1")
    emr_vtrack.create("vt_l", src = "l1", filter = "f2 && !f2")

    t1 <- emr_extract("vt_p", names = c("val"))
    t2 <- emr_extract("vt_l", names = c("val"))

    expect_equal(t1, t2)

    emr_filter.create("f1", src = "l1_ph")
    emr_filter.create("f2", src = "l1")

    emr_vtrack.create("vt_p", src = "l1_ph", func = "closest", filter = "f1")
    emr_vtrack.create("vt_l", src = "l1", func = "closest", filter = "f2")

    t1 <- emr_extract("vt_p", names = c("val"))
    t2 <- emr_extract("vt_l", names = c("val"))

    expect_equal(t1, t2)

    emr_filter.create("f1", src = "l1_ph")
    emr_filter.create("f2", src = "l1")

    emr_vtrack.create("vt_p", src = "l1_ph", func = "closest", filter = "f1")
    emr_vtrack.create("vt_l", src = "l1", func = "closest", filter = "f2")

    t1 <- emr_extract("vt_p", names = c("val"), keepref = TRUE)
    t2 <- emr_extract("vt_l", names = c("val"), keepref = TRUE)

    expect_equal(t1, t2)

    emr_filter.create("f1", src = "l1_ph")
    emr_filter.create("f2", src = "l1")

    emr_vtrack.create("vt_p", src = "l1_ph", func = "closest", filter = "f1")
    emr_vtrack.create("vt_l", src = "l1", func = "closest", filter = "f2")

    t1 <- emr_extract("vt_p", names = c("val"), keepref = TRUE)
    t2 <- emr_extract("vt_l", names = c("val"), keepref = TRUE)

    expect_equal(t1, t2)

    withr::defer(emr_vtrack.rm("vt_p"))
    withr::defer(emr_vtrack.rm("vt_l"))
    withr::defer(emr_track.rm("l1_ph", force = TRUE))
})

test_that("emr_vtrack.create with filter works on logical tracks", {
    emr_vtrack.clear()
    emr_filter.clear()
    withr::defer(clean_logical_tracks())

    emr_filter.create("f1", src = "ph1", val = seq(4, 16, 1), keepref = TRUE)
    df <- emr_extract("ph1", names = c("value"), keepref = TRUE, filter = "f1")
    emr_track.import("l1_ph", space = "global", categorical = TRUE, src = df)
    emr_track.logical.create("l1", "ph1", seq(4, 16, 1))

    emr_filter.create("f1", src = "ph1", val = c(15, 16))

    emr_vtrack.create("vt_p", src = "l1_ph", filter = "f1")
    emr_vtrack.create("vt_l", src = "l1", filter = "f1")

    t1 <- emr_extract("vt_p", names = c("val"))
    t2 <- emr_extract("vt_l", names = c("val"))

    expect_equal(t1, t2)

    emr_filter.create("f1", src = "l1_ph", val = c(4))
    emr_filter.create("f2", src = "l1", val = c(4))

    emr_vtrack.create("vt_p", src = "l1_ph", filter = "f1")
    emr_vtrack.create("vt_l", src = "l1", filter = "f2")

    t1 <- emr_extract("vt_p", names = c("val"))
    t2 <- emr_extract("vt_l", names = c("val"))

    expect_equal(t1, t2)

    emr_filter.create("f1", src = "l1_ph", val = c(4, 5))
    emr_filter.create("f2", src = "l1", val = c(4, 5))

    emr_vtrack.create("vt_p", src = "l1_ph", func = "closest", params = c(15), filter = "f1")
    emr_vtrack.create("vt_l", src = "l1", func = "closest", params = c(15), filter = "f2")

    t1 <- emr_extract("vt_p", names = c("val"))
    t2 <- emr_extract("vt_l", names = c("val"))

    expect_equal(t1, t2)

    emr_filter.create("f1", src = "l1_ph", val = c(4, 5))
    emr_filter.create("f2", src = "l1", val = c(4, 5))

    emr_vtrack.create("vt_p", src = "l1_ph", func = "closest", params = c(19), filter = "f1")
    emr_vtrack.create("vt_l", src = "l1", func = "closest", params = c(19), filter = "f2")

    t1 <- emr_extract("vt_p", names = c("val"), keepref = TRUE)
    t2 <- emr_extract("vt_l", names = c("val"), keepref = TRUE)

    expect_equal(t1, t2)

    emr_filter.create("f1", src = "l1_ph", val = c(19))
    emr_filter.create("f2", src = "l1", val = c(19))

    emr_vtrack.create("vt_p", src = "l1_ph", func = "closest", filter = "f1")
    emr_vtrack.create("vt_l", src = "l1", func = "closest", filter = "f2")

    t1 <- emr_extract("vt_p", names = c("val"), keepref = TRUE)
    t2 <- emr_extract("vt_l", names = c("val"), keepref = TRUE)

    expect_equal(t1, t2)

    withr::defer(emr_vtrack.rm("vt_p"))
    withr::defer(emr_vtrack.rm("vt_l"))
    withr::defer(emr_track.rm("l1_ph", force = TRUE))
})

test_that("emr_vtrack works on numeric logical tracks with filter on extract", {
    emr_vtrack.clear()
    emr_filter.clear()
    withr::defer(clean_logical_tracks())

    df <- emr_extract("track0", names = c("value"), keepref = TRUE)
    emr_track.import("l1_ph", space = "global", categorical = FALSE, src = df)
    emr_track.logical.create("l1", "track0")

    emr_filter.create("f1", src = "l1_ph")
    emr_filter.create("f2", src = "l1")

    emr_vtrack.create("vt_p", src = "l1_ph")
    emr_vtrack.create("vt_l", src = "l1")

    t1 <- emr_extract("vt_p", names = c("val"), filter = "f1")
    t2 <- emr_extract("vt_l", names = c("val"), filter = "f2")

    expect_equal(t1, t2)

    emr_filter.create("f1", src = "l1_ph")
    emr_filter.create("f2", src = "l1")
    emr_filter.create("f3", src = "track0")

    emr_vtrack.create("vt_p", src = "l1_ph", func = "closest", filter = "f1")
    emr_vtrack.create("vt_l", src = "l1", func = "closest", filter = "f2")

    t1 <- emr_extract("vt_p", names = c("val"), filter = "f3", keepref = TRUE)
    t2 <- emr_extract("vt_l", names = c("val"), filter = "f3", keepref = TRUE)

    expect_equal(t1, t2)

    withr::defer(emr_vtrack.rm("vt_p"))
    withr::defer(emr_vtrack.rm("vt_l"))
    withr::defer(emr_track.rm("l1_ph", force = TRUE))
})

test_that("emr_vtrack works on logical tracks with filter on extract", {
    emr_vtrack.clear()
    emr_filter.clear()
    withr::defer(clean_logical_tracks())

    emr_filter.create("f1", src = "ph1", val = seq(4, 16, 1), keepref = TRUE)
    df <- emr_extract("ph1", names = c("value"), keepref = TRUE, filter = "f1")
    emr_track.import("l1_ph", space = "global", categorical = TRUE, src = df)
    emr_track.logical.create("l1", "ph1", seq(4, 16, 1))

    emr_filter.create("f1", src = "l1_ph", val = c(15, 16))
    emr_filter.create("f2", src = "l1", val = c(15, 16))

    emr_vtrack.create("vt_p", src = "l1_ph")
    emr_vtrack.create("vt_l", src = "l1")

    t1 <- emr_extract("vt_p", names = c("val"), filter = "f1")
    t2 <- emr_extract("vt_l", names = c("val"), filter = "f2")

    expect_equal(t1, t2)

    emr_filter.create("f1", src = "l1_ph", val = c(5, 10, 30))
    emr_filter.create("f2", src = "l1", val = c(5, 10, 30))
    emr_filter.create("f3", src = "ph1", val = c(16))

    emr_vtrack.create("vt_p", src = "l1_ph", func = "closest", filter = "f1")
    emr_vtrack.create("vt_l", src = "l1", func = "closest", filter = "f2")

    t1 <- emr_extract("vt_p", names = c("val"), filter = "f3")
    t2 <- emr_extract("vt_l", names = c("val"), filter = "f3")

    expect_equal(t1, t2)

    emr_filter.create("f1", src = "l1_ph", val = c(5, 10, 30))
    emr_filter.create("f2", src = "l1", val = c(5, 10, 30))
    emr_filter.create("f3", src = "ph1", val = c(16))

    emr_vtrack.create("vt_p", src = "l1_ph", func = "closest", filter = "f1")
    emr_vtrack.create("vt_l", src = "l1", func = "closest", filter = "f2")

    t1 <- emr_extract("vt_p", names = c("val"), filter = "f3", keepref = TRUE)
    t2 <- emr_extract("vt_l", names = c("val"), filter = "f3", keepref = TRUE)

    expect_equal(t1, t2)

    withr::defer(emr_vtrack.rm("vt_p"))
    withr::defer(emr_vtrack.rm("vt_l"))
    withr::defer(emr_track.rm("l1_ph", force = TRUE))
})

# emr_vtrack.attr.src

test_that("emr_vtrack.attr.src works with vtracks on logical tracks", {
    emr_vtrack.clear()
    withr::defer(clean_logical_tracks())

    emr_filter.create("f1", src = "ph1", val = seq(4, 16, 1), keepref = TRUE)
    df <- emr_extract("ph1", names = c("value"), keepref = TRUE, filter = "f1")
    emr_track.import("l2_ph", space = "global", categorical = TRUE, src = df)

    emr_track.logical.create("l1", "ph1", c(15, 16))
    emr_track.logical.create("l2", "ph1", seq(4, 16, 1))

    emr_vtrack.create("vt1", src = "l1")
    src <- emr_vtrack.attr.src("vt1")

    expect_equal(src, "l1")

    emr_vtrack.attr.src("vt1", "l2")
    src <- emr_vtrack.attr.src("vt1")

    expect_equal(src, "l2")

    emr_vtrack.create("vt1", src = "l1", func = "value", params = 16, keepref = TRUE)

    emr_vtrack.attr.src("vt1", "l2")
    t1 <- emr_extract("vt1")

    emr_vtrack.attr.src("vt1", "l2_ph")
    t2 <- emr_extract("vt1")

    expect_equal(t1, t2)

    withr::defer(emr_vtrack.rm("vt_1"))
    withr::defer(emr_track.rm("l1_ph", force = TRUE))
    withr::defer(emr_track.rm("l2_ph", force = TRUE))
})

# emr_vtrack.attr.params

test_that("emr_vtrack.attr.params works on logical tracks", {
    emr_vtrack.clear()
    withr::defer(clean_logical_tracks())

    emr_filter.create("f1", src = "ph1", val = seq(4, 16, 1), keepref = TRUE)
    df <- emr_extract("ph1", names = c("value"), keepref = TRUE, filter = "f1")
    emr_track.import("l1_ph", space = "global", categorical = TRUE, src = df)

    emr_track.logical.create("l1", "ph1", seq(4, 16, 1))

    emr_vtrack.create("vt_p", src = "l1_ph", func = "closest", params = c(15))
    emr_vtrack.create("vt_l", src = "l1", func = "closest", params = c(15))

    params <- emr_vtrack.attr.params("vt_l")

    expect_equal(params, c(15))

    p_params <- emr_vtrack.attr.params("vt_p")
    l_params <- emr_vtrack.attr.params("vt_l")

    expect_equal(p_params, l_params)

    emr_vtrack.create("vt_p1", src = "l1_ph", func = "closest", params = c(10, 16, 30))
    emr_vtrack.attr.params("vt_l", c(10, 16, 30))

    t1 <- emr_extract("vt_p1", names = c("val"))
    t2 <- emr_extract("vt_l", names = c("val"))

    expect_equal(t1, t2)

    withr::defer(emr_track.rm("l1_ph", force = TRUE))
})

test_that("filter on vtrack based on logical track", {
    emr_filter.clear()
    emr_vtrack.clear()
    withr::defer(clean_logical_tracks())

    emr_track.logical.create("l1", src = "track0")

    emr_vtrack.create("vt", src = "l1", func = "min", time.shift = c(-30, 0))
    f <- emr_filter.create(filter = NULL, src = "vt", val = 700, operator = ">")
    iter <- emr_screen("vt > 700")
    a <- emr_extract(c("l1", "vt"), iterator = iter)
    d <- emr_extract(c("track0", "vt"), iterator = iter, names = c("l1", "vt"))
    b <- emr_extract(c("l1", "vt"), iterator = "l1", filter = f)
    expect_equal(a, b)
    expect_equal(b, d)
})

test_that("filter on vtrack based on logical track with values", {
    emr_filter.clear()
    emr_vtrack.clear()
    withr::defer(clean_logical_tracks())

    emr_track.logical.create("l1", src = "ph1", values = c(15, 16))

    emr_vtrack.create("vt", src = "l1", time.shift = c(-30, 0))
    f <- emr_filter.create(filter = NULL, src = "vt", val = 15)
    iter <- emr_screen("vt == 15")
    a <- emr_extract(c("l1", "vt"), iterator = iter)
    b <- emr_extract(c("l1", "vt"), iterator = "l1", filter = f)
    expect_equal(a, b)
})
