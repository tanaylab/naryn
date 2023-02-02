load_test_db()

# not that the test db has a missing patient from 'patients.dob' track (2510). It only exists in 'ph1' track.
test_that("emr_track.import from data frame works", {
    a <- emr_extract("track1", keepref = TRUE, names = "value")
    emr_track.import("temp_track", "global", categorical = FALSE, src = a)
    withr::defer(emr_track.rm("temp_track", force = TRUE))
    b <- emr_extract("temp_track", keepref = TRUE, names = "value")
    expect_equal(a, b)

    a <- emr_extract("track1", keepref = TRUE, names = "value")
    emr_track.import("u_temp_track", "user", categorical = FALSE, src = a)
    withr::defer(emr_track.rm("u_temp_track", force = TRUE))
    b <- emr_extract("u_temp_track", keepref = TRUE, names = "value")
    expect_equal(a, b)

    a <- emr_extract("track6", keepref = TRUE, names = "value")
    emr_track.import("temp_track1", "global", categorical = TRUE, src = a)
    withr::defer(emr_track.rm("temp_track1", force = TRUE))
    b <- emr_extract("temp_track1", keepref = TRUE, names = "value")
    expect_equal(a, b)


    a <- emr_extract("track6", keepref = TRUE, names = "value")
    emr_track.import("u_temp_track1", "user", categorical = TRUE, src = a)
    withr::defer(emr_track.rm("u_temp_track1", force = TRUE))
    b <- emr_extract("u_temp_track1", keepref = TRUE, names = "value")
    expect_equal(a, b)
})

test_that("emr_track.import from file works", {
    a <- emr_extract("track1", keepref = TRUE, names = "value")
    fn <- tempfile()
    readr::write_tsv(a, fn, col_names = FALSE)
    emr_track.import("temp_track", "global", categorical = FALSE, src = fn)
    withr::defer(emr_track.rm("temp_track", force = TRUE))
    b <- emr_extract("temp_track", keepref = TRUE, names = "value")
    expect_equal(a, b)

    a <- emr_extract("track1", keepref = TRUE, names = "value")
    fn <- tempfile()
    readr::write_tsv(a, fn, col_names = FALSE)
    emr_track.import("u_temp_track", "user", categorical = FALSE, src = fn)
    withr::defer(emr_track.rm("u_temp_track", force = TRUE))
    b <- emr_extract("u_temp_track", keepref = TRUE, names = "value")
    expect_equal(a, b)

    a <- emr_extract("track6", keepref = TRUE, names = "value")
    fn <- tempfile()
    readr::write_tsv(a, fn, col_names = FALSE)
    emr_track.import("temp_track1", "global", categorical = TRUE, src = fn)
    withr::defer(emr_track.rm("temp_track1", force = TRUE))
    b <- emr_extract("temp_track1", keepref = TRUE, names = "value")
    expect_equal(a, b)


    a <- emr_extract("track6", keepref = TRUE, names = "value")
    fn <- tempfile()
    readr::write_tsv(a, fn, col_names = FALSE)
    emr_track.import("u_temp_track1", "user", categorical = TRUE, src = fn)
    withr::defer(emr_track.rm("u_temp_track1", force = TRUE))
    b <- emr_extract("u_temp_track1", keepref = TRUE, names = "value")
    expect_equal(a, b)
})

test_that("emr_track.addto works with data frame", {
    a <- emr_extract("track1", keepref = TRUE, names = "value")
    a1 <- a[1:250000, ]
    a2 <- a[250001:500000, ]
    emr_track.import("temp_track", "global", categorical = FALSE, src = a1)
    withr::defer(emr_track.rm("temp_track", force = TRUE))
    emr_track.addto("temp_track", a2)
    b <- emr_extract("temp_track", keepref = TRUE, names = "value")
    expect_equal(a, b)
})

test_that("emr_track.addto works with file", {
    a <- emr_extract("track1", keepref = TRUE, names = "value")
    a1 <- a[1:250000, ]
    a2 <- a[250001:500000, ]
    fn1 <- tempfile()
    fn2 <- tempfile()
    readr::write_tsv(a1, fn1, col_names = FALSE)
    readr::write_tsv(a2, fn2, col_names = FALSE)
    emr_track.import("temp_track", "global", categorical = FALSE, src = fn1)
    withr::defer(emr_track.rm("temp_track", force = TRUE))
    emr_track.addto("temp_track", fn2)
    b <- emr_extract("temp_track", keepref = TRUE, names = "value")
    expect_equal(a, b)
})

test_that("emr_track.addto fails when time points already exist with different values", {
    a <- emr_extract("track1", keepref = TRUE, names = "value")
    b <- a
    b$value[1] <- 15
    emr_track.import("temp_track", "global", categorical = FALSE, src = a)
    withr::defer(emr_track.rm("temp_track", force = TRUE))
    expect_error(emr_track.addto("temp_track", b))
})

test_that("emr_track.addto works when time points already exist with the same value", {
    a <- emr_extract("track1", keepref = TRUE, names = "value")
    emr_track.import("temp_track", "global", categorical = FALSE, src = a)
    withr::defer(emr_track.rm("temp_track", force = TRUE))
    emr_track.addto("temp_track", a[1:2, ])
    b <- emr_extract("temp_track", keepref = TRUE, names = "value")

    expect_equal(a, b)

    old <- brio::read_file_raw(file.path(.naryn$EMR_GROOT, "track1.nrtrack"))
    new <- brio::read_file_raw(file.path(.naryn$EMR_GROOT, "temp_track.nrtrack"))

    expect_identical(
        old,
        new
    )
})

test_that("creating a virtual track with duplicate values still fails", {
    a <- emr_extract("track1", names = "value")
    b <- rbind(a[1:3, ], a[1:3, ])
    emr_vtrack.create("vt", src = list(b, FALSE))
    expect_error(emr_extract("vt"))
    expect_error(emr_extract("vt", iterator = "track1"))
})

# test remove unknown with a data frame

test_that("emr_track.import failes with unknown id", {
    withr::defer(emr_track.rm("test", force = TRUE))
    expect_error(emr_track.import("test", .naryn$EMR_GROOT, categorical = TRUE, src = data.frame(id = 2000, time = 5, value = 1)))
    expect_true(!emr_track.exists("test"))
})

test_that("emr_track.import failes with multiple unknown ids", {
    a <- emr_extract("track1", keepref = TRUE, names = "value")
    a <- rbind(a, data.frame(id = 2000, time = 5, ref = 0, value = 1))
    a <- rbind(a, data.frame(id = 3000, time = 6, ref = 0, value = 1))

    withr::defer(emr_track.rm("test", force = TRUE))
    expect_error(emr_track.import("test", .naryn$EMR_GROOT, categorical = TRUE, src = a))
    expect_true(!emr_track.exists("test"))
})

test_that("emr_track.import works when remove_unknown is TRUE", {
    a <- emr_extract("track1", keepref = TRUE, names = "value")
    a <- rbind(a, data.frame(id = 2000, time = 5, ref = 0, value = 1))
    a <- rbind(a, data.frame(id = 3000, time = 6, ref = 0, value = 1))

    withr::defer(emr_track.rm("test", force = TRUE))
    emr_track.import("test", .naryn$EMR_GROOT, categorical = TRUE, src = a, remove_unknown = TRUE)
    expect_true(emr_track.exists("test"))
    expect_true(all(!(c(2000, 3000) %in% emr_extract("test")$id)))
})

# test remove unknown with a file
test_that("emr_track.import failes with unknown id", {
    fn <- tempfile()
    readr::write_tsv(data.frame(id = 2000, time = 5, ref = 0, value = 1), fn, col_names = FALSE)
    withr::defer(emr_track.rm("test", force = TRUE))
    expect_error(emr_track.import("test", .naryn$EMR_GROOT, categorical = TRUE, src = fn))
    expect_true(!emr_track.exists("test"))
})

test_that("emr_track.import failes with multiple unknown ids", {
    a <- emr_extract("track1", keepref = TRUE, names = "value")
    a <- rbind(a, data.frame(id = 2000, time = 5, ref = 0, value = 1))
    a <- rbind(a, data.frame(id = 3000, time = 6, ref = 0, value = 1))

    fn <- tempfile()
    readr::write_tsv(a, fn, col_names = FALSE)

    withr::defer(emr_track.rm("test", force = TRUE))
    expect_error(emr_track.import("test", .naryn$EMR_GROOT, categorical = TRUE, src = fn))
    expect_true(!emr_track.exists("test"))
})

test_that("emr_track.import works when remove_unknown is TRUE", {
    a <- emr_extract("track1", keepref = TRUE, names = "value")
    a <- rbind(a, data.frame(id = 2000, time = 5, ref = 0, value = 1))
    a <- rbind(a, data.frame(id = 3000, time = 6, ref = 0, value = 1))

    fn <- tempfile()
    readr::write_tsv(a, fn, col_names = FALSE)

    withr::defer(emr_track.rm("test", force = TRUE))
    emr_track.import("test", .naryn$EMR_GROOT, categorical = TRUE, src = fn, remove_unknown = TRUE)
    expect_true(emr_track.exists("test"))
    expect_true(all(!(c(2000, 3000) %in% emr_extract("test")$id)))
})


test_that("emr_track.addto fails with unknown ids", {
    a <- emr_extract("track1", keepref = TRUE, names = "value")
    emr_track.import("temp_track", "global", categorical = FALSE, src = a)
    withr::defer(emr_track.rm("temp_track", force = TRUE))
    expect_error(emr_track.addto("temp_track", data.frame(id = 2000, time = 5, ref = 0, value = 1)))
})

test_that("emr_track.addto works with remove_unknown", {
    a <- emr_extract("track1", keepref = TRUE, names = "value")
    emr_track.import("temp_track", "global", categorical = FALSE, src = a)
    withr::defer(emr_track.rm("temp_track", force = TRUE))
    emr_track.addto("temp_track", data.frame(id = 2000, time = 5, ref = 0, value = 1), remove_unknown = TRUE)
    expect_true(all(!(c(2000) %in% emr_extract("temp_track")$id)))
})

test_that("emr_track.addto fails with unknown ids with a file", {
    a <- emr_extract("track1", keepref = TRUE, names = "value")
    fn <- tempfile()
    readr::write_tsv(a, fn, col_names = FALSE)
    emr_track.import("temp_track", "global", categorical = FALSE, src = fn)
    withr::defer(emr_track.rm("temp_track", force = TRUE))
    expect_error(emr_track.addto("temp_track", data.frame(id = 2000, time = 5, ref = 0, value = 1)))
})

test_that("emr_track.addto works with remove_unknown with a file", {
    a <- emr_extract("track1", keepref = TRUE, names = "value")
    fn <- tempfile()
    readr::write_tsv(a, fn, col_names = FALSE)
    emr_track.import("temp_track", "global", categorical = FALSE, src = fn)
    withr::defer(emr_track.rm("temp_track", force = TRUE))
    emr_track.addto("temp_track", data.frame(id = 2000, time = 5, ref = 0, value = 1), remove_unknown = TRUE)
    expect_true(all(!(c(2000) %in% emr_extract("temp_track")$id)))
})
