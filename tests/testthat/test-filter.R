test_that("filter works", {
    EMR_FILTERS <<- list()
    fname <- emr_filter.create("f1", "track1", keepref = T)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = T, filter = "f1"), "filter.1")
    expect_equal(fname, "f1")
})

test_that("time.shift is not allowed when keepref is TRUE", {
    expect_error(emr_filter.create("f1", "track1", keepref = T, time.shift = 10))
})

test_that("'val' parameter can be used only with categorical tracks", {
    expect_error(emr_filter.create("f1", "track1", val = c(3, 5)))
})

test_that("filter works with time shift", {
    EMR_FILTERS <<- list()
    emr_filter.create("f1", "track8", val = c(3, 5), time.shift = c(-10, 10))
    expect_regression(emr_extract("track7", keepref = T, filter = "f1"), "filter.2")
})

test_that("'expiration' must be a positive integer", {
    expect_error(emr_filter.create("f1", "track8", expiration = -10))
})

test_that("'expiration' cannot be used when keepref is 'TRUE'", {
    expect_error(emr_filter.create("f1", "track8", keepref = T, expiration = 100))
})

test_that("filter works with keepref", {
    EMR_FILTERS <<- list()
    emr_filter.create("f1", "track8", expiration = 100)
    expect_regression(emr_extract("track7", keepref = T, filter = "f1"), "filter.3")
})

test_that("filter works with keepref and val", {
    EMR_FILTERS <<- list()
    emr_filter.create("f1", "track8", expiration = 100, val = c(3, 5))
    expect_regression(emr_extract("track7", keepref = T, filter = "f1"), "filter.4")
})

test_that("filter works with keepref and time shift", {
    EMR_FILTERS <<- list()
    emr_filter.create("f1", "track1", keepref = F, time.shift = 10)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = T, filter = "f1"), "filter.5")
})

test_that("filter works with keepref and time shift and stime etime #1", {
    EMR_FILTERS <<- list()
    emr_filter.create("f1", "track1", keepref = F, time.shift = c(10, 20))
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = T, filter = "f1"), "filter.6")
})

test_that("use a track as filter", {
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = T, filter = "!track1"), "filter.7")
})

test_that("use multiple tracks as filter", {
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = T, filter = "!track0 | !track1"), "filter.8")
})

test_that("filter works with keepref and time shift and stime etime #1", {
    EMR_FILTERS <<- list()
    emr_filter.create("f1", "track1", keepref = T)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = T, filter = "!f1"), "filter.9")
})

test_that("use multiple tracks as filter", {
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = T, filter = "track0 & track1"), "filter.10")
})

test_that("filter works", {
    EMR_FILTERS <<- list()
    emr_filter.create("f1", "track0", keepref = T)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = T, filter = "f1 & track1"), "filter.11")
})

test_that("filter works", {
    EMR_FILTERS <<- list()
    emr_filter.create("f1", "track0", keepref = T)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = F, filter = "(f1 | track1) & !track5"), "filter.12")
})

test_that("filter works", {
    EMR_FILTERS <<- list()
    emr_filter.create("f1", "track0", keepref = T)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = T, filter = "(f1 | track1) & !track5"), "filter.13")
})

test_that("filter works", {
    EMR_FILTERS <<- list()
    emr_filter.create("f1", "track0_sparse", keepref = T)
    expect_regression(emr_extract("track2_sparse", stime = 10, etime = 1000, keepref = T, filter = "(f1 | track1) & !track5_sparse"), "filter.14")
})

test_that("Id-time list contains two or more points that differ only by reference", {
    i1 <- emr_extract("track1", keepref = T)
    expect_error(emr_extract("2 * track2", stime = 10, etime = 1000, keepref = T, filter = "i1"))
})

test_that("filter works", {
    i1 <- emr_extract("track1", keepref = F)
    expect_regression(emr_extract("2 * track2", stime = 10, etime = 1000, keepref = T, filter = "i1"), "filter.15")
})

test_that("filter works", {
    i1 <- emr_extract("track1_sparse", keepref = F)
    expect_regression(emr_extract("track2_sparse", stime = 10, etime = 1000, keepref = T, filter = "i1"), "filter.16")
})

test_that("filter works", {
    EMR_FILTERS <<- list()
    i1 <- emr_extract("track1", keepref = T)
    emr_filter.create("f1", i1, keepref = T)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = T, filter = "f1"), "filter.17")
})

test_that("filter works", {
    EMR_FILTERS <<- list()
    i1 <- emr_extract("track1", keepref = F)
    emr_filter.create("f1", i1, keepref = T)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = T, filter = "f1"), "filter.18")
})

test_that("'time.shift' is not allowed when keepref is 'TRUE'", {
    EMR_FILTERS <<- list()
    i1 <- emr_extract("track1", keepref = T)
    expect_error(emr_filter.create("f1", i1, keepref = T, time.shift = 10))
    expect_error(emr_extract("track2", stime = 10, etime = 1000, keepref = T, filter = "f1"))
})

test_that("filter works", {
    EMR_FILTERS <<- list()
    i1 <- emr_extract("track1", keepref = F)
    emr_filter.create("f1", i1, keepref = F, time.shift = 10)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = T, filter = "f1"), "filter.19")
})

test_that("filter works", {
    EMR_FILTERS <<- list()
    i1 <- emr_extract("track1", keepref = F)
    emr_filter.create("f1", i1, keepref = F, time.shift = c(10, 20))
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = T, filter = "f1"), "filter.20")
})

test_that("filter works", {
    i1 <- emr_extract("track1", keepref = F)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = T, filter = "!i1"), "filter.21")
})

test_that("filter works", {
    EMR_FILTERS <<- list()
    i1 <- emr_extract("track1", keepref = T)
    emr_filter.create("f1", i1, keepref = T)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = T, filter = "!f1"), "filter.22")
})

test_that("filter works", {
    i0 <- emr_extract("track0", keepref = F)
    i1 <- emr_extract("track1", keepref = F)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = T, filter = "i0 & i1"), "filter.23")
})

test_that("filter works", {
    EMR_FILTERS <<- list()
    i0 <- emr_extract("track0", keepref = T)
    i1 <- emr_extract("track1", keepref = F)
    emr_filter.create("f1", i0, keepref = T)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = T, filter = "f1 & i1"), "filter.24")
})

test_that("filter works", {
    EMR_FILTERS <<- list()
    i0 <- emr_extract("track0", keepref = T)
    i1 <- emr_extract("track1", keepref = F)
    emr_filter.create("f1", i0, keepref = T)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = T, filter = "f1 | i1"), "filter.25")
})

test_that("filter works", {
    EMR_FILTERS <<- list()
    i0 <- emr_extract("track0", keepref = T)
    i1 <- emr_extract("track1", keepref = F)
    emr_filter.create("f1", i0, keepref = T)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = F, filter = "(f1 | i1) & !track5"), "filter.26")
})

test_that("filter works", {
    EMR_FILTERS <<- list()
    i0 <- emr_extract("track0", keepref = T)
    i1 <- emr_extract("track1", keepref = F)
    emr_filter.create("f1", i0, keepref = T)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = T, filter = "(f1 | i1) & !track5"), "filter.27")
})

test_that("filter works", {
    EMR_FILTERS <<- list()
    i1 <- emr_extract("track1", keepref = F)
    emr_filter.create("f1", "track0_sparse", keepref = T)
    expect_regression(emr_extract("track2_sparse", stime = 10, etime = 1000, keepref = T, filter = "(f1 | i1) & !track5_sparse"), "filter.28")
})

test_that("filter works", {
    i1 <- emr_screen("track1>990", stime = 500, etime = 2000)
    i1 <- data.frame(id = unique(i1$id))
    expect_regression(emr_extract("2 * track2", stime = 10, etime = 1000, keepref = T, filter = "i1"), "filter.29")
})

test_that("Invalid format of id-time points", {
    EMR_FILTERS <<- list()
    i1 <- emr_screen("track1>990", stime = 500, etime = 2000)
    i1 <- data.frame(id = unique(i1$id))
    expect_error(emr_filter.create("f1", i1, keepref = T))
})

test_that("filter works", {
    i1 <- emr_screen("track1>990", stime = 500, etime = 2000)
    i1 <- data.frame(id = unique(i1$id))
    expect_regression(emr_extract("2 * track2", stime = 10, etime = 1000, keepref = T, filter = "i1"), "filter.30")
})

test_that("filter works", {
    i1 <- emr_screen("track1>990", stime = 500, etime = 2000)
    i1 <- data.frame(id = unique(i1$id))
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = T, filter = "!i1"), "filter.31")
})

test_that("filter works", {
    i0 <- emr_screen("track0>990", stime = 500, etime = 2000)
    i0 <- data.frame(id = unique(i0$id))
    i1 <- emr_screen("track1>990", stime = 500, etime = 2000)
    i1 <- data.frame(id = unique(i1$id))
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = T, filter = "i0 & i1"), "filter.32")
})

test_that("filter works", {
    i0 <- emr_screen("track0>990", stime = 500, etime = 2000)
    i0 <- data.frame(id = unique(i0$id))
    i1 <- emr_screen("track1>990", stime = 500, etime = 2000)
    i1 <- data.frame(id = unique(i1$id))
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = T, filter = "i0 | i1"), "filter.33")
})

test_that("filter works", {
    i0 <- emr_screen("track0>990", stime = 500, etime = 2000)
    i0 <- data.frame(id = unique(i0$id))
    i1 <- emr_screen("track1>990", stime = 500, etime = 2000)
    i1 <- data.frame(id = unique(i1$id))
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = F, filter = "(i0 | i1) & !track5"), "filter.34")
})

test_that("filter works", {
    EMR_FILTERS <<- list()
    i0 <- emr_extract("track0", keepref = T)
    i1 <- emr_screen("track1>990", stime = 500, etime = 2000)
    i1 <- data.frame(id = unique(i1$id))
    emr_filter.create("f1", i0, keepref = T)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = T, filter = "(f1 | i1) & !track5"), "filter.35")
})

test_that("start time cannot exceed end time", {
    i0 <- emr_screen("track0>700", keepref = F)
    i1 <- emr_screen("track0<300", keepref = F)
    num.rows <- min(nrow(i0), nrow(i1))
    t <- data.frame(stime = i0$time[1:num.rows], etime = i1$time[1:num.rows])
    expect_error(emr_extract("track2", stime = 10, etime = 20, keepref = T, filter = "t"))
})

test_that("filter works", {
    i0 <- emr_screen("track5>990", keepref = F)
    i1 <- emr_screen("track5<10", keepref = F)
    num.rows <- min(nrow(i0), nrow(i1))
    t <- data.frame(stime = pmin(i0$time[1:num.rows], i1$time[1:num.rows]), etime = pmax(i0$time[1:num.rows], i1$time[1:num.rows]))
    expect_regression(emr_extract("track2", stime = 10, etime = 2000, keepref = T, filter = "t"), "filter.36")
})

test_that("Invalid format of id-time points", {
    EMR_FILTERS <<- list()
    i0 <- emr_screen("track5>990", keepref = F)
    i1 <- emr_screen("track5<10", keepref = F)
    num.rows <- min(nrow(i0), nrow(i1))
    t <- data.frame(stime = pmin(i0$time[1:num.rows], i1$time[1:num.rows]), etime = pmax(i0$time[1:num.rows], i1$time[1:num.rows]))
    expect_error(emr_filter.create("f1", t, keepref = F, time.shift = c(1000, 3000)))
})

test_that("filter works", {
    expect_regression(emr_extract("track1", iterator = 1, keepref = F, stime = 10, etime = 15), "filter.37")
})

test_that("beat iterator warning", {
    expect_warning(emr_extract("track1", iterator = 1, keepref = T, stime = 10, etime = 15))
    expect_warning(expect_regression(emr_extract("track1", iterator = 1, keepref = T, stime = 10, etime = 15), "filter.38"))
})

test_that("Cannot use an implicit time scope with Beat Iteator: please specify 'stime' and 'etime'", {
    EMR_FILTERS <<- list()
    emr_filter.create("f1", "track0", keepref = F, time.shift = c(10, 50))
    emr_filter.create("f2", "track3", keepref = T)
    expect_error(emr_extract("track1", iterator = 1, keepref = T, filter = "f1 & f2"))
})

test_that("filter works", {
    EMR_FILTERS <<- list()
    emr_filter.create("f1", "track0", keepref = F, time.shift = c(10, 50))
    emr_filter.create("f2", "track3", keepref = T)
    expect_regression(emr_extract("track1", iterator = 1, stime = 20, etime = 5000, keepref = T, filter = "f1 & f2"), "filter.39")
})

test_that("filter works", {
    EMR_FILTERS <<- list()
    emr_filter.create("f1", "track0", keepref = F, time.shift = c(10, 50))
    emr_filter.create("f2", "track3", keepref = F)
    expect_regression(emr_extract("track1", iterator = 1, stime = 20, etime = 5000, keepref = T, filter = "f1 & f2"), "filter.40")
})

test_that("filter works", {
    expect_regression(emr_extract("track1", iterator = 2, keepref = F, stime = 10, etime = 15), "filter.41")
})

test_that("filter works", {
    expect_warning(expect_regression(emr_extract("track1", iterator = 2, keepref = T, stime = 10, etime = 15), "filter.42"))
})

test_that("filter works", {
    EMR_FILTERS <<- list()
    emr_filter.create("f1", "track0", keepref = F, time.shift = c(10, 50))
    emr_filter.create("f2", "track3", keepref = T)
    expect_regression(emr_extract("track1", iterator = 2, stime = 20, etime = 5000, keepref = T, filter = "f1 & f2"), "filter.43")
})

test_that("filter works", {
    EMR_FILTERS <<- list()
    emr_filter.create("f1", "track0", keepref = F, time.shift = c(10, 50))
    emr_filter.create("f2", "track3", keepref = F)
    expect_regression(emr_extract("track1", iterator = 2, stime = 20, etime = 5000, keepref = T, filter = "f1 & f2"), "filter.44")
})

test_that("filter works", {
    set.seed(0)
    expect_regression(emr_extract("track1", iterator = list(2, data.frame(id = sample(1:1000, 500), time = sample(1:100, 500, replace = T))), stime = 10, etime = 15), "filter.45")
})

test_that("filter works", {
    EMR_FILTERS <<- list()
    emr_track.rm("test_track1", T)
    emr_filter.create("f1", "track6", expiration = 100000)
    emr_track.create("test_track1", "user", T, "track6", keepref = T, filter = "f1")
    r <- emr_extract("track1", iterator = list(7, "test_track1"), stime = 40, etime = 190)
    emr_track.rm("test_track1", T)
    expect_regression(r, "filter.46")
})

test_that("emr_filter.attr.src works", {
    EMR_FILTERS <<- list()
    emr_filter.create("f1", "track1", time.shift = c(-10, 20))
    emr_filter.attr.src("f1", "track2")
    expect_equal(emr_filter.attr.src("f1"), "track2")
})

test_that("emr_filter.attr.src fails when track doesn't exist", {
    EMR_FILTERS <<- list()
    emr_filter.create("f1", "track1", time.shift = c(-10, 20))
    expect_error(emr_filter.attr.src("f1", "track10"))
    expect_equal(emr_filter.attr.src("f1"), "track1")
})

test_that("emr_filter.attr.src works wotj data frame", {
    EMR_FILTERS <<- list()
    emr_filter.create("f1", "track1", time.shift = c(-10, 20))
    emr_filter.attr.src("f1", data.frame(id = c(1, 3), time = c(10, 30)))
    expect_equal(
        emr_filter.attr.src("f1"),
        structure(list(id = c(1, 3), time = c(10, 30)), class = "data.frame", row.names = c(NA, -2L))
    )
})

test_that("emr_filter.attr.src fails with invalit dataframe", {
    EMR_FILTERS <<- list()
    emr_filter.create("f1", "track1", time.shift = c(-10, 20))
    expect_error(emr_filter.attr.src("f1", data.frame(bla = 2)))
    expect_equal(emr_filter.attr.src("f1"), "track1")
})

test_that("emr_filter.attr.keepref works", {
    EMR_FILTERS <<- list()
    emr_filter.create("f1", "track1", time.shift = c(-10, 20))
    emr_filter.attr.keepref("f1", T)
    expect_true(emr_filter.attr.keepref("f1"))
})

test_that("emr_filter.attr.time.shift works", {
    EMR_FILTERS <<- list()
    emr_filter.create("f1", "track1", time.shift = c(-10, 20))
    emr_filter.attr.time.shift("f1", c(-17, 30))
    expect_equal(emr_filter.attr.time.shift("f1"), c(-17, 30))
})

test_that("emr_filter.attr.val works", {
    EMR_FILTERS <<- list()
    emr_filter.create("f1", "track1", time.shift = c(-10, 20))
    emr_filter.attr.val("f1", 500)
    expect_equal(emr_filter.attr.val("f1"), 500)
})

test_that("emr_filter.attr.expiration works", {
    EMR_FILTERS <<- list()
    emr_filter.create("f1", "track1", time.shift = c(-10, 20))
    emr_filter.attr.expiration("f1", 300)
    expect_equal(emr_filter.attr.expiration("f1"), 300)
})

test_that("emr_filter.exists works", {
    EMR_FILTERS <<- list()
    emr_filter.create("f1", "track2", keepref = F, time.shift = c(-10, 20))
    emr_filter.create("f2", "track2", keepref = F, time.shift = c(-10, 30))
    expect_true(emr_filter.exists("f1"))
})

test_that("emr_filter.exists works", {
    EMR_FILTERS <<- list()
    emr_filter.create("f1", "track2", keepref = F, time.shift = c(-10, 20))
    emr_filter.create("f2", "track2", keepref = F, time.shift = c(-10, 30))
    expect_false(emr_filter.exists("sdaf"))
})

test_that("emr_filter.info works", {
    EMR_FILTERS <<- list()
    emr_filter.create("f1", "track2", keepref = F, time.shift = c(-10, 20))
    expect_equal(
        emr_filter.info("f1"),
        list(
            src = "track2", time_shift = c(-10, 20), keepref = FALSE,
            val = NULL, expiration = NULL
        )
    )
})

test_that("emr_filter.ls works", {
    EMR_FILTERS <<- list()
    emr_filter.create("f1", "track2", keepref = F, time.shift = c(-10, 20))
    emr_filter.create("f2", "track2", keepref = F, time.shift = c(-10, 30))
    expect_equal(emr_filter.ls(), c("f1", "f2"))
})

test_that("emr_filter.ls works", {
    EMR_FILTERS <<- list()
    expect_error(emr_filter.create("f1", "track10", keepref = F, time.shift = c(-10, 20)))
    emr_filter.create("f2", "track1", keepref = F, time.shift = c(-10, 30))
    expect_equal(emr_filter.ls(), "f2")
})

test_that("emr_filter.ls works", {
    EMR_FILTERS <<- list()
    emr_filter.create("f1", "track2", keepref = F, time.shift = c(-10, 20))
    emr_filter.create("f2", "track2", keepref = F, time.shift = c(-10, 30))
    emr_filter.rm("f1")
    expect_equal(emr_filter.ls(), "f2")
})
