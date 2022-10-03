load_test_db()

test_that("filter works", {
    emr_filter.clear()
    fname <- emr_filter.create("f1", "track1", keepref = TRUE)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = TRUE, filter = "f1"), "filter.1")
    expect_equal(fname, "f1")
})

test_that("time.shift is not allowed when keepref is TRUE", {
    expect_error(emr_filter.create("f1", "track1", keepref = TRUE, time.shift = 10))
})

test_that("'val' parameter of length greater than 1 can be used only with categorical tracks", {
    expect_error(emr_filter.create("f1", "track1", val = c(3, 5)))
})

test_that("filter works with time shift", {
    emr_filter.clear()
    emr_filter.create("f1", "track8", val = c(3, 5), time.shift = c(-10, 10))
    expect_regression(emr_extract("track7", keepref = TRUE, filter = "f1"), "filter.2")
})

test_that("'expiration' must be a positive integer", {
    expect_error(emr_filter.create("f1", "track8", expiration = -10))
})

test_that("'expiration' cannot be used when keepref is 'TRUE'", {
    expect_error(emr_filter.create("f1", "track8", keepref = TRUE, expiration = 100))
})

test_that("filter works with keepref", {
    emr_filter.clear()
    emr_filter.create("f1", "track8", expiration = 100)
    expect_regression(emr_extract("track7", keepref = TRUE, filter = "f1"), "filter.3")
})

test_that("filter works with keepref and val", {
    emr_filter.clear()
    emr_filter.create("f1", "track8", expiration = 100, val = c(3, 5))
    expect_regression(emr_extract("track7", keepref = TRUE, filter = "f1"), "filter.4")
})

test_that("filter works with keepref and time shift", {
    emr_filter.clear()
    emr_filter.create("f1", "track1", keepref = FALSE, time.shift = 10)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = TRUE, filter = "f1"), "filter.5")
})

test_that("filter works with keepref and time shift and stime etime #1", {
    emr_filter.clear()
    emr_filter.create("f1", "track1", keepref = FALSE, time.shift = c(10, 20))
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = TRUE, filter = "f1"), "filter.6")
})

test_that("use a track as filter", {
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = TRUE, filter = "!track1"), "filter.7")
})

test_that("use multiple tracks as filter", {
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = TRUE, filter = "!track0 | !track1"), "filter.8")
})

test_that("filter works with keepref and time shift and stime etime #1", {
    emr_filter.clear()
    emr_filter.create("f1", "track1", keepref = TRUE)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = TRUE, filter = "!f1"), "filter.9")
})

test_that("use multiple tracks as filter", {
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = TRUE, filter = "track0 & track1"), "filter.10")
})

test_that("filter works", {
    emr_filter.clear()
    emr_filter.create("f1", "track0", keepref = TRUE)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = TRUE, filter = "f1 & track1"), "filter.11")
})

test_that("filter works", {
    emr_filter.clear()
    emr_filter.create("f1", "track0", keepref = TRUE)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = FALSE, filter = "(f1 | track1) & !track5"), "filter.12")
})

test_that("filter works", {
    emr_filter.clear()
    emr_filter.create("f1", "track0", keepref = TRUE)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = TRUE, filter = "(f1 | track1) & !track5"), "filter.13")
})

test_that("filter works", {
    emr_filter.clear()
    emr_filter.create("f1", "track0_sparse", keepref = TRUE)
    expect_regression(emr_extract("track2_sparse", stime = 10, etime = 1000, keepref = TRUE, filter = "(f1 | track1) & !track5_sparse"), "filter.14")
})

test_that("Id-time list contains two or more points that differ only by reference", {
    i1 <- emr_extract("track1", keepref = TRUE)
    expect_error(emr_extract("2 * track2", stime = 10, etime = 1000, keepref = TRUE, filter = "i1"))
})

test_that("filter works", {
    i1 <- emr_extract("track1", keepref = FALSE)
    expect_regression(emr_extract("2 * track2", stime = 10, etime = 1000, keepref = TRUE, filter = "i1"), "filter.15")
})

test_that("filter works", {
    i1 <- emr_extract("track1_sparse", keepref = FALSE)
    expect_regression(emr_extract("track2_sparse", stime = 10, etime = 1000, keepref = TRUE, filter = "i1"), "filter.16")
})

test_that("filter works", {
    emr_filter.clear()
    i1 <- emr_extract("track1", keepref = TRUE)
    emr_filter.create("f1", i1, keepref = TRUE)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = TRUE, filter = "f1"), "filter.17")
})

test_that("filter works", {
    emr_filter.clear()
    i1 <- emr_extract("track1", keepref = FALSE)
    emr_filter.create("f1", i1, keepref = TRUE)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = TRUE, filter = "f1"), "filter.18")
})

test_that("'time.shift' is not allowed when keepref is 'TRUE'", {
    emr_filter.clear()
    i1 <- emr_extract("track1", keepref = TRUE)
    expect_error(emr_filter.create("f1", i1, keepref = TRUE, time.shift = 10))
    expect_error(emr_extract("track2", stime = 10, etime = 1000, keepref = TRUE, filter = "f1"))
})

test_that("filter works", {
    emr_filter.clear()
    i1 <- emr_extract("track1", keepref = FALSE)
    emr_filter.create("f1", i1, keepref = FALSE, time.shift = 10)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = TRUE, filter = "f1"), "filter.19")
})

test_that("filter works", {
    emr_filter.clear()
    i1 <- emr_extract("track1", keepref = FALSE)
    emr_filter.create("f1", i1, keepref = FALSE, time.shift = c(10, 20))
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = TRUE, filter = "f1"), "filter.20")
})

test_that("filter works", {
    i1 <- emr_extract("track1", keepref = FALSE)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = TRUE, filter = "!i1"), "filter.21")
})

test_that("filter works", {
    emr_filter.clear()
    i1 <- emr_extract("track1", keepref = TRUE)
    emr_filter.create("f1", i1, keepref = TRUE)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = TRUE, filter = "!f1"), "filter.22")
})

test_that("filter works", {
    i0 <- emr_extract("track0", keepref = FALSE)
    i1 <- emr_extract("track1", keepref = FALSE)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = TRUE, filter = "i0 & i1"), "filter.23")
})

test_that("filter works", {
    emr_filter.clear()
    i0 <- emr_extract("track0", keepref = TRUE)
    i1 <- emr_extract("track1", keepref = FALSE)
    emr_filter.create("f1", i0, keepref = TRUE)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = TRUE, filter = "f1 & i1"), "filter.24")
})

test_that("filter works", {
    emr_filter.clear()
    i0 <- emr_extract("track0", keepref = TRUE)
    i1 <- emr_extract("track1", keepref = FALSE)
    emr_filter.create("f1", i0, keepref = TRUE)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = TRUE, filter = "f1 | i1"), "filter.25")
})

test_that("filter works", {
    emr_filter.clear()
    i0 <- emr_extract("track0", keepref = TRUE)
    i1 <- emr_extract("track1", keepref = FALSE)
    emr_filter.create("f1", i0, keepref = TRUE)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = FALSE, filter = "(f1 | i1) & !track5"), "filter.26")
})

test_that("filter works", {
    emr_filter.clear()
    i0 <- emr_extract("track0", keepref = TRUE)
    i1 <- emr_extract("track1", keepref = FALSE)
    emr_filter.create("f1", i0, keepref = TRUE)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = TRUE, filter = "(f1 | i1) & !track5"), "filter.27")
})

test_that("filter works", {
    emr_filter.clear()
    i1 <- emr_extract("track1", keepref = FALSE)
    emr_filter.create("f1", "track0_sparse", keepref = TRUE)
    expect_regression(emr_extract("track2_sparse", stime = 10, etime = 1000, keepref = TRUE, filter = "(f1 | i1) & !track5_sparse"), "filter.28")
})

test_that("filter works", {
    i1 <- emr_screen("track1>990", stime = 500, etime = 2000)
    i1 <- data.frame(id = unique(i1$id))
    expect_regression(emr_extract("2 * track2", stime = 10, etime = 1000, keepref = TRUE, filter = "i1"), "filter.29")
})

test_that("Invalid format of id-time points", {
    emr_filter.clear()
    i1 <- emr_screen("track1>990", stime = 500, etime = 2000)
    i1 <- data.frame(id = unique(i1$id))
    expect_error(emr_filter.create("f1", i1, keepref = TRUE))
})

test_that("filter works", {
    i1 <- emr_screen("track1>990", stime = 500, etime = 2000)
    i1 <- data.frame(id = unique(i1$id))
    expect_regression(emr_extract("2 * track2", stime = 10, etime = 1000, keepref = TRUE, filter = "i1"), "filter.30")
})

test_that("filter works", {
    i1 <- emr_screen("track1>990", stime = 500, etime = 2000)
    i1 <- data.frame(id = unique(i1$id))
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = TRUE, filter = "!i1"), "filter.31")
})

test_that("filter works", {
    i0 <- emr_screen("track0>990", stime = 500, etime = 2000)
    i0 <- data.frame(id = unique(i0$id))
    i1 <- emr_screen("track1>990", stime = 500, etime = 2000)
    i1 <- data.frame(id = unique(i1$id))
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = TRUE, filter = "i0 & i1"), "filter.32")
})

test_that("filter works", {
    i0 <- emr_screen("track0>990", stime = 500, etime = 2000)
    i0 <- data.frame(id = unique(i0$id))
    i1 <- emr_screen("track1>990", stime = 500, etime = 2000)
    i1 <- data.frame(id = unique(i1$id))
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = TRUE, filter = "i0 | i1"), "filter.33")
})

test_that("filter works", {
    i0 <- emr_screen("track0>990", stime = 500, etime = 2000)
    i0 <- data.frame(id = unique(i0$id))
    i1 <- emr_screen("track1>990", stime = 500, etime = 2000)
    i1 <- data.frame(id = unique(i1$id))
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = FALSE, filter = "(i0 | i1) & !track5"), "filter.34")
})

test_that("filter works", {
    emr_filter.clear()
    i0 <- emr_extract("track0", keepref = TRUE)
    i1 <- emr_screen("track1>990", stime = 500, etime = 2000)
    i1 <- data.frame(id = unique(i1$id))
    emr_filter.create("f1", i0, keepref = TRUE)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = TRUE, filter = "(f1 | i1) & !track5"), "filter.35")
})

test_that("start time cannot exceed end time", {
    i0 <- emr_screen("track0>700", keepref = FALSE)
    i1 <- emr_screen("track0<300", keepref = FALSE)
    num.rows <- min(nrow(i0), nrow(i1))
    t <- data.frame(stime = i0$time[1:num.rows], etime = i1$time[1:num.rows])
    expect_error(emr_extract("track2", stime = 10, etime = 20, keepref = TRUE, filter = "t"))
})

test_that("filter works", {
    i0 <- emr_screen("track5>990", keepref = FALSE)
    i1 <- emr_screen("track5<10", keepref = FALSE)
    num.rows <- min(nrow(i0), nrow(i1))
    t <- data.frame(stime = pmin(i0$time[1:num.rows], i1$time[1:num.rows]), etime = pmax(i0$time[1:num.rows], i1$time[1:num.rows]))
    expect_regression(emr_extract("track2", stime = 10, etime = 2000, keepref = TRUE, filter = "t"), "filter.36")
})

test_that("Invalid format of id-time points", {
    emr_filter.clear()
    i0 <- emr_screen("track5>990", keepref = FALSE)
    i1 <- emr_screen("track5<10", keepref = FALSE)
    num.rows <- min(nrow(i0), nrow(i1))
    t <- data.frame(stime = pmin(i0$time[1:num.rows], i1$time[1:num.rows]), etime = pmax(i0$time[1:num.rows], i1$time[1:num.rows]))
    expect_error(emr_filter.create("f1", t, keepref = FALSE, time.shift = c(1000, 3000)))
})

test_that("filter works", {
    expect_regression(emr_extract("track1", iterator = 1, keepref = FALSE, stime = 10, etime = 15), "filter.37")
})

test_that("beat iterator warning", {
    expect_warning(emr_extract("track1", iterator = 1, keepref = TRUE, stime = 10, etime = 15))
    expect_warning(expect_regression(emr_extract("track1", iterator = 1, keepref = TRUE, stime = 10, etime = 15), "filter.38"))
})

test_that("Cannot use an implicit time scope with Beat Iterator: please specify 'stime' and 'etime'", {
    emr_filter.clear()
    emr_filter.create("f1", "track0", keepref = FALSE, time.shift = c(10, 50))
    emr_filter.create("f2", "track3", keepref = TRUE)
    expect_error(emr_extract("track1", iterator = 1, keepref = TRUE, filter = "f1 & f2"))
})

test_that("filter works", {
    emr_filter.clear()
    emr_filter.create("f1", "track0", keepref = FALSE, time.shift = c(10, 50))
    emr_filter.create("f2", "track3", keepref = TRUE)
    expect_regression(emr_extract("track1", iterator = 1, stime = 20, etime = 5000, keepref = TRUE, filter = "f1 & f2"), "filter.39")
})

test_that("filter works", {
    emr_filter.clear()
    emr_filter.create("f1", "track0", keepref = FALSE, time.shift = c(10, 50))
    emr_filter.create("f2", "track3", keepref = FALSE)
    expect_regression(emr_extract("track1", iterator = 1, stime = 20, etime = 5000, keepref = TRUE, filter = "f1 & f2"), "filter.40")
})

test_that("filter works", {
    expect_regression(emr_extract("track1", iterator = 2, keepref = FALSE, stime = 10, etime = 15), "filter.41")
})

test_that("filter works", {
    expect_warning(expect_regression(emr_extract("track1", iterator = 2, keepref = TRUE, stime = 10, etime = 15), "filter.42"))
})

test_that("filter works", {
    emr_filter.clear()
    emr_filter.create("f1", "track0", keepref = FALSE, time.shift = c(10, 50))
    emr_filter.create("f2", "track3", keepref = TRUE)
    expect_regression(emr_extract("track1", iterator = 2, stime = 20, etime = 5000, keepref = TRUE, filter = "f1 & f2"), "filter.43")
})

test_that("filter works", {
    emr_filter.clear()
    emr_filter.create("f1", "track0", keepref = FALSE, time.shift = c(10, 50))
    emr_filter.create("f2", "track3", keepref = FALSE)
    expect_regression(emr_extract("track1", iterator = 2, stime = 20, etime = 5000, keepref = TRUE, filter = "f1 & f2"), "filter.44")
})

test_that("filter works", {
    set.seed(0)
    expect_regression(emr_extract("track1", iterator = list(2, data.frame(id = sample(1:1000, 500), time = sample(1:100, 500, replace = TRUE))), stime = 10, etime = 15), "filter.45")
})

test_that("filter works", {
    emr_filter.clear()
    emr_track.rm("test_track1", TRUE)
    emr_filter.create("f1", "track6", expiration = 100000)
    emr_track.create("test_track1", "user", TRUE, "track6", keepref = TRUE, filter = "f1")
    r <- emr_extract("track1", iterator = list(7, "test_track1"), stime = 40, etime = 190)
    emr_track.rm("test_track1", TRUE)
    expect_regression(r, "filter.46")
})

test_that("emr_filter.attr.src works", {
    emr_filter.clear()
    emr_filter.create("f1", "track1", time.shift = c(-10, 20))
    emr_filter.attr.src("f1", "track2")
    expect_equal(emr_filter.attr.src("f1"), "track2")
})

test_that("emr_filter.attr.src fails when track doesn't exist", {
    emr_filter.clear()
    emr_filter.create("f1", "track1", time.shift = c(-10, 20))
    expect_error(emr_filter.attr.src("f1", "track10"))
    expect_equal(emr_filter.attr.src("f1"), "track1")
})

test_that("emr_filter.attr.src works wotj data frame", {
    emr_filter.clear()
    emr_filter.create("f1", "track1", time.shift = c(-10, 20))
    emr_filter.attr.src("f1", data.frame(id = c(1, 3), time = c(10, 30)))
    expect_equal(
        emr_filter.attr.src("f1"),
        structure(list(id = c(1, 3), time = c(10, 30)), class = "data.frame", row.names = c(NA, -2L))
    )
})

test_that("emr_filter.attr.src fails with invalit dataframe", {
    emr_filter.clear()
    emr_filter.create("f1", "track1", time.shift = c(-10, 20))
    expect_error(emr_filter.attr.src("f1", data.frame(bla = 2)))
    expect_equal(emr_filter.attr.src("f1"), "track1")
})

test_that("emr_filter.attr.keepref works", {
    emr_filter.clear()
    emr_filter.create("f1", "track1", time.shift = c(-10, 20))
    emr_filter.attr.keepref("f1", TRUE)
    expect_true(emr_filter.attr.keepref("f1"))
})

test_that("emr_filter.attr.time.shift works", {
    emr_filter.clear()
    emr_filter.create("f1", "track1", time.shift = c(-10, 20))
    emr_filter.attr.time.shift("f1", c(-17, 30))
    expect_equal(emr_filter.attr.time.shift("f1"), c(-17, 30))
})

test_that("emr_filter.attr.val works", {
    emr_filter.clear()
    emr_filter.create("f1", "track1", time.shift = c(-10, 20))
    emr_filter.attr.val("f1", 500)
    expect_equal(emr_filter.attr.val("f1"), 500)
})

test_that("emr_filter.attr.expiration works", {
    emr_filter.clear()
    emr_filter.create("f1", "track1", time.shift = c(-10, 20))
    emr_filter.attr.expiration("f1", 300)
    expect_equal(emr_filter.attr.expiration("f1"), 300)
})

test_that("emr_filter.exists works", {
    emr_filter.clear()
    emr_filter.create("f1", "track2", keepref = FALSE, time.shift = c(-10, 20))
    emr_filter.create("f2", "track2", keepref = FALSE, time.shift = c(-10, 30))
    expect_true(emr_filter.exists("f1"))
})

test_that("emr_filter.exists works", {
    emr_filter.clear()
    emr_filter.create("f1", "track2", keepref = FALSE, time.shift = c(-10, 20))
    emr_filter.create("f2", "track2", keepref = FALSE, time.shift = c(-10, 30))
    expect_false(emr_filter.exists("sdaf"))
})

test_that("emr_filter.info works", {
    emr_filter.clear()
    emr_filter.create("f1", "track2", keepref = FALSE, time.shift = c(-10, 20))
    expect_equal(
        emr_filter.info("f1"),
        list(
            src = "track2", time_shift = c(-10, 20), keepref = FALSE,
            val = NULL, expiration = NULL, operator = "=", use_values = FALSE
        )
    )
})

test_that("emr_filter.create works with -1 as val (kr issues)", {
    emr_filter.clear()
    df <- data.frame(id = 1, time = c(1, 2, 2), value = c(-1, 4, 3), ref = c(0, 0, 1))
    emr_track.import("minusone", space = .naryn$EMR_UROOT, categorical = TRUE, src = df)
    withr::defer(emr_track.rm("minusone", force = TRUE))
    emr_filter.create("f", "minusone", val = -1)
    t <- emr_extract("minusone", filter = "f")
    expect_equal(data.frame(id = 1, time = 1, ref = -1, minusone = -1), t)
})

test_that("emr_filter.ls works", {
    emr_filter.clear()
    emr_filter.create("f1", "track2", keepref = FALSE, time.shift = c(-10, 20))
    emr_filter.create("f2", "track2", keepref = FALSE, time.shift = c(-10, 30))
    expect_equal(emr_filter.ls(), c("f1", "f2"))
})

test_that("emr_filter.ls works", {
    emr_filter.clear()
    expect_error(emr_filter.create("f1", "track10", keepref = FALSE, time.shift = c(-10, 20)))
    emr_filter.create("f2", "track1", keepref = FALSE, time.shift = c(-10, 30))
    expect_equal(emr_filter.ls(), "f2")
})

test_that("emr_filter.ls works", {
    emr_filter.clear()
    emr_filter.create("f1", "track2", keepref = FALSE, time.shift = c(-10, 20))
    emr_filter.create("f2", "track2", keepref = FALSE, time.shift = c(-10, 30))
    emr_filter.rm("f1")
    expect_equal(emr_filter.ls(), "f2")
})


test_that("emr_filter.clear works", {
    emr_filter.clear()
    expect_equal(emr_filter.ls(), character(0))
    emr_filter.create("f1", "track2", keepref = FALSE, time.shift = c(-10, 20))
    emr_filter.create("f2", "track2", keepref = FALSE, time.shift = c(-10, 30))
    emr_filter.clear()
    expect_equal(emr_filter.ls(), character(0))
})

test_that("cannot create filters with invalid names", {
    expect_error(emr_filter.create("..emr_savta", "track0"))
    expect_error(emr_filter.create(".. savta", "track0"))
    expect_error(emr_filter.create("savta?", "track0"))
    expect_error(emr_filter.create("saba!", "track0"))
    expect_error(emr_filter.create("bamba+bissli", "track0"))
})

test_that("emr_filter.name works", {
    expect_equal(
        emr_filter.name("track10", keepref = FALSE, time.shift = c(-10, 20)),
        "f_track10.krF.ts_minus10_20"
    )

    expect_equal(
        emr_filter.name("track10", keepref = TRUE, time.shift = c(-10, 30)),
        "f_track10.krT.ts_minus10_30"
    )

    expect_equal(
        emr_filter.name("track10", keepref = TRUE, time.shift = c(-10, 30), expiration = 100),
        "f_track10.krT.ts_minus10_30.exp_100"
    )

    expect_equal(
        emr_filter.name("track10", keepref = FALSE, time.shift = c(-10, 30), val = c(15, 16, 17), expiration = 100),
        "f_track10.krF.vals_15_16_17.ts_minus10_30.exp_100"
    )

    expect_equal(
        emr_filter.name("track10"),
        "f_track10.krF"
    )

    expect_equal(
        emr_filter.name("track10", time.shift = c(-10, 30)),
        "f_track10.krF.ts_minus10_30"
    )

    expect_equal(
        emr_filter.name("track10", expiration = 100),
        "f_track10.krF.exp_100"
    )

    expect_equal(
        emr_filter.name("track10", val = 15:17),
        "f_track10.krF.vals_15_16_17"
    )

    expect_equal(
        emr_filter.name("track1", keepref = FALSE, time.shift = 10),
        "f_track1.krF.ts_10"
    )

    expect_equal(
        emr_filter.name("track.kr.krT.krF.kr", keepref = FALSE, time.shift = 10),
        "f_track.kr.krT.krF.kr.krF.ts_10"
    )

    expect_equal(
        emr_filter.name("track0", keepref = FALSE, time.shift = 10, val = 4, operator = "<="),
        "f_track0.krF.vals_4.ts_10.op_lte"
    )

    expect_equal(
        emr_filter.name("track0", val = 4, operator = ">"),
        "f_track0.krF.vals_4.op_gt"
    )

    expect_error(emr_filter.name("track10", time.shift = 1:10))
})

test_that("filter works with automatic naming", {
    emr_filter.clear()
    fname <- emr_filter.create(NULL, "track1", keepref = TRUE)
    expect_equal(fname, "f_track1.krT")
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = TRUE, filter = fname), "filter.1")

    fname <- emr_filter.create(NULL, "track8", val = c(3, 5), time.shift = c(-10, 10))
    expect_equal(fname, "f_track8.krF.vals_3_5.ts_minus10_10")
    expect_regression(emr_extract("track7", keepref = TRUE, filter = fname), "filter.2")
})

test_that("emr_filter.create_from_name works", {
    emr_filter.clear()
    fname <- emr_filter.name("track8", val = c(3, 5), time.shift = c(-10, 10))
    fname2 <- emr_filter.create_from_name(fname)
    expect_equal(fname, fname2)
    expect_true(fname %in% emr_filter.ls())
    expect_regression(emr_extract("track7", keepref = TRUE, filter = fname), "filter.2")

    fname <- emr_filter.name("track1", keepref = TRUE)
    emr_filter.create_from_name(fname)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = TRUE, filter = fname), "filter.1")

    fname <- emr_filter.name("track8", expiration = 100)
    emr_filter.create_from_name(fname)
    expect_regression(emr_extract("track7", keepref = TRUE, filter = fname), "filter.3")

    fname <- emr_filter.name("track8", expiration = 100, val = c(3, 5))
    emr_filter.create_from_name(fname)
    expect_regression(emr_extract("track7", keepref = TRUE, filter = fname), "filter.4")

    fname <- emr_filter.create_from_name(emr_filter.name("track1", keepref = FALSE, time.shift = 10))
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = TRUE, filter = fname), "filter.5")

    fname <- emr_filter.create_from_name(emr_filter.name("track1", keepref = FALSE, time.shift = c(10, 20)))
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = TRUE, filter = fname), "filter.6")

    fname <- emr_filter.create_from_name(emr_filter.name("track0", keepref = TRUE))
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = TRUE, filter = glue::glue("{fname} & track1")), "filter.11")

    i1 <- emr_extract("track1", keepref = FALSE)
    expect_error(emr_filter.name(i1, keepref = FALSE, time.shift = 10), "Cannot generate automatic filter name when source is a data.frame")

    fname1 <- emr_filter.create_from_name(emr_filter.name("track0", keepref = FALSE, time.shift = c(10, 50)))
    fname2 <- emr_filter.create_from_name(emr_filter.name("track3", keepref = TRUE))
    expect_regression(emr_extract("track1", iterator = 1, stime = 20, etime = 5000, keepref = TRUE, filter = glue::glue("{fname1} & {fname2}")), "filter.39")

    fname1 <- emr_filter.create_from_name(emr_filter.name("track0", keepref = FALSE, time.shift = c(10, 50)))
    fname2 <- emr_filter.create_from_name(emr_filter.name("track3", keepref = FALSE))
    expect_regression(emr_extract("track1", iterator = 1, stime = 20, etime = 5000, keepref = TRUE, filter = glue::glue("{fname1} & {fname2}")), "filter.40")

    emr_filter.create("f1", "track2", keepref = FALSE, time.shift = c(-10, 20))
    fname <- emr_filter.name("track2", keepref = FALSE, time.shift = c(-10, 20))
    emr_filter.create_from_name(fname)
    expect_equal(emr_filter.info("f1"), emr_filter.info(fname))

    fname <- emr_filter.create_from_name(emr_filter.name("track0", keepref = TRUE, val = 414, operator = "<"))
    expect_equal(emr_extract("track0", keepref = TRUE) %>% dplyr::filter(track0 < 414), emr_extract("track0", keepref = TRUE, filter = fname))
})

test_that("emr_filter.create_from_name works when track name has '.'", {
    emr_track.create("track.1", "user", TRUE, "track1", keepref = TRUE)
    withr::defer(emr_track.rm("track.1", TRUE))
    fname <- emr_filter.create_from_name(emr_filter.name("track.1", keepref = TRUE))
    expect_equal(emr_filter.info(fname)$src, "track.1")
})

test_that("emr_filter.create_from_name works when track name has '.kr'", {
    emr_track.create("track.kr", "user", TRUE, "track1", keepref = TRUE)
    withr::defer(emr_track.rm("track.kr", TRUE))
    fname <- emr_filter.create_from_name(emr_filter.name("track.kr", keepref = TRUE))
    expect_equal(emr_filter.info(fname)$src, "track.kr")

    emr_track.create("track.kr.krT.kr_kr.kr", "user", TRUE, "track1", keepref = TRUE)
    withr::defer(emr_track.rm("track.kr.krT.kr_kr.kr", TRUE))
    fname <- emr_filter.create_from_name(emr_filter.name("track.kr.krT.kr_kr.kr", keepref = TRUE))
    expect_equal(emr_filter.info(fname)$src, "track.kr.krT.kr_kr.kr")
})

test_that("emr_filter.create_from_name works when track name has '.krT'", {
    emr_track.create("track.krT", "user", TRUE, "track1", keepref = TRUE)
    withr::defer(emr_track.rm("track.krT", TRUE))
    fname <- emr_filter.create_from_name(emr_filter.name("track.krT", keepref = TRUE))
    expect_equal(emr_filter.info(fname)$src, "track.krT")
})

test_that("emr_filter.create_from_name works when track name starts with 'f_'", {
    emr_track.create("f_track", "user", TRUE, "track1", keepref = TRUE)
    withr::defer(emr_track.rm("f_track", TRUE))
    fname <- emr_filter.create_from_name(emr_filter.name("f_track", keepref = TRUE))
    expect_equal(emr_filter.info(fname)$src, "f_track")
})

test_that("emr_filter.create_from_name works when track name has '_'", {
    emr_track.create("track_1", "user", TRUE, "track1", keepref = TRUE)
    withr::defer(emr_track.rm("track_1", TRUE))
    fname <- emr_filter.create_from_name(emr_filter.name("track_1", keepref = TRUE))
    expect_equal(emr_filter.info(fname)$src, "track_1")
})

test_that("emr_filter.create_from_name works when arguments have a '.'", {
    fname <- emr_filter.create_from_name(emr_filter.name("ph1", val = c(1, 2.5, 3.4)))
    expect_equal(emr_filter.info(fname)$src, "ph1")
    expect_equal(emr_filter.info(fname)$val, c(1, 2.5, 3.4))

    fname <- emr_filter.create_from_name(emr_filter.name("ph1", val = c(1, 2.5, 3.4), time.shift = c(-10, 50)))
    expect_equal(emr_filter.info(fname)$src, "ph1")
    expect_equal(emr_filter.info(fname)$val, c(1, 2.5, 3.4))
    expect_equal(emr_filter.info(fname)$time_shift, c(-10, 50))

    fname <- emr_filter.create_from_name(emr_filter.name("ph1", val = c(1, 2.5, 3.4), time.shift = c(-10, 50), expiration = 100))
    expect_equal(emr_filter.info(fname)$src, "ph1")
    expect_equal(emr_filter.info(fname)$val, c(1, 2.5, 3.4))
    expect_equal(emr_filter.info(fname)$time_shift, c(-10, 50))
    expect_equal(emr_filter.info(fname)$expiration, 100)

    fname <- emr_filter.create_from_name(emr_filter.name("ph1", val = c(1, 2.5, 3.4), time.shift = c(-10.5, 50.4)))
    expect_equal(emr_filter.info(fname)$src, "ph1")
    expect_equal(emr_filter.info(fname)$val, c(1, 2.5, 3.4))
    expect_equal(emr_filter.info(fname)$time_shift, c(-10.5, 50.4))

    fname <- emr_filter.create_from_name(emr_filter.name("ph1", time.shift = c(-10.5, 50.4)))
    expect_equal(emr_filter.info(fname)$src, "ph1")
    expect_null(emr_filter.info(fname)$val)
    expect_equal(emr_filter.info(fname)$time_shift, c(-10.5, 50.4))

    fname <- emr_filter.create_from_name(emr_filter.name("ph1", val = c(1, 2.5, 3.4), time.shift = c(-10.5, 50.4), expiration = 100))
    expect_equal(emr_filter.info(fname)$src, "ph1")
    expect_equal(emr_filter.info(fname)$val, c(1, 2.5, 3.4))
    expect_equal(emr_filter.info(fname)$time_shift, c(-10.5, 50.4))
    expect_equal(emr_filter.info(fname)$expiration, 100)

    fname <- emr_filter.create_from_name(emr_filter.name("ph1", time.shift = c(-10.5, 50.4), expiration = 100))
    expect_equal(emr_filter.info(fname)$src, "ph1")
    expect_equal(emr_filter.info(fname)$time_shift, c(-10.5, 50.4))
    expect_equal(emr_filter.info(fname)$expiration, 100)
})


test_that("emr_filter.create_from_name works with very large numbers", {
    withr::local_options(list(scipen = 1))
    fname <- emr_filter.name("ph1", keepref = TRUE, val = 1e9)
    expect_equal(fname, "f_ph1.krT.vals_1000000000")
    emr_filter.create_from_name(fname)
    expect_equal(emr_filter.info(fname)$val, 1e9)
})

test_that("emr_filter.create_from_name works when src is a vector", {
    fnames <- emr_filter.name(c("track0", "track1", "track3"), keepref = FALSE, time.shift = c(10, 50))
    expect_equal(fnames, c("f_track0.krF.ts_10_50", "f_track1.krF.ts_10_50", "f_track3.krF.ts_10_50"))
    fnames1 <- emr_filter.create_from_name(fnames)
    expect_equal(fnames, fnames1)
    expect_equal(purrr::map_chr(fnames, ~ emr_filter.info(.x)$src), c("track0", "track1", "track3"))
    purrr::walk(fnames, ~ {
        expect_equal(emr_filter.info(.x)$time_shift, c(10, 50))
        expect_equal(emr_filter.info(.x)$keepref, FALSE)
    })
})

test_that("emr_filter.create works when src is a vector (1)", {
    fnames <- emr_filter.create(filter = NULL, src = c("track0", "track1", "track3"), keepref = FALSE, time.shift = c(10, 50))
    expect_equal(fnames, c("f_track0.krF.ts_10_50", "f_track1.krF.ts_10_50", "f_track3.krF.ts_10_50"))
    expect_equal(purrr::map_chr(fnames, ~ emr_filter.info(.x)$src), c("track0", "track1", "track3"))
    purrr::walk(fnames, ~ {
        expect_equal(emr_filter.info(.x)$time_shift, c(10, 50))
        expect_equal(emr_filter.info(.x)$keepref, FALSE)
    })
})

test_that("emr_filter.create works when src is a vector (2)", {
    fnames <- emr_filter.create(filter = c("f1", "f2", "f3"), src = c("track0", "track1", "track3"), keepref = FALSE, time.shift = c(10, 50))
    expect_equal(fnames, c("f1", "f2", "f3"))
    expect_equal(purrr::map_chr(fnames, ~ emr_filter.info(.x)$src), c("track0", "track1", "track3"))
    purrr::walk(fnames, ~ {
        expect_equal(emr_filter.info(.x)$time_shift, c(10, 50))
        expect_equal(emr_filter.info(.x)$keepref, FALSE)
    })
})

test_that("emr_filter with operator works as expected on numerical tracks and keepref true", {
    emr_filter.clear()
    t1 <- emr_extract("track0", keepref = TRUE)

    emr_filter.create("eq", src = "track0", keepref = TRUE, val = 414, operator = "=")
    emr_filter.create("lte", src = "track0", keepref = TRUE, val = 414, operator = "<=")
    emr_filter.create("lt", src = "track0", keepref = TRUE, val = 414, operator = "<")
    emr_filter.create("gt", src = "track0", keepref = TRUE, val = 414, operator = ">")
    emr_filter.create("gte", src = "track0", keepref = TRUE, val = 414, operator = ">=")

    expect_error(emr_filter.create("gte", src = "track0", keepref = TRUE, val = 414, operator = "~"))
    expect_error(emr_filter.create("gte", src = "track0", keepref = TRUE, operator = ">"))
    expect_error(emr_filter.create("gte", src = "track0", keepref = TRUE, val = c(414, 888), operator = ">="))
    # time.shift is allowed when keepref is false
    expect_error(emr_filter.create("gte", src = "track0", keepref = TRUE, time.shift = c(-5, 5) * 24, val = 414, operator = "<="))

    t2 <- emr_extract("track0", keepref = TRUE, filter = "eq")
    expect_equal(t1 %>% dplyr::filter(track0 == 414), t2)

    t2 <- emr_extract("track0", keepref = TRUE, filter = "lte")
    expect_equal(t1 %>% dplyr::filter(track0 <= 414), t2)

    t2 <- emr_extract("track0", keepref = TRUE, filter = "lt")
    expect_equal(t1 %>% dplyr::filter(track0 < 414), t2)

    t2 <- emr_extract("track0", keepref = TRUE, filter = "gte")
    expect_equal(t1 %>% dplyr::filter(track0 >= 414), t2)

    t2 <- emr_extract("track0", keepref = TRUE, filter = "gt")
    expect_equal(t1 %>% dplyr::filter(track0 > 414), t2)
})

test_that("emr_filter with operator works as expected on numerical tracks and keepref false", {
    emr_filter.clear()

    # This should work the same as the test below - keepref is ignored for filters with operators

    df <- data.frame(id = c(1, 1, 2), time = c(5, 5, 7), ref = c(0, 1, 0), value = c(100, 500, 300))

    emr_track.import("t1", space = "user", categorical = FALSE, src = df)
    withr::defer(emr_track.rm("t1", force = TRUE))

    emr_filter.create("lt.150", src = "t1", val = 150, keepref = FALSE, operator = "<")
    emr_filter.create("gt.150", src = "t1", val = 150, keepref = FALSE, operator = ">")

    lt <- emr_extract("t1", filter = "lt.150")
    gt <- emr_extract("t1", filter = "gt.150")

    expect_equal(data.frame(id = 1, time = 5, ref = -1, t1 = 300), lt)
    expect_equal(data.frame(id = c(1, 2), time = c(5, 7), ref = c(-1, -1), t1 = c(300, 300)), gt)
})

test_that("emr_filter with operator works as expected on numerical tracks and mix of keeprefs", {
    emr_filter.clear()

    # This should work the same as the test above - keepref is ignored for filters with operators

    df <- data.frame(id = c(1, 1, 2), time = c(5, 5, 7), ref = c(0, 1, 0), value = c(100, 500, 300))

    emr_track.import("t1", space = "user", categorical = FALSE, src = df)
    withr::defer(emr_track.rm("t1", force = TRUE))

    emr_filter.create("lt.150", src = "t1", val = 150, keepref = TRUE, operator = "<")
    emr_filter.create("gt.150", src = "t1", val = 150, keepref = TRUE, operator = ">")

    lt <- emr_extract("t1", keepref = FALSE, filter = "lt.150")
    gt <- emr_extract("t1", keepref = FALSE, filter = "gt.150")

    expect_equal(data.frame(id = 1, time = 5, ref = -1, t1 = 300), lt)
    expect_equal(data.frame(id = c(1, 2), time = c(5, 7), ref = c(-1, -1), t1 = c(300, 300)), gt)
})

test_that("emr_filter with operator works as expected on numerical tracks and mix of keeprefs", {
    emr_filter.clear()
    df <- data.frame(id = c(1, 1, 2), time = c(5, 5, 7), ref = c(0, 1, 0), value = c(100, 500, 300))

    emr_track.import("t1", space = "user", categorical = FALSE, src = df)
    withr::defer(emr_track.rm("t1", force = TRUE))

    emr_filter.create("lt.150", src = "t1", val = 150, keepref = FALSE, operator = "<")
    emr_filter.create("gt.150", src = "t1", val = 150, keepref = FALSE, operator = ">")

    lt <- emr_extract("t1", keepref = TRUE, filter = "lt.150")
    gt <- emr_extract("t1", keepref = TRUE, filter = "gt.150")

    # both 100 and 500 are returned when kr is TRUE, since keepref is ignored in the filter
    # and 100 passes the filter for id,time = 1,5 therefore we get also 500 which has same id,time
    expect_equal(data.frame(id = c(1, 1), time = c(5, 5), ref = c(0, 1), t1 = c(100, 500)), lt)

    # we expect to get everything back, same case as before, now 500 passes the filter for if,time = 1,5
    # so we get also the other value of the point (100) which is lower than 150.
    expect_equal(data.frame(id = c(1, 1, 2), time = c(5, 5, 7), ref = c(0, 1, 0), t1 = c(100, 500, 300)), gt)
})

test_that("emr_filter with operator works as expected on categorical tracks", {
    emr_filter.clear()
    df <- data.frame(id = c(1, 1, 2), time = c(5, 5, 7), ref = c(0, 1, 0), value = c(2, 4, 4))

    emr_track.import("t1", space = "user", categorical = TRUE, src = df)
    withr::defer(emr_track.rm("t1", force = TRUE))

    emr_filter.create("lt.150", src = "t1", val = 3, keepref = FALSE, operator = "<")
    emr_filter.create("gt.150", src = "t1", val = 3, keepref = FALSE, operator = ">")

    lt <- emr_extract("t1", keepref = FALSE, filter = "lt.150")
    gt <- emr_extract("t1", keepref = FALSE, filter = "gt.150")

    expect_equal(data.frame(id = c(1), time = c(5), ref = c(-1), t1 = c(-1)), lt)
    expect_equal(data.frame(id = c(1, 2), time = c(5, 7), ref = c(-1, -1), t1 = c(-1, 4)), gt)

    emr_filter.create("lt.150", src = "t1", val = 3, keepref = TRUE, operator = "<")
    emr_filter.create("gt.150", src = "t1", val = 3, keepref = TRUE, operator = ">")

    lt <- emr_extract("t1", keepref = TRUE, filter = "lt.150")
    gt <- emr_extract("t1", keepref = TRUE, filter = "gt.150")

    expect_equal(data.frame(id = c(1), time = c(5), ref = c(0), t1 = c(2)), lt)
    expect_equal(data.frame(id = c(1, 2), time = c(5, 7), ref = c(1, 0), t1 = c(4, 4)), gt)

    emr_filter.create("lt.150", src = "t1", val = 3, keepref = FALSE, operator = "<")
    emr_filter.create("gt.150", src = "t1", val = 3, keepref = FALSE, operator = ">")

    lt <- emr_extract("t1", keepref = TRUE, filter = "lt.150")
    gt <- emr_extract("t1", keepref = TRUE, filter = "gt.150")

    expect_equal(data.frame(id = c(1, 1), time = c(5, 5), ref = c(0, 1), t1 = c(2, 4)), lt)
    expect_equal(data.frame(id = c(1, 1, 2), time = c(5, 5, 7), ref = c(0, 1, 0), t1 = c(2, 4, 4)), gt)

    emr_filter.create("lt.150", src = "t1", val = 3, keepref = TRUE, operator = "<")
    emr_filter.create("gt.150", src = "t1", val = 3, keepref = TRUE, operator = ">")

    lt <- emr_extract("t1", keepref = FALSE, filter = "lt.150")
    gt <- emr_extract("t1", keepref = FALSE, filter = "gt.150")

    expect_equal(data.frame(id = c(1), time = c(5), ref = c(-1), t1 = c(-1)), lt)
    expect_equal(data.frame(id = c(1, 2), time = c(5, 7), ref = c(-1, -1), t1 = c(-1, 4)), gt)
})

test_that("filter on vtrack works with avg", {
    emr_filter.clear()
    emr_vtrack.clear()

    df <- data.frame(id = c(1, 1, 3, 3), time = c(1, 3, 5, 7), value = c(1, 3, 5, 7))

    emr_track.import("t", src = df, categorical = FALSE)
    withr::defer(emr_track.rm("t", force = TRUE))

    emr_vtrack.create("vt", src = "t", func = "avg", time.shift = c(-2, 0))
    withr::defer(emr_vtrack.rm("vt"))

    expect_error(.emr_filter.create("fvt", src = "vt", operator = ">", use_values = TRUE))
    emr_filter.create("fvt", src = "vt", val = 3, operator = ">")

    withr::defer(emr_filter.rm("fvt"))

    t <- emr_extract("vt", iterator = "t", filter = "fvt")

    expect_equal(t, data.frame(id = c(3, 3), time = c(5, 7), ref = c(-1, -1), vt = c(5, 6)))
})

test_that("filter on vtrack works with sum", {
    emr_filter.clear()
    emr_vtrack.clear()

    df <- data.frame(id = c(1, 1, 3, 3), time = c(1, 3, 5, 7), value = c(1, 3, 5, 7))

    emr_track.import("t", src = df, categorical = FALSE)
    withr::defer(emr_track.rm("t", force = TRUE))

    emr_vtrack.create("vt", src = "t", func = "sum", time.shift = c(-2, 0))
    withr::defer(emr_vtrack.rm("vt"))

    emr_filter.create("fvt", src = "vt", val = 3, operator = ">")

    withr::defer(emr_filter.rm("fvt"))

    t <- emr_extract("vt", iterator = "t", filter = "fvt")

    expect_equal(t, data.frame(id = c(1, 3, 3), time = c(3, 5, 7), ref = c(-1, -1, -1), vt = c(4, 5, 12)))
})

test_that("filter on vtrack works with min/max", {
    emr_filter.clear()
    emr_vtrack.clear()

    df <- data.frame(id = c(1, 1, 3, 3), time = c(1, 3, 5, 7), value = c(1, 3, 5, 7))

    emr_track.import("t", src = df, categorical = FALSE)
    withr::defer(emr_track.rm("t", force = TRUE))

    emr_vtrack.create("vt", src = "t", func = "min", time.shift = c(-2, 0))
    withr::defer(emr_vtrack.rm("vt"))

    emr_filter.create("fvt", src = "vt", val = 3, operator = ">")
    withr::defer(emr_filter.rm("fvt"))

    t <- emr_extract("vt", iterator = "t", filter = "fvt")
    expect_equal(t, data.frame(id = c(3, 3), time = c(5, 7), ref = c(-1, -1), vt = c(5, 5)))

    emr_vtrack.create("vt", src = "t", func = "max", time.shift = c(-2, 0))
    withr::defer(emr_vtrack.rm("vt"))

    t <- emr_extract("vt", iterator = "t", filter = "fvt")
    expect_equal(t, data.frame(id = c(3, 3), time = c(5, 7), ref = c(-1, -1), vt = c(5, 7)))
})

test_that("filter on vtrack works with earliest/latest", {
    emr_filter.clear()
    emr_vtrack.clear()

    df <- data.frame(id = c(1, 1, 3, 3), time = c(1, 3, 5, 7), value = c(1, 3, 5, 7))

    emr_track.import("t", src = df, categorical = FALSE)
    withr::defer(emr_track.rm("t", force = TRUE))

    emr_vtrack.create("vt", src = "t", func = "earliest", time.shift = c(-2, 0))
    withr::defer(emr_vtrack.rm("vt"))

    emr_filter.create("fvt", src = "vt", val = 3, operator = ">")

    withr::defer(emr_filter.rm("fvt"))

    t <- emr_extract("vt", iterator = "t", filter = "fvt")
    expect_equal(t, data.frame(id = c(3, 3), time = c(5, 7), ref = c(-1, -1), vt = c(5, 5)))

    emr_vtrack.create("vt", src = "t", func = "latest", time.shift = c(-2, 0))
    withr::defer(emr_vtrack.rm("vt"))

    t <- emr_extract("vt", iterator = "t", filter = "fvt")
    expect_equal(t, data.frame(id = c(3, 3), time = c(5, 7), ref = c(-1, -1), vt = c(5, 7)))
})


test_that("filter on vtrack works with exists", {
    emr_filter.clear()
    emr_vtrack.clear()

    df <- data.frame(id = c(1, 1, 3, 3), time = c(1, 3, 5, 7), value = c(1, 3, 5, 7))

    emr_track.import("t", src = df, categorical = TRUE)
    withr::defer(emr_track.rm("t", force = TRUE))

    emr_vtrack.create("vt", src = "t", params = c(5), func = "exists", time.shift = c(-2, 0))
    withr::defer(emr_vtrack.rm("vt"))

    emr_filter.create("fvt", src = "vt", val = 0.5, operator = ">")
    withr::defer(emr_filter.rm("fvt"))

    t <- emr_extract("vt", iterator = "t", filter = "fvt")

    expect_equal(t, data.frame(id = c(3, 3), time = c(5, 7), ref = c(-1, -1), vt = c(1, 1)))
})

test_that("filter on vtrack works with combination of filters", {
    emr_filter.clear()
    emr_vtrack.clear()

    df <- data.frame(id = c(1, 1, 3, 3), time = c(1, 3, 5, 7), value = c(1, 3, 5, 7))

    emr_track.import("t", src = df, categorical = FALSE)
    withr::defer(emr_track.rm("t", force = TRUE))

    emr_vtrack.create("vt", src = "t", func = "min", time.shift = c(-2, 0))
    withr::defer(emr_vtrack.rm("vt"))

    emr_filter.create("fvt", src = "vt", val = 3, operator = ">")
    withr::defer(emr_filter.rm("fvt"))

    emr_filter.create("fnothing", src = "vt", val = c(50))
    withr::defer(emr_filter.rm("fnothing"))

    t <- emr_extract("vt", iterator = "t", filter = "fvt & fnothing")
    expect_equal(t, data.frame(id = as.numeric(), time = as.numeric(), ref = as.numeric(), vt = as.numeric()))

    t <- emr_extract("vt", iterator = "t", filter = "fvt | fnothing")
    expect_equal(t, data.frame(id = c(3, 3), time = c(5, 7), ref = c(-1, -1), vt = c(5, 5)))
})

test_that("filter on vtrack retores the original filter", {
    emr_filter.clear()
    emr_vtrack.clear()

    df <- data.frame(id = c(1, 1, 3, 3), time = c(1, 3, 5, 7), value = c(1, 3, 5, 7))

    emr_track.import("t", src = df, categorical = FALSE)
    withr::defer(emr_track.rm("t", force = TRUE))

    emr_vtrack.create("vt", src = "t", func = "avg", time.shift = c(-2, 0))
    orig_vt <- emr_vtrack.info("vt")
    withr::defer(emr_vtrack.rm("vt"))

    emr_filter.create("fvt", src = "vt", val = 3, operator = ">")
    orig_filter <- emr_filter.info("fvt")
    withr::defer(emr_filter.rm("fvt"))

    t <- emr_extract("vt", iterator = "t", filter = "fvt")
    expect_equal(orig_vt, emr_vtrack.info("vt"))
    expect_equal(orig_filter, emr_filter.info("fvt"))
})

test_that("emr_track.create works with value filters", {
    abnormal_glucose <- emr_screen("track0 > 700", keepref = TRUE) %>% dplyr::distinct(id, time)
    emr_track.create("abnormal_glucose", categorical = FALSE, expr = "track0", iterator = abnormal_glucose)
    f <- emr_filter.create(filter = NULL, src = "track0", val = 700, operator = ">")
    emr_track.create("abnormal_glucose1", categorical = FALSE, expr = "track0", filter = f)
    withr::defer({
        emr_filter.clear()
        emr_vtrack.clear()
        emr_track.rm("abnormal_glucose", force = TRUE)
        emr_track.rm("abnormal_glucose1", force = TRUE)
    })
    a <- emr_extract("abnormal_glucose", names = "v")
    b <- emr_extract("abnormal_glucose1", names = "v")
    expect_equal(a, b)
})

test_that("emr_extract with filter on vtrack is equivalent to emr_screen + emr_extract call", {
    emr_filter.clear()
    emr_vtrack.clear()

    emr_vtrack.create("vt", src = "track0", func = "avg", time.shift = c(-30, 0))
    f <- emr_filter.create(filter = NULL, src = "vt", val = 700, operator = ">")
    iter <- emr_screen("vt > 700")
    a <- emr_extract(c("track0", "vt"), iterator = iter)
    b <- emr_extract(c("track0", "vt"), iterator = "track0", filter = f)
    expect_equal(a, b)
})

test_that("emr_dist works with filters on vtrack", {
    emr_filter.clear()
    emr_vtrack.clear()

    emr_vtrack.create("vt", src = "track0", func = "avg", time.shift = c(-30, 0))
    f <- emr_filter.create(filter = NULL, src = "vt", val = 700, operator = ">")
    iter <- emr_screen("vt > 700")
    breaks <- emr_quantiles("track0", 0:20 / 20)
    a <- emr_dist("track0", breaks, iterator = iter)
    b <- emr_dist("track0", breaks, iterator = "track0", filter = f)
    expect_equal(a, b)

    emr_vtrack.create("vt", src = "track0", func = "earliest", time.shift = c(-30, 0))
    f <- emr_filter.create(filter = NULL, src = "vt", val = 700, operator = ">")
    iter <- emr_screen("vt > 700")
    breaks <- emr_quantiles("track0", 0:20 / 20)
    a <- emr_dist("track0", breaks, iterator = iter)
    b <- emr_dist("track0", breaks, iterator = "track0", filter = f)
    expect_equal(a, b)
})

test_that("emr_cor works with filters on vtrack", {
    emr_filter.clear()
    emr_vtrack.clear()

    emr_vtrack.create("vt", src = "track0", func = "avg", time.shift = c(-30, 0))
    f <- emr_filter.create(filter = NULL, src = "vt", val = 700, operator = ">")
    iter <- emr_screen("vt > 700")
    breaks <- emr_quantiles("track0", 0:20 / 20)

    a <- emr_cor("track0", breaks, cor.exprs = c("track1", "track2"), iterator = iter)
    b <- emr_cor("track0", breaks, cor.exprs = c("track1", "track2"), iterator = "track0", filter = f)
    expect_equal(a, b)
})

test_that("emr_quantiles works with vtrack with filters", {
    emr_filter.clear()
    emr_vtrack.clear()

    emr_vtrack.create("vt", src = "track0", func = "avg", time.shift = c(-30, 0))
    f <- emr_filter.create(filter = NULL, src = "vt", val = 700, operator = ">")
    iter <- emr_screen("vt > 700")

    a <- emr_quantiles("track0", 0:20 / 20, iterator = iter)
    b <- emr_quantiles("track0", 0:20 / 20, iterator = "track0", filter = f)
    expect_equal(a, b)
})

test_that("emr_summary works with filters on vtrack", {
    emr_filter.clear()
    emr_vtrack.clear()

    emr_vtrack.create("vt", src = "track0", func = "avg", time.shift = c(-30, 0))
    f <- emr_filter.create(filter = NULL, src = "vt", val = 700, operator = ">")
    iter <- emr_screen("vt > 700")

    a <- emr_summary("track0", iterator = iter)
    b <- emr_summary("track0", iterator = "track0", filter = f)
    expect_equal(a, b)
})

test_that("emr_screen works with filters on vtrack", {
    emr_filter.clear()
    emr_vtrack.clear()

    emr_vtrack.create("vt", src = "track0", func = "avg", time.shift = c(-30, 0))
    f <- emr_filter.create(filter = NULL, src = "vt", val = 700, operator = ">")
    iter <- emr_screen("vt > 700")

    a <- emr_screen("track0 <= 500", iterator = iter)
    b <- emr_screen("track0 <= 500", iterator = "track0", filter = f)
    expect_equal(a, b)
})

test_that("emr_ids_coverage works with filters on vtrack", {
    emr_filter.clear()
    emr_vtrack.clear()

    emr_vtrack.create("vt", src = "track0", func = "avg", time.shift = c(-30, 0))
    f <- emr_filter.create(filter = NULL, src = "vt", val = 700, operator = ">")

    ids <- emr_extract("track7")

    tracks <- c("track0", "track1", "track3", "track4", "track5", "track6", "track7", "track8")

    b <- emr_ids_coverage(ids, tracks, filter = f)

    purrr::walk(tracks, ~ {
        iter <- emr_screen("vt > 700", iterator = .x, keepref = TRUE) %>% dplyr::distinct(id, time)
        a <- emr_extract(.x, iterator = iter)
        expect_equal(
            a %>%
                dplyr::filter(!is.na(!!rlang::sym(.x))) %>%
                dplyr::distinct(id) %>%
                dplyr::filter(id %in% ids$id) %>%
                nrow(),
            b[.x],
            ignore_attr = TRUE
        )
    })
})

test_that("emr_ids_vals_coverage works with filters on vtrack", {
    emr_filter.clear()
    emr_vtrack.clear()

    emr_vtrack.create("vt", src = "track0", func = "avg", time.shift = c(-30, 0))
    f <- emr_filter.create(filter = NULL, src = "vt", val = 700, operator = ">")

    ids <- emr_extract("track7")

    tracks <- c("track6", "track7", "track8")
    b <- emr_ids_vals_coverage(ids, tracks, filter = f) %>%
        dplyr::mutate(track = as.character(track))

    a <- purrr::map_dfr(tracks, ~ {
        iter <- emr_screen("vt > 700", iterator = .x, keepref = TRUE) %>% dplyr::distinct(id, time, ref)
        track_values <- emr_track.unique(.x)
        emr_extract(.x, iterator = iter, keepref = TRUE) %>%
            dplyr::select(-ref, -time) %>%
            dplyr::filter(id %in% ids$id) %>%
            tidyr::gather("track", "val", -id) %>%
            dplyr::filter(!is.na(val), val != -1) %>%
            dplyr::group_by(track, val) %>%
            dplyr::summarise(count = dplyr::n_distinct(id), .groups = "drop") %>%
            dplyr::mutate(val = factor(val, levels = track_values)) %>%
            tidyr::complete(track, val, fill = list(count = 0)) %>%
            dplyr::mutate(val = as.numeric(as.character(val))) %>%
            dplyr::arrange(track, val) %>%
            as.data.frame()
    })

    expect_equal(a, b)
})

test_that("explicit virtual tracks can be used as filters", {
    emr_filter.clear()
    emr_vtrack.clear()

    emr_vtrack.create("vt", src = "track0", time.shift = c(-30, 0))

    iter <- emr_extract(c("track6", "vt"), iterator = "track6") %>%
        dplyr::filter(!is.na(vt))
    a <- emr_extract(c("track6", "vt"), iterator = iter)
    b <- emr_extract(c("track6", "vt"), iterator = "track6", filter = "vt")
    expect_equal(a, b)

    # without a time shift
    emr_vtrack.create("vt", src = "track0")
    iter <- emr_extract(c("track6", "vt"), iterator = "track6") %>%
        dplyr::filter(!is.na(vt))
    a <- emr_extract(c("track6", "vt"), iterator = iter)
    b <- emr_extract(c("track6", "vt"), iterator = "track6", filter = "vt")
    expect_equal(a, b)
})

test_that("filters on vtracks when vtrack and filter have different time.shift", {
    emr_filter.clear()
    emr_vtrack.clear()

    emr_vtrack.create("vt", src = "track0", func = "avg", time.shift = c(-30, 0))
    emr_track.create("vt_track", categorical = FALSE, expr = "vt", iterator = "track0", keepref = TRUE)
    withr::defer(emr_track.rm("vt_track", force = TRUE))
    f_screen <- emr_filter.create("f_screen", src = "vt_track", val = 700, operator = ">", time.shift = c(-15, 0))
    a <- emr_extract(c("track0", "vt"), iterator = "track0", filter = f_screen)
    f <- emr_filter.create(filter = NULL, src = "vt", val = 700, operator = ">", time.shift = c(-15, 0))
    b <- emr_extract(c("track0", "vt"), iterator = "track0", filter = f)
    expect_equal(a, b)

    # without values
    f_screen <- emr_filter.create("f_screen", src = "vt_track", time.shift = c(-15, 0))
    a <- emr_extract(c("track0", "vt"), iterator = "track0", filter = f_screen)
    f <- emr_filter.create(filter = NULL, src = "vt", time.shift = c(-15, 0))
    b <- emr_extract(c("track0", "vt"), iterator = "track0", filter = f)
    expect_equal(a, b)
})

test_that("filters on vtracks when vtrack and filter have the same time.shift", {
    emr_filter.clear()
    emr_vtrack.clear()

    emr_vtrack.create("vt", src = "track0", func = "avg", time.shift = c(-30, 0))
    emr_track.create("vt_track", categorical = FALSE, expr = "vt", iterator = "track0", keepref = TRUE)
    withr::defer(emr_track.rm("vt_track", force = TRUE))
    f_screen <- emr_filter.create("f_screen", src = "vt_track", val = 700, operator = ">", time.shift = c(-30, 0))
    a <- emr_extract(c("track0", "vt"), iterator = "track0", filter = f_screen)
    f <- emr_filter.create(filter = NULL, src = "vt", val = 700, operator = ">", time.shift = c(-30, 0))
    b <- emr_extract(c("track0", "vt"), iterator = "track0", filter = f)
    expect_equal(a, b)

    # without values
    f_screen <- emr_filter.create("f_screen", src = "vt_track", time.shift = c(-30, 0))
    a <- emr_extract(c("track0", "vt"), iterator = "track0", filter = f_screen)
    f <- emr_filter.create(filter = NULL, src = "vt", time.shift = c(-30, 0))
    b <- emr_extract(c("track0", "vt"), iterator = "track0", filter = f)
    expect_equal(a, b)
})

test_that("filters on vtracks when only vtrack has time.shift", {
    emr_filter.clear()
    emr_vtrack.clear()

    emr_vtrack.create("vt", src = "track0", func = "avg", time.shift = c(-30, 0))
    emr_track.create("vt_track", categorical = FALSE, expr = "vt", iterator = "track0", keepref = TRUE)
    withr::defer(emr_track.rm("vt_track", force = TRUE))
    f_screen <- emr_filter.create("f_screen", src = "vt_track", val = 700, operator = ">")
    a <- emr_extract(c("track0", "vt"), iterator = "track0", filter = f_screen)
    f <- emr_filter.create(filter = NULL, src = "vt", val = 700, operator = ">")
    b <- emr_extract(c("track0", "vt"), iterator = "track0", filter = f)
    expect_equal(a, b)

    # without values
    f_screen <- emr_filter.create("f_screen", src = "vt_track")
    a <- emr_extract(c("track0", "vt"), iterator = "track0", filter = f_screen)
    f <- emr_filter.create(filter = NULL, src = "vt")
    b <- emr_extract(c("track0", "vt"), iterator = "track0", filter = f)
    expect_equal(a, b)
})

test_that("filters on vtracks when only filter has time.shift", {
    emr_filter.clear()
    emr_vtrack.clear()

    emr_vtrack.create("vt", src = "track0", func = "avg")
    emr_track.create("vt_track", categorical = FALSE, expr = "vt", iterator = "track0", keepref = TRUE)
    withr::defer(emr_track.rm("vt_track", force = TRUE))
    f_screen <- emr_filter.create("f_screen", src = "vt_track", val = 700, operator = ">", time.shift = c(-30, 0))
    a <- emr_extract(c("track0", "vt"), iterator = "track0", filter = f_screen)
    f <- emr_filter.create(filter = NULL, src = "vt", val = 700, operator = ">", time.shift = c(-30, 0))
    b <- emr_extract(c("track0", "vt"), iterator = "track0", filter = f)
    expect_equal(a, b)

    # without values
    f_screen <- emr_filter.create("f_screen", src = "vt_track", time.shift = c(-30, 0))
    a <- emr_extract(c("track0", "vt"), iterator = "track0", filter = f_screen)
    f <- emr_filter.create(filter = NULL, src = "vt", time.shift = c(-30, 0))
    b <- emr_extract(c("track0", "vt"), iterator = "track0", filter = f)
    expect_equal(a, b)
})

test_that("filters on vtracks when source virtual track has a filter", {
    emr_filter.clear()
    emr_vtrack.clear()

    emr_filter.create("vt_f", src = "track7", time.shift = c(-5, 0))

    emr_vtrack.create("vt", src = "track0", func = "avg", filter = "vt_f")
    emr_track.create("vt_track", categorical = FALSE, expr = "vt", iterator = "track0", keepref = TRUE, filter = "vt_f")
    withr::defer(emr_track.rm("vt_track", force = TRUE))
    f_screen <- emr_filter.create("f_screen", src = "vt_track", val = 700, operator = ">")
    a <- emr_extract(c("track0", "vt"), iterator = "track0", filter = f_screen)
    f <- emr_filter.create(filter = NULL, src = "vt", val = 700, operator = ">")
    b <- emr_extract(c("track0", "vt"), iterator = "track0", filter = f)
    expect_equal(a, b)

    # without values
    f_screen <- emr_filter.create("f_screen", src = "vt_track")
    a <- emr_extract(c("track0", "vt"), iterator = "track0", filter = f_screen)
    f <- emr_filter.create(filter = NULL, src = "vt")
    b <- emr_extract(c("track0", "vt"), iterator = "track0", filter = f)
    expect_equal(a, b)
})

test_that("filters on vtracks when there is a combination of filters", {
    emr_filter.clear()
    emr_vtrack.clear()

    emr_filter.create("f1", src = "track5", time.shift = c(-20, 0))
    emr_filter.create("f2", src = "track0", time.shift = c(-30, 0), val = 700, operator = ">")

    emr_vtrack.create("vt1", src = "track1", func = "latest", time.shift = c(-5, 0))
    emr_vtrack.create("vt2", src = "track6", func = "earliest", time.shift = c(-10, 0))
    emr_vtrack.create("vt3", src = "track6", time.shift = c(-5, 0), filter = "f1")
    emr_vtrack.create("vt4", src = "track2", func = "min", time.shift = c(-5, 0), filter = "f2")
    emr_vtrack.create("vt5", src = "track3", func = "max")
    emr_vtrack.create("vt6", src = "track4")

    emr_filter.create("fvt_1", src = "vt1", time.shift = c(-5, 0), val = 3, operator = "<=")
    emr_filter.create("fvt_5", src = "vt5", time.shift = c(-20, 0))
    emr_filter.create("fvt_6", src = "vt1", val = 2, operator = "<=")

    f <- "fvt_1 | fvt_5 | vt2 & (vt3 | vt4) | (fvt_6 & f1) | (f2 & track1) | vt6"

    itr0 <- emr_extract("vt6", iterator = "track0") %>%
        dplyr::filter(!is.na(vt6)) %>%
        dplyr::distinct(id, time) # filter: vt6
    itr1 <- emr_extract("track0", iterator = "track0", filter = "f2 & track1") %>%
        dplyr::distinct(id, time) # filter: (f2 & track1)
    itr2 <- emr_extract("vt1", iterator = "track0", filter = "f1") %>%
        dplyr::filter(!is.na(vt1), vt1 <= 2) %>%
        dplyr::distinct(id, time) # filter: (fvt_6 & f1)
    itr3 <- emr_extract(c("vt2", "vt3", "vt4"), iterator = "track0") %>%
        dplyr::filter(!is.na(vt2) & (!is.na(vt3) | !is.na(vt4))) %>%
        dplyr::distinct(id, time) # filter: vt2 & (vt3 | vt4)

    # filter: fvt_1 & fvt_5
    emr_track.create("vt1_track", categorical = FALSE, expr = "vt1", iterator = "track0", keepref = TRUE)
    emr_filter.create("fvt1_track", src = "vt1_track", time.shift = c(-5, 0), val = 3, operator = "<=")
    emr_track.create("vt5_track", categorical = FALSE, expr = "vt5", iterator = "track3", keepref = TRUE)
    emr_filter.create("fvt5_track", src = "vt5_track", time.shift = c(-20, 0))
    withr::defer(emr_track.rm("vt1_track", force = TRUE))
    withr::defer(emr_track.rm("vt5_track", force = TRUE))


    a <- emr_extract("track0", iterator = "track0", filter = "fvt1_track | fvt5_track | itr3 | itr2 | itr1 | itr0")
    b <- emr_extract("track0", iterator = "track0", filter = f)
    expect_equal(a, b)
})

test_that("Cannot apply virtual track filters on virtual tracks (in order to avoid loops)", {
    emr_filter.clear()
    emr_vtrack.clear()

    emr_vtrack.create("vt", "track0")
    emr_filter.create("fvt", "vt")
    expect_error(emr_vtrack.create("vt", "track0", filter = "fvt"))
})
