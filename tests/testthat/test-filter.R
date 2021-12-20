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

test_that("Cannot use an implicit time scope with Beat Iteator: please specify 'stime' and 'etime'", {
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
            val = NULL, expiration = NULL, operator = "=", use_values=FALSE
        )
    )
})

test_that("emr_filter.create works with -1 as val (kr issues)", {
    emr_filter.clear()
    df <- data.frame(id = 1, time = c(1, 2, 2), value = c(-1, 4, 3), ref = c(0, 0, 1))
    emr_track.import("minusone", space = EMR_UROOT, categorical = TRUE, src = df)
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
    withr::with_options(list(scipen = 1), {
        fname <- emr_filter.name("ph1", keepref = TRUE, val = 1e9)
        expect_equal(fname, "f_ph1.krT.vals_1000000000")
        emr_filter.create_from_name(fname)
        expect_equal(emr_filter.info(fname)$val, 1e9)
    })
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

test_that("test filter on vtrack with avg", {
    emr_filter.clear()
    emr_vtrack.clear()

    df <- data.frame(id=c(1, 1, 3, 3), time=c(1, 3, 5, 7), value=c(1, 3, 5, 7))

    emr_track.import("t", src=df, categorical=FALSE)
    withr::defer(emr_track.rm("t", force=TRUE))

    emr_vtrack.create("vt", src="t", func="avg", time.shift=c(-2,0))
    withr::defer(emr_vtrack.rm("vt"))

    expect_error(emr_filter.create("fvt", src="vt", operator=">", use_values=TRUE))
    emr_filter.create("fvt", src="vt", val=3, operator=">", use_values=TRUE)

    withr::defer(emr_filter.rm("fvt"))
    
    t <- emr_extract("vt", iterator="t", filter="fvt")

    expect_equal(t, data.frame(id=c(3, 3), time=c(5, 7), ref=c(-1, -1), vt=c(5, 6)))
})