test_that("emr_vtrack fails when track deosn't exist", {
    expect_error(emr_vtrack.create("v1", "blabla"))
})

test_that("function min does not accept any parameters", {
    expect_error(emr_vtrack.create("v1", "track1", func = "min", params = 2))
})

test_that("Function min is not supported when keepref is 'TRUE'", {
    expect_error(emr_vtrack.create("v1", "track1", func = "min", keepref = T))
})

test_that("Function value is not supported with quantitative data", {
    expect_error(emr_vtrack.create("v1", "track1", func = "value"))
})

test_that("Function min is not supported with categorical data", {
    expect_error(emr_vtrack.create("v1", "track6", func = "min"))
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track6", func = "value")
    expect_regression(emr_extract("v1"), "vtrack.1")
})

test_that("function exists requires an additional parameter", {
    EMR_VTRACKS <<- list()
    expect_error(emr_vtrack.create("v1", "track6", func = "exists"))
    expect_error(emr_extract("v1"))
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track7", func = "exists", params = c(1:8), time.shift = c(-20, 30))
    expect_regression(emr_extract("v1"), "vtrack.2")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track7", func = "frequent", time.shift = c(-100, 200))
    expect_regression(emr_extract("v1"), "vtrack.3")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track7", func = "size", time.shift = c(-100, 200))
    expect_regression(emr_extract("v1"), "vtrack.4")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track7", func = "size", params = c(2:5), time.shift = c(-100, 200))
    expect_regression(emr_extract("v1"), "vtrack.5")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track0", func = "size", time.shift = c(-100, 200))
    expect_regression(emr_extract("v1"), "vtrack.6")
})

test_that("function size does not accept any parameters when applied to quantative data", {
    EMR_VTRACKS <<- list()
    expect_error(emr_vtrack.create("v1", "track0", func = "size", params = c(2:5), time.shift = c(-100, 200)))
    expect_error(emr_extract("v1"))
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track8", func = "earliest", time.shift = c(-100, 200))
    expect_regression(emr_extract("v1", stime = 100, etime = 500), "vtrack.7")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track8", func = "earliest", params = c(2:5), time.shift = c(-100, 200))
    expect_regression(emr_extract("v1", stime = 100, etime = 500), "vtrack.8")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track8", func = "latest", time.shift = c(-100, 200))
    expect_regression(emr_extract("v1", stime = 100, etime = 500), "vtrack.9")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track8", func = "latest", params = c(2:5), time.shift = c(-100, 200))
    expect_regression(emr_extract("v1", stime = 100, etime = 500), "vtrack.10")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track8", func = "closest", time.shift = c(-100, 200))
    expect_regression(emr_extract("v1", stime = 100, etime = 500), "vtrack.11")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track8", func = "closest", params = c(2:5), time.shift = c(-100, 200))
    expect_regression(emr_extract("v1", stime = 100, etime = 500), "vtrack.12")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track0", func = "stddev")
    expect_regression(emr_extract("v1", stime = 100, etime = 500), "vtrack.13")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track0", func = "stddev", time.shift = c(-100, 200))
    expect_regression(emr_extract("v1", stime = 100, etime = 500), "vtrack.14")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track1")
    expect_regression(emr_extract("v1"), "vtrack.15")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track1", keepref = T)
    expect_regression(emr_extract("v1"), "vtrack.16")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track1", keepref = T)
    expect_regression(emr_extract("v1", keepref = T), "vtrack.17")
})

test_that("Function max is not supported when keepref is 'TRUE'", {
    expect_error(emr_vtrack.create("v1", "track1", func = "max", keepref = T))
})

test_that("Time shift is not allowed when keepref is 'TRUE'", {
    expect_error(emr_vtrack.create("v1", "track1", func = "avg", keepref = T, time.shift = 2))
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track1", func = "percentile.upper", keepref = T)
    expect_regression(emr_extract("v1", keepref = T), "vtrack.18")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track1", func = "percentile.lower", keepref = T)
    expect_regression(emr_extract("v1", keepref = T), "vtrack.19")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track2", func = "avg", keepref = F, time.shift = 2)
    expect_regression(emr_extract("v1", keepref = T), "vtrack.20")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track2", func = "avg", keepref = F, time.shift = c(100000, 200000))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = T), "vtrack.21")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track2", func = "avg", keepref = F, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = T), "vtrack.22")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track2", func = "min", keepref = F, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = T), "vtrack.23")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track2", func = "max", keepref = F, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = T), "vtrack.24")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track2", func = "earliest", keepref = F, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = T), "vtrack.25")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track2", func = "latest", keepref = F, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = T), "vtrack.26")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track2", func = "closest", keepref = F, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = T), "vtrack.27")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track2", func = "earliest.time", keepref = F, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = T), "vtrack.28")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track2", func = "latest.time", keepref = F, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = T), "vtrack.29")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track2", func = "closest.earlier.time", keepref = F, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = T), "vtrack.30")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track2", func = "closest.later.time", keepref = F, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = T), "vtrack.31")
})

test_that("function quantile requires an additional parameter - percentile", {
    expect_error(emr_vtrack.create("v1", "track2", func = "quantile", keepref = F, time.shift = c(-10, 20)))
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track2", func = "quantile", keepref = F, time.shift = c(-10, 20), params = 0.5)
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = T), "vtrack.32")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track2", func = "sum", keepref = F, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = T), "vtrack.33")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track2", func = "lm.intercept", keepref = F, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = T), "vtrack.34")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track2", func = "lm.slope", keepref = F, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = T), "vtrack.35")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track2", func = "percentile.upper", keepref = F, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = T), "vtrack.36")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track2", func = "percentile.upper.min", keepref = F, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = T), "vtrack.37")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track2", func = "percentile.upper.max", keepref = F, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = T), "vtrack.38")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track2", func = "percentile.lower", keepref = F, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = T), "vtrack.39")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track2", func = "percentile.lower.min", keepref = F, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = T), "vtrack.40")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track2", func = "percentile.lower.max", keepref = F, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = T), "vtrack.41")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track2", func = "dt1.earliest", keepref = F, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = T), "vtrack.42")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track2", func = "dt1.latest", keepref = F, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = T), "vtrack.43")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track2", func = "dt2.earliest", keepref = F, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = T), "vtrack.44")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track2", func = "dt2.latest", keepref = F, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = T), "vtrack.45")
})

test_that("emr_vtrack works", {
    set.seed(0)
    id2id <- data.frame(id1 = sample(0:999, 500), id2 = sample(0:999, 500))
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track1", func = "avg", id.map = id2id)
    expect_regression(emr_extract("v1", iterator = "track2"), "vtrack.46")
})

test_that("emr_vtrack works", {
    set.seed(0)
    id2id <- data.frame(id1 = sample(0:999, 500), id2 = sample(0:999, 500), time.shift = sample(-100:200, 500, replace = T))
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track1", func = "avg", id.map = id2id)
    expect_regression(emr_extract("v1", iterator = "track2"), "vtrack.47")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track1", filter = "!track0")
    expect_regression(emr_extract("v1"), "vtrack.48")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track1", filter = "track0")
    expect_regression(emr_extract("v1"), "vtrack.49")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track1", filter = "!track0", func = "percentile.upper")
    expect_regression(emr_extract("v1"), "vtrack.50")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track1", filter = "track0", func = "percentile.upper")
    expect_regression(emr_extract("v1"), "vtrack.51")
})

test_that("emr_vtrack works", {
    EMR_FILTERS <<- list()
    emr_filter.create("f1", "track0", keepref = T)
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track1", filter = "!f1", keepref = T)
    expect_regression(emr_extract("v1", keepref = T), "vtrack.52")
})


test_that("emr_vtrack works", {
    EMR_FILTERS <<- list()
    emr_filter.create("f1", "track0", keepref = T)
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track1", filter = "f1", keepref = T)
    expect_regression(emr_extract("v1", keepref = T), "vtrack.53")
})

test_that("emr_vtrack works", {
    EMR_FILTERS <<- list()
    emr_filter.create("f1", "track0", keepref = T)
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track1", filter = "!f1", keepref = T, func = "percentile.upper")
    expect_regression(emr_extract("v1", keepref = T), "vtrack.54")
})

test_that("emr_vtrack works", {
    EMR_FILTERS <<- list()
    emr_filter.create("f1", "track0", keepref = T)
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track1", filter = "f1", keepref = T, func = "percentile.upper")
    expect_regression(emr_extract("v1", keepref = T), "vtrack.55")
})

test_that("Function max is not supported with categorical data", {
    EMR_VTRACKS <<- list()
    r <- emr_extract("track0", keepref = T, names = "value")
    expect_error(emr_vtrack.create("v1", list(r, T), func = "max", time.shift = c(-10, 20)))
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    r <- emr_extract("track0", keepref = T, names = "value")
    emr_vtrack.create("v1", list(r, F), func = "max", time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", keepref = T), "vtrack.56")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    r <- emr_extract("track0", keepref = T, names = "value")
    emr_vtrack.create("v1", list(r, T), func = "closest", time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", keepref = T), "vtrack.57")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    r <- emr_extract("track0", keepref = F, names = "value")
    r$ref <- NULL
    emr_vtrack.create("v1", list(r, F), func = "max", time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", keepref = T), "vtrack.58")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    r <- emr_extract("track0", keepref = F, names = "value")
    r$ref <- NULL
    emr_vtrack.create("v1", list(r, T), func = "closest", time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", keepref = T), "vtrack.59")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    r <- emr_extract("track0", keepref = F, names = "value")
    emr_vtrack.create("v1", list(r, F), func = "percentile.upper", time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", keepref = T), "vtrack.60")
})

test_that("emr_vtrack works with a single NA value as params", {
    EMR_VTRACKS <<- list()
    test_with_func <- function(func) {
        emr_vtrack.create("v1", "ph1", func = func, time.shift = c(-10, 20), params = NA)
        emr_vtrack.create("v2", "ph1", func = func, time.shift = c(-10, 20), params = 800)
        expect_equal(
            emr_extract("v1", names = "value"),
            emr_extract("v2", names = "value")
        )
    }

    categorical_funcs <- c("exists", "value", "sample", "sample.time", "frequent", "size", "earliest", "latest", "earliest.time", "latest.time", "closest.earlier.time", "closest.later.time", "dt1.earliest", "dt1.latest", "dt2.earliest", "dt2.latest")

    for (f in categorical_funcs) {
        test_with_func(f)
    }
})

test_that("emr_vtrack fails when params are invalid", {
    EMR_VTRACKS <<- list()
    expect_error(emr_vtrack.create("v1", "ph1", func = "exists", time.shift = c(-10, 20), params = c(NA, NA)))
    expect_error(emr_vtrack.create("v1", "ph1", func = "exists", time.shift = c(-10, 20), params = c(TRUE)))
    expect_error(emr_vtrack.create("v1", "ph1", func = "exists", time.shift = c(-10, 20), params = c("savta")))
    expect_error(emr_vtrack.create("v1", "ph1", func = "exists", time.shift = c(-10, 20), params = c(15, NA)))
})

test_that("filter cannot be used when 'src' is a data frame", {
    EMR_VTRACKS <<- list()
    r <- emr_extract("track0", keepref = F, names = "value")
    expect_error(emr_vtrack.create("v1", list(r, F), func = "percentile.upper", time.shift = c(-10, 20), filter = "track1"))
})

test_that("Unable to implicitly set iterator policy with vtracks", {
    EMR_VTRACKS <<- list()
    r <- emr_extract("track0", keepref = F, names = "value")
    emr_vtrack.create("v1", list(r, F), func = "percentile.upper", time.shift = c(-10, 20))
    expect_error(emr_extract(c("v1", "track1"), keepref = T))
})

test_that("Unable to implicitly set iterator policy with logical tracks", {
    EMR_VTRACKS <<- list()
    emr_track.create_logical("logical_track1", "ph1", c(15, 16))
    withr::defer(emr_track.logical.rm("logical_track1", force = TRUE))
    expect_error(emr_extract(c("logical_track1", "track1"), keepref = T))
})


test_that("emr_vtrack.attr.src works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track1")
    emr_vtrack.attr.src("v1", "track2")
    expect_equal(emr_vtrack.attr.src("v1"), "track2")
})

test_that("emr_vtrack.attr.src works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track1")
    expect_error(emr_vtrack.attr.src("v1", "track10"))
    expect_equal(emr_vtrack.attr.src("v1"), "track1")
})

test_that("Invalid source used in a virtual track", {
    EMR_VTRACKS <<- list()
    expect_error(emr_vtrack.create("v1", "track10"))
    r <- emr_extract("track0", keepref = F, names = "value")
    expect_error(emr_vtrack.attr.src("v1", list(head(r), F)))
    expect_error(emr_vtrack.attr.src("v1"))
})

test_that("emr_vtrack.attr.func works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track1")
    emr_vtrack.attr.func("v1", "value")
    expect_equal(emr_vtrack.attr.func("v1"), "value")
})

test_that("emr_vtrack.attr.params works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track1")
    emr_vtrack.attr.params("v1", 26)
    expect_equal(emr_vtrack.attr.params("v1"), 26)
})

test_that("emr_vtrack.attr.keepref works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track1")
    emr_vtrack.attr.keepref("v1", T)
    expect_true(emr_vtrack.attr.keepref("v1"))
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track1")
    emr_vtrack.attr.time.shift("v1", c(-10, 20))
    expect_equal(emr_vtrack.attr.time.shift("v1"), c(-10, 20))
})

test_that("emr_vtrack.attr.id.map works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track1")
    emr_vtrack.attr.id.map("v1", data.frame(id1 = 10, id2 = 20))
    expect_equal(
        emr_vtrack.attr.id.map("v1"),
        structure(list(id1 = 10, id2 = 20), class = "data.frame", row.names = c(
            NA,
            -1L
        ))
    )
})

test_that("emr_vtrack.attr.filter works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track1")
    emr_vtrack.attr.filter("v1", "track2 & track3")
    # NOTE: it returns a call - not a string!
    expect_equal(emr_vtrack.attr.filter("v1"), quote(track2 & track3))
})

test_that("emr_vtrack.exists works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track2", func = "dt2.latest", keepref = F, time.shift = c(-10, 20))
    emr_vtrack.create("v2", "track2", func = "dt2.latest", keepref = F, time.shift = c(-10, 20))
    expect_true(emr_vtrack.exists("v1"))
})

test_that("emr_vtrack.exists works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track2", func = "dt2.latest", keepref = F, time.shift = c(-10, 20))
    emr_vtrack.create("v2", "track2", func = "dt2.latest", keepref = F, time.shift = c(-10, 20))
    expect_false(emr_vtrack.exists("sdaf"))
})

test_that("emr_vtrack.info works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track2", func = "dt2.latest", keepref = F, time.shift = c(-10, 20))
    expect_equal(
        emr_vtrack.info("v1"),
        list(
            src = "track2", time_shift = c(-10, 20), func = "dt2.latest",
            params = NULL, keepref = FALSE, id_map = NULL, filter = NULL
        )
    )
})

test_that("emr_vtrack.ls works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track2", func = "dt2.latest", keepref = F, time.shift = c(-10, 20))
    emr_vtrack.create("v2", "track2", func = "dt2.latest", keepref = F, time.shift = c(-10, 20))
    expect_equal(emr_vtrack.ls(), c("v1", "v2"))
})

test_that("emr_vtrack.ls works", {
    EMR_VTRACKS <<- list()
    expect_error(emr_vtrack.create("v1", "track10"))
    emr_vtrack.create("v2", "track1")
    expect_equal(emr_vtrack.ls(), "v2")
})

test_that("emr_vtrack.ls works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track2", func = "dt2.latest", keepref = F, time.shift = c(-10, 20))
    emr_vtrack.create("v2", "track2", func = "dt2.latest", keepref = F, time.shift = c(-10, 20))
    emr_vtrack.rm("v1")
    expect_equal(emr_vtrack.ls(), "v2")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track7", func = "value")
    expect_regression(emr_extract("v1"), "vtrack.61")
})

test_that("emr_vtrack works", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track7", func = "frequent", params = c(2:5), time.shift = c(-100, 200))
    expect_regression(emr_extract("v1"), "vtrack.62")
})

test_that("vtrack and extract return the same", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("vt", "track1")
    expect_equal(emr_extract("vt"), emr_extract("track1", names = "vt"))
})
