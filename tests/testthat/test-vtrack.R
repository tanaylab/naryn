load_test_db()


test_that("emr_vtrack fails when track deosn't exist", {
    expect_error(emr_vtrack.create("v1", "blabla"))
})

test_that("function min does not accept any parameters", {
    expect_error(emr_vtrack.create("v1", "track1", func = "min", params = 2))
})

test_that("Function min is not supported when keepref is 'TRUE'", {
    expect_error(emr_vtrack.create("v1", "track1", func = "min", keepref = TRUE))
})

test_that("Function value is not supported with quantitative data", {
    expect_error(emr_vtrack.create("v1", "track1", func = "value"))
})

test_that("Function min is not supported with categorical data", {
    expect_error(emr_vtrack.create("v1", "track6", func = "min"))
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    vt_name <- emr_vtrack.create("v1", "track6", func = "value")
    expect_regression(emr_extract("v1"), "vtrack.1")
    expect_equal(vt_name, "v1")
})

test_that("function exists without additional parameters looks for all of them", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track6", func = "exists")
    emr_vtrack.create("size", "track6", func = "size")
    res <- emr_extract(c("v1", "size"), iterator = "track1") %>%
        mutate(f = as.numeric(size > 0))
    expect_equal(res$f, res$v1)
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track7", func = "exists", params = c(1:8), time.shift = c(-20, 30))
    expect_regression(emr_extract("v1"), "vtrack.2")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track7", func = "frequent", time.shift = c(-100, 200))
    expect_regression(emr_extract("v1"), "vtrack.3")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track7", func = "size", time.shift = c(-100, 200))
    expect_regression(emr_extract("v1"), "vtrack.4")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track7", func = "size", params = c(2:5), time.shift = c(-100, 200))
    expect_regression(emr_extract("v1"), "vtrack.5")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track0", func = "size", time.shift = c(-100, 200))
    expect_regression(emr_extract("v1"), "vtrack.6")
})

test_that("function size does not accept any parameters when applied to quantative data", {
    emr_vtrack.clear()
    expect_error(emr_vtrack.create("v1", "track0", func = "size", params = c(2:5), time.shift = c(-100, 200)))
    expect_error(emr_extract("v1"))
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track8", func = "earliest", time.shift = c(-100, 200))
    expect_regression(emr_extract("v1", stime = 100, etime = 500), "vtrack.7")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track8", func = "earliest", params = c(2:5), time.shift = c(-100, 200))
    expect_regression(emr_extract("v1", stime = 100, etime = 500), "vtrack.8")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track8", func = "latest", time.shift = c(-100, 200))
    expect_regression(emr_extract("v1", stime = 100, etime = 500), "vtrack.9")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track8", func = "latest", params = c(2:5), time.shift = c(-100, 200))
    expect_regression(emr_extract("v1", stime = 100, etime = 500), "vtrack.10")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track8", func = "closest", time.shift = c(-100, 200))
    expect_regression(emr_extract("v1", stime = 100, etime = 500), "vtrack.11")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track8", func = "closest", params = c(2:5), time.shift = c(-100, 200))
    expect_regression(emr_extract("v1", stime = 100, etime = 500), "vtrack.12")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track0", func = "stddev")
    expect_regression(emr_extract("v1", stime = 100, etime = 500), "vtrack.13")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track0", func = "stddev", time.shift = c(-100, 200))
    expect_regression(emr_extract("v1", stime = 100, etime = 500), "vtrack.14")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1")
    expect_regression(emr_extract("v1"), "vtrack.15")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1", keepref = TRUE)
    expect_regression(emr_extract("v1"), "vtrack.16")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1", keepref = TRUE)
    expect_regression(emr_extract("v1", keepref = TRUE), "vtrack.17")
})

test_that("Function max is not supported when keepref is 'TRUE'", {
    expect_error(emr_vtrack.create("v1", "track1", func = "max", keepref = TRUE))
})

test_that("Time shift is not allowed when keepref is 'TRUE'", {
    expect_error(emr_vtrack.create("v1", "track1", func = "avg", keepref = TRUE, time.shift = 2))
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1", func = "percentile.upper", keepref = TRUE)
    expect_regression(emr_extract("v1", keepref = TRUE), "vtrack.18")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1", func = "percentile.lower", keepref = TRUE)
    expect_regression(emr_extract("v1", keepref = TRUE), "vtrack.19")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "avg", keepref = FALSE, time.shift = 2)
    expect_regression(emr_extract("v1", keepref = TRUE), "vtrack.20")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "avg", keepref = FALSE, time.shift = c(100000, 200000))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.21")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "avg", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.22")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "min", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.23")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "max", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.24")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "earliest", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.25")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "latest", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.26")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "closest", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.27")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "earliest.time", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.28")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "latest.time", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.29")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "closest.earlier.time", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.30")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "closest.later.time", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.31")
})

test_that("function quantile requires an additional parameter - percentile", {
    expect_error(emr_vtrack.create("v1", "track2", func = "quantile", keepref = FALSE, time.shift = c(-10, 20)))
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "quantile", keepref = FALSE, time.shift = c(-10, 20), params = 0.5)
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.32")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "sum", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.33")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "lm.intercept", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.34")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "lm.slope", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.35")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "percentile.upper", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.36")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "percentile.upper.min", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.37")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "percentile.upper.max", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.38")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "percentile.lower", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.39")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "percentile.lower.min", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.40")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "percentile.lower.max", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.41")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "dt1.earliest", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.42")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "dt1.latest", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.43")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "dt2.earliest", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.44")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "dt2.latest", keepref = FALSE, time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", stime = 10, etime = 500, keepref = TRUE), "vtrack.45")
})

test_that("emr_vtrack works", {
    set.seed(0)
    id2id <- data.frame(id1 = sample(0:999, 500), id2 = sample(0:999, 500))
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1", func = "avg", id.map = id2id)
    expect_regression(emr_extract("v1", iterator = "track2"), "vtrack.46")
})

test_that("emr_vtrack works", {
    set.seed(0)
    id2id <- data.frame(id1 = sample(0:999, 500), id2 = sample(0:999, 500), time.shift = sample(-100:200, 500, replace = TRUE))
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1", func = "avg", id.map = id2id)
    expect_regression(emr_extract("v1", iterator = "track2"), "vtrack.47")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1", filter = "!track0")
    expect_regression(emr_extract("v1"), "vtrack.48")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1", filter = "track0")
    expect_regression(emr_extract("v1"), "vtrack.49")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1", filter = "!track0", func = "percentile.upper")
    expect_regression(emr_extract("v1"), "vtrack.50")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1", filter = "track0", func = "percentile.upper")
    expect_regression(emr_extract("v1"), "vtrack.51")
})

test_that("emr_vtrack works", {
    emr_filter.clear()
    emr_filter.create("f1", "track0", keepref = TRUE)
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1", filter = "!f1", keepref = TRUE)
    expect_regression(emr_extract("v1", keepref = TRUE), "vtrack.52")
})


test_that("emr_vtrack works", {
    emr_filter.clear()
    emr_filter.create("f1", "track0", keepref = TRUE)
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1", filter = "f1", keepref = TRUE)
    expect_regression(emr_extract("v1", keepref = TRUE), "vtrack.53")
})

test_that("emr_vtrack works", {
    emr_filter.clear()
    emr_filter.create("f1", "track0", keepref = TRUE)
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1", filter = "!f1", keepref = TRUE, func = "percentile.upper")
    expect_regression(emr_extract("v1", keepref = TRUE), "vtrack.54")
})

test_that("emr_vtrack works", {
    emr_filter.clear()
    emr_filter.create("f1", "track0", keepref = TRUE)
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1", filter = "f1", keepref = TRUE, func = "percentile.upper")
    expect_regression(emr_extract("v1", keepref = TRUE), "vtrack.55")
})

test_that("Function max is not supported with categorical data", {
    emr_vtrack.clear()
    r <- emr_extract("track0", keepref = TRUE, names = "value")
    expect_error(emr_vtrack.create("v1", list(r, TRUE), func = "max", time.shift = c(-10, 20)))
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    r <- emr_extract("track0", keepref = TRUE, names = "value")
    emr_vtrack.create("v1", list(r, FALSE), func = "max", time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", keepref = TRUE), "vtrack.56")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    r <- emr_extract("track0", keepref = TRUE, names = "value")
    emr_vtrack.create("v1", list(r, TRUE), func = "closest", time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", keepref = TRUE), "vtrack.57")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    r <- emr_extract("track0", keepref = FALSE, names = "value")
    r$ref <- NULL
    emr_vtrack.create("v1", list(r, FALSE), func = "max", time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", keepref = TRUE), "vtrack.58")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    r <- emr_extract("track0", keepref = FALSE, names = "value")
    r$ref <- NULL
    emr_vtrack.create("v1", list(r, TRUE), func = "closest", time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", keepref = TRUE), "vtrack.59")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    r <- emr_extract("track0", keepref = FALSE, names = "value")
    emr_vtrack.create("v1", list(r, FALSE), func = "percentile.upper", time.shift = c(-10, 20))
    expect_regression(emr_extract("v1", keepref = TRUE), "vtrack.60")
})

test_that("emr_vtrack works with a single NA value as params", {
    emr_vtrack.clear()
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
    emr_vtrack.clear()
    expect_error(emr_vtrack.create("v1", "ph1", func = "exists", time.shift = c(-10, 20), params = c(NA, NA)))
    expect_error(emr_vtrack.create("v1", "ph1", func = "exists", time.shift = c(-10, 20), params = c(TRUE)))
    expect_error(emr_vtrack.create("v1", "ph1", func = "exists", time.shift = c(-10, 20), params = c("savta")))
    expect_error(emr_vtrack.create("v1", "ph1", func = "exists", time.shift = c(-10, 20), params = c(15, NA)))
})

test_that("filter cannot be used when 'src' is a data frame", {
    emr_vtrack.clear()
    r <- emr_extract("track0", keepref = FALSE, names = "value")
    expect_error(emr_vtrack.create("v1", list(r, FALSE), func = "percentile.upper", time.shift = c(-10, 20), filter = "track1"))
})

test_that("Unable to implicitly set iterator policy with vtracks", {
    emr_vtrack.clear()
    r <- emr_extract("track0", keepref = FALSE, names = "value")
    emr_vtrack.create("v1", list(r, FALSE), func = "percentile.upper", time.shift = c(-10, 20))
    expect_error(emr_extract(c("v1", "track1"), keepref = TRUE))
})

test_that("Unable to implicitly set iterator policy with logical tracks", {
    emr_vtrack.clear()
    emr_track.logical.create("logical_track1", "ph1", c(15, 16))
    withr::defer(emr_track.logical.rm("logical_track1", force = TRUE))
    expect_error(emr_extract(c("logical_track1", "track1"), keepref = TRUE))
})


test_that("emr_vtrack.attr.src works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1")
    emr_vtrack.attr.src("v1", "track2")
    expect_equal(emr_vtrack.attr.src("v1"), "track2")
})

test_that("emr_vtrack.attr.src works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1")
    expect_error(emr_vtrack.attr.src("v1", "track10"))
    expect_equal(emr_vtrack.attr.src("v1"), "track1")
})

test_that("Invalid source used in a virtual track", {
    emr_vtrack.clear()
    expect_error(emr_vtrack.create("v1", "track10"))
    r <- emr_extract("track0", keepref = FALSE, names = "value")
    expect_error(emr_vtrack.attr.src("v1", list(head(r), FALSE)))
    expect_error(emr_vtrack.attr.src("v1"))
})

test_that("emr_vtrack.attr.func works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1")
    emr_vtrack.attr.func("v1", "value")
    expect_equal(emr_vtrack.attr.func("v1"), "value")
})

test_that("emr_vtrack.attr.params works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1")
    emr_vtrack.attr.params("v1", 26)
    expect_equal(emr_vtrack.attr.params("v1"), 26)
})

test_that("emr_vtrack.attr.keepref works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1")
    emr_vtrack.attr.keepref("v1", TRUE)
    expect_true(emr_vtrack.attr.keepref("v1"))
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1")
    emr_vtrack.attr.time.shift("v1", c(-10, 20))
    expect_equal(emr_vtrack.attr.time.shift("v1"), c(-10, 20))
})

test_that("emr_vtrack.attr.id.map works", {
    emr_vtrack.clear()
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
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track1")
    emr_vtrack.attr.filter("v1", "track2 & track3")
    # NOTE: it returns a call - not a string!
    expect_equal(emr_vtrack.attr.filter("v1"), quote(track2 & track3))
})

test_that("emr_vtrack.exists works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "dt2.latest", keepref = FALSE, time.shift = c(-10, 20))
    emr_vtrack.create("v2", "track2", func = "dt2.latest", keepref = FALSE, time.shift = c(-10, 20))
    expect_true(emr_vtrack.exists("v1"))
})

test_that("emr_vtrack.exists works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "dt2.latest", keepref = FALSE, time.shift = c(-10, 20))
    emr_vtrack.create("v2", "track2", func = "dt2.latest", keepref = FALSE, time.shift = c(-10, 20))
    expect_false(emr_vtrack.exists("sdaf"))
})

test_that("emr_vtrack.info works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "dt2.latest", keepref = FALSE, time.shift = c(-10, 20))
    expect_equal(
        emr_vtrack.info("v1"),
        list(
            src = "track2", time_shift = c(-10, 20), func = "dt2.latest",
            params = NULL, keepref = FALSE, id_map = NULL, filter = NULL
        )
    )
})

test_that("emr_vtrack.ls works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "dt2.latest", keepref = FALSE, time.shift = c(-10, 20))
    emr_vtrack.create("v2", "track2", func = "dt2.latest", keepref = FALSE, time.shift = c(-10, 20))
    expect_equal(emr_vtrack.ls(), c("v1", "v2"))
})

test_that("emr_vtrack.ls works", {
    emr_vtrack.clear()
    expect_error(emr_vtrack.create("v1", "track10"))
    emr_vtrack.create("v2", "track1")
    expect_equal(emr_vtrack.ls(), "v2")
})

test_that("emr_vtrack.ls works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "dt2.latest", keepref = FALSE, time.shift = c(-10, 20))
    emr_vtrack.create("v2", "track2", func = "dt2.latest", keepref = FALSE, time.shift = c(-10, 20))
    emr_vtrack.rm("v1")
    expect_equal(emr_vtrack.ls(), "v2")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track7", func = "value")
    expect_regression(emr_extract("v1"), "vtrack.61")
})

test_that("emr_vtrack works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track7", func = "frequent", params = c(2:5), time.shift = c(-100, 200))
    expect_regression(emr_extract("v1"), "vtrack.62")
})

test_that("vtrack and extract return the same", {
    emr_vtrack.clear()
    emr_vtrack.create("vt", "track1")
    expect_equal(emr_extract("vt"), emr_extract("track1", names = "vt"))
})

test_that("emr_vtrack.clear works", {
    emr_vtrack.clear()
    expect_equal(emr_vtrack.ls(), character(0))
    emr_vtrack.create("v1", "track2", func = "dt2.latest", keepref = FALSE, time.shift = c(-10, 20))
    emr_vtrack.create("v2", "track2", func = "dt2.latest", keepref = FALSE, time.shift = c(-10, 20))
    emr_vtrack.clear()
    expect_equal(emr_vtrack.ls(), character(0))
})

test_that("logical_to_varname works correctly", {
    expect_equal(logical_to_varname("x > 10 & y < 20"), "x__gt__10__and__y__lt__20")
    expect_equal(logical_to_varname("x = 5 | y != 7"), "x__eq__5__or__y__not____eq__7")
    expect_equal(logical_to_varname("(a & b) | c"), "__ob__a__and__b__cb____or__c")
    expect_equal(logical_to_varname("a = b & (c < d | e > f)"), "a__eq__b__and____ob__c__lt__d__or__e__gt__f__cb__")
    expect_error(logical_to_varname("invalid__gt__string"), "Invalid input")
})

test_that("varname_to_logical works correctly", {
    expect_equal(varname_to_logical("x__gt__10__and__y__lt__20"), gsub(" ", "", "x > 10 & y < 20"))
    expect_equal(varname_to_logical("x__eq__5__or__y__not____eq__7"), gsub(" ", "", "x = 5 | y != 7"))
    expect_equal(varname_to_logical("__ob__a__and__b__cb____or__c"), gsub(" ", "", "(a & b) | c"))
    expect_equal(varname_to_logical("a__eq__b__and____ob__c__lt__d__or__e__gt__f__cb__"), gsub(" ", "", "a = b & (c < d | e > f)"))
})

# Test that conversion is reversible
test_that("conversion is reversible", {
    original_expr1 <- "x > 10 & y < 20"
    original_expr2 <- "x = 5 | y != 7"
    original_expr3 <- "(a & b) | c"
    original_expr4 <- "a = b & (c < d | e > f)"
    expect_equal(varname_to_logical(logical_to_varname(original_expr1)), gsub(" ", "", original_expr1))
    expect_equal(varname_to_logical(logical_to_varname(original_expr2)), gsub(" ", "", original_expr2))
    expect_equal(varname_to_logical(logical_to_varname(original_expr3)), gsub(" ", "", original_expr3))
    expect_equal(varname_to_logical(logical_to_varname(original_expr4)), gsub(" ", "", original_expr4))
})


test_that("emr_vtrack.from_name works", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "dt2.latest", keepref = FALSE, time.shift = c(-10, 20))
    name <- emr_vtrack.name(src = "track2", func = "dt2.latest", params = NULL, keepref = FALSE, time.shift = c(-10, 20))
    emr_vtrack.create_from_name(name)
    expect_equal(.naryn$EMR_VTRACKS[[name]], .naryn$EMR_VTRACKS$v1)
})

test_that("emr_vtrack.from_name works with keepref=TRUE", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", keepref = TRUE)
    name <- emr_vtrack.name(src = "track2", params = NULL, keepref = TRUE)
    emr_vtrack.create_from_name(name)
    expect_equal(.naryn$EMR_VTRACKS[[name]], .naryn$EMR_VTRACKS$v1)
})

test_that("emr_vtrack.from_name works with params", {
    emr_vtrack.clear()
    name <- emr_vtrack.name(src = "ph1", func = "dt2.latest", params = c(2:5), keepref = FALSE)
    emr_vtrack.create_from_name(name)
    emr_vtrack.create("v1", "ph1", func = "dt2.latest", params = c(2:5), keepref = FALSE)
    expect_equal(.naryn$EMR_VTRACKS[[name]], .naryn$EMR_VTRACKS$v1)
})

test_that("emr_vtrack.from_name works without time shift", {
    emr_vtrack.clear()
    emr_vtrack.create("v1", "track2", func = "dt2.latest", keepref = FALSE)
    name <- emr_vtrack.name(src = "track2", func = "dt2.latest", params = NULL, keepref = FALSE)
    emr_vtrack.create_from_name(name)
    expect_equal(.naryn$EMR_VTRACKS[[name]], .naryn$EMR_VTRACKS$v1)
})

test_that("emr_vtrack.from_name works with filter", {
    emr_vtrack.clear()
    emr_filter.create("filter1", "track2", keepref = TRUE)
    emr_vtrack.create("v1", "track2", keepref = TRUE, filter = "filter1")
    name <- emr_vtrack.name(src = "track2", params = NULL, keepref = TRUE, filter = "filter1")
    emr_vtrack.create_from_name(name)
    expect_equal(.naryn$EMR_VTRACKS[[name]], .naryn$EMR_VTRACKS$v1)
})

test_that("emr_vtrack.from_name works with filter that has a dot in its name", {
    emr_vtrack.clear()
    emr_filter.create("filter1.savta", "track2", keepref = TRUE)
    emr_vtrack.create("v1", "track2", keepref = TRUE, filter = "filter1.savta")
    name <- emr_vtrack.name(src = "track2", params = NULL, keepref = TRUE, filter = "filter1.savta")
    emr_vtrack.create_from_name(name)
    expect_equal(.naryn$EMR_VTRACKS[[name]], .naryn$EMR_VTRACKS$v1)
})

test_that("emr_vtrack.from_name works with a 'not' filter", {
    emr_vtrack.clear()
    emr_filter.create("filter1", "track2", keepref = TRUE)
    emr_vtrack.create("v1", "track2", keepref = TRUE, filter = "!filter1")
    name <- emr_vtrack.name(src = "track2", params = NULL, keepref = TRUE, filter = "!filter1")
    emr_vtrack.create_from_name(name)
    expect_equal(.naryn$EMR_VTRACKS[[name]], .naryn$EMR_VTRACKS$v1)
})

test_that("emr_vtrack.from_name works with a complex filter", {
    emr_vtrack.clear()
    emr_filter.create("filter1", "track2", keepref = TRUE)
    emr_filter.create("filter2", "track1", keepref = TRUE)
    emr_vtrack.create("v1", "track2", keepref = TRUE, filter = "!filter1 & filter2")
    name <- emr_vtrack.name(src = "track2", params = NULL, keepref = TRUE, filter = "!filter1 & filter2")
    emr_vtrack.create_from_name(name)
    expect_equal(.naryn$EMR_VTRACKS[[name]], .naryn$EMR_VTRACKS$v1)
})

test_that("emr_vtrack.from_name works with filter and time shift", {
    emr_vtrack.clear()
    emr_filter.create("filter1", "track2", keepref = FALSE)
    emr_vtrack.create("v1", "track2", func = "dt2.latest", keepref = FALSE, filter = "filter1", time.shift = c(-10, 20))
    name <- emr_vtrack.name(src = "track2", func = "dt2.latest", params = NULL, keepref = FALSE, filter = "filter1", time.shift = c(-10, 20))
    emr_vtrack.create_from_name(name)
    expect_equal(.naryn$EMR_VTRACKS[[name]], .naryn$EMR_VTRACKS$v1)
})


test_that("emr_vtrack.name generates the correct name for a virtual track", {
    # Test with all parameters provided
    name <- emr_vtrack.name(src = "test_src", func = "test_func", params = c(1, 2, 3), keepref = TRUE, time.shift = 1.5, id.map = NULL, filter = "filter1")
    expect_equal(name, "vt_test_src.func_test_func.params_1_2_3.krT.ts_1.5.filter_filter1")

    # Test with missing parameters
    name <- emr_vtrack.name(src = "test_src", func = "test_func", params = NULL, keepref = FALSE, time.shift = NULL, id.map = NULL, filter = NULL)
    expect_equal(name, "vt_test_src.func_test_func.params_.krF")

    # Test with NULL func
    name <- emr_vtrack.name(src = "test_src", func = NULL, params = c(1, 2), keepref = FALSE, time.shift = NULL, id.map = NULL, filter = NULL)
    expect_equal(name, "vt_test_src.func_params_1_2.krF")

    # Test with NULL params
    name <- emr_vtrack.name(src = "test_src", func = "test_func", params = NULL, keepref = FALSE, time.shift = NULL, id.map = NULL, filter = NULL)
    expect_equal(name, "vt_test_src.func_test_func.params_.krF")

    # Test with NULL time.shift
    name <- emr_vtrack.name(src = "test_src", func = "test_func", params = c(1, 2), keepref = FALSE, time.shift = NULL, id.map = NULL, filter = "filter1")
    expect_equal(name, "vt_test_src.func_test_func.params_1_2.krF.filter_filter1")

    # Test with NULL filter
    name <- emr_vtrack.name(src = "test_src", func = "test_func", params = c(1, 2), keepref = FALSE, time.shift = 2.5, id.map = NULL, filter = NULL)
    expect_equal(name, "vt_test_src.func_test_func.params_1_2.krF.ts_2.5")

    expect_error(emr_vtrack.name(src = data.frame(), func = "test_func", params = c(1, 2), keepref = FALSE, time.shift = NULL, id.map = NULL, filter = NULL), "Cannot generate automatic virtual track name when source is not a character vector")

    # Test with non-character src
    expect_error(emr_vtrack.name(src = 1:5, func = "test_func", params = c(1, 2), keepref = FALSE, time.shift = NULL, id.map = NULL, filter = NULL), "Cannot generate automatic virtual track name when source is not a character vector")

    # Test with non-NULL id.map
    expect_error(emr_vtrack.name(src = "test_src", func = "test_func", params = c(1, 2), keepref = FALSE, time.shift = NULL, id.map = list(id1 = c(1, 2)), filter = NULL), "Cannot generate automatic virtual track name when id.map is not NULL")
})
