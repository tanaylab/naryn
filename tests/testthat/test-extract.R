
test_that("emr_extract works", {
    r1 <- emr_extract("track1", stime = 10, etime = 10000, keepref = TRUE)
    expect_regression(r1, "extract.1")
    r2 <- emr_extract("track2", stime = 10, etime = 10000, keepref = TRUE)
    expect_regression(r2, "extract.2")
    expect_regression(emr_annotate(r1, r2), "extract.3")
})

test_that("adding a column to emr_annotate", {
    r1 <- emr_extract("track1", stime = 10, etime = 100, keepref = TRUE)
    r2 <- emr_extract("track2", stime = 10, etime = 100, keepref = TRUE)
    r2$blabla <- paste("bla", 1:nrow(r2))
    expect_regression(emr_annotate(r1, r2), "extract.4")
})

test_that("adding a factor column to emr_annotate", {
    r1 <- emr_extract("track1", stime = 10, etime = 100, keepref = TRUE)
    r2 <- emr_extract("track2", stime = 10, etime = 100, keepref = TRUE)
    r2$blabla <- as.factor(paste("bla", 1:nrow(r2)))
    expect_regression(emr_annotate(r1, r2), "extract.5")
    expect_regression(emr_annotate(r2, r1), "extract.6")
})

test_that("emr_annotate with keepref=TRUE and keepref=FALSE", {
    r1 <- emr_extract("track1", stime = 10, etime = 10000, keepref = FALSE)
    r2 <- emr_extract("track2", stime = 10, etime = 10000, keepref = TRUE)
    expect_regression(emr_annotate(r1, r2), "extract.7")
    expect_regression(emr_annotate(r2, r1), "extract.8")
})

test_that("emr_annotate with keepref=TRUE and keepref=FALSE #2", {
    r1 <- emr_extract("track1", stime = 10, etime = 10000, keepref = TRUE)
    r2 <- emr_extract("track2", stime = 10, etime = 10000, keepref = FALSE)
    expect_regression(emr_annotate(r1, r2), "extract.9")
    expect_regression(emr_annotate(r2, r1), "extract.10")
})

test_that("emr_annotate with keepref=FALSE and keepref=FALSE #2", {
    r1 <- emr_extract("track1", stime = 10, etime = 10000, keepref = FALSE)
    r2 <- emr_extract("track2", stime = 10, etime = 10000, keepref = FALSE)
    expect_regression(emr_annotate(r1, r2), "extract.11")
    expect_regression(emr_annotate(r2, r1), "extract.12")
})

test_that("emr_annotate with sampled data fails", {
    set.seed(17)
    r1 <- emr_extract("track1", stime = 10, etime = 10000, keepref = TRUE)
    r1 <- r1[sample(1:nrow(r1)), ]
    r2 <- emr_extract("track2", stime = 10, etime = 10000, keepref = TRUE)
    expect_error(emr_annotate(r1, r2))
})

test_that("emr_annotate with sampled data fails #2", {
    set.seed(17)
    r1 <- emr_extract("track1", stime = 10, etime = 10000, keepref = TRUE)
    r2 <- emr_extract("track2", stime = 10, etime = 10000, keepref = TRUE)
    r2 <- r2[sample(1:nrow(r2)), ]
    expect_error(emr_annotate(r1, r2))
})

test_that("emr_annotate #1", {
    r1 <- emr_extract("track1", stime = 10, etime = 10000, keepref = TRUE)
    r1$ref <- NULL
    r1 <- r1[!duplicated(r1[, 1:2]), ]
    r2 <- emr_extract("track2", stime = 10, etime = 10000, keepref = TRUE)
    expect_regression(emr_annotate(r1, r2), "extract.13")
})

test_that("emr_annotate without ref", {
    r1 <- emr_extract("track1", stime = 10, etime = 10000, keepref = TRUE)
    r1$ref <- NULL
    r1 <- r1[!duplicated(r1[, 1:2]), ]
    r2 <- emr_extract("track2", stime = 10, etime = 10000, keepref = TRUE)
    expect_regression(emr_annotate(r2, r1), "extract.14")
})

test_that("emr_extract fails when track doesn't exist", {
    expect_error(emr_extract("bla"))
})

test_that("emr_extract #1", {
    expect_error(emr_extract("track2", stime = 1000, etime = 10))
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = FALSE), "extract.15")
    expect_error(emr_extract(c("track1", "track2"), stime = 10, etime = 1000))
    expect_regression(emr_extract(c("track1", "track2"), stime = 10, etime = 1000, iterator = 100), "extract.16")
    expect_regression(emr_extract(c("track1", "track2"), iterator = "track0", tidy = TRUE, keepref = TRUE), "extract.17")
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = FALSE), "extract.18")
    expect_error(emr_extract(c("track1", "track2"), stime = 10, etime = 1000))
    expect_regression(emr_extract(c("track1", "track2"), stime = 10, etime = 1000, iterator = 100), "extract.19")
    expect_regression(emr_extract(c("track1", "track2"), iterator = "track0", tidy = TRUE, keepref = TRUE), "extract.20")
})

test_that("emr_extract with names", {
    expect_regression(emr_extract(c("track1", "track2"), iterator = "track0", tidy = TRUE, keepref = TRUE, names = c("t1", "t2")), "extract.21")
    expect_regression(emr_extract(c("track1", "track2"), iterator = "track0", keepref = TRUE, names = c("t1", "t2")), "extract.22")
})

test_that("emr_screen works", {
    i <- emr_screen("track1>990", stime = 500, etime = 2000)
    expect_regression(i, "extract.23")
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = FALSE, iterator = i), "extract.24")
})

test_that("emr_screen works without ref", {
    i <- emr_screen("track1>990", stime = 500, etime = 2000)
    i$ref <- NULL
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = FALSE, iterator = i), "extract.25")
})

test_that("emr_screen works with keepref=TRUE", {
    i <- emr_screen("track1>990", stime = 500, etime = 2000)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = TRUE, iterator = i), "extract.26")
})

test_that("emr_screen works with keepref=FALSE", {
    i <- emr_screen("track1>990", stime = 5000, etime = 20000)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = FALSE, iterator = i), "extract.27")
})

test_that("emr_extract fails with dataframe iterator with non-unique ids", {
    i <- emr_screen("track1>990", stime = 500, etime = 2000)
    i <- data.frame(id = i$id)
    expect_error(emr_extract("track2", stime = 10, etime = 1000, keepref = TRUE, iterator = i))
})

test_that("emr_extract works with dataframe iterator", {
    i <- emr_screen("track1>990", stime = 500, etime = 2000)
    i <- data.frame(id = unique(i$id))
    expect_regression(emr_extract("track2", stime = 10, etime = 11, keepref = TRUE, iterator = i), "extract.28")
})

test_that("emr_extract works with dataframe iterator keepref=FALSE", {
    i <- emr_screen("track1>990", stime = 500, etime = 2000)
    i <- data.frame(id = unique(i$id))
    expect_regression(emr_extract("track2", stime = 10, etime = 20, keepref = FALSE, iterator = i), "extract.29")
})

test_that("emr_extract works with dataframe stime and etime iterator", {
    t <- data.frame(stime = c(5, 50), etime = c(10, 52))
    expect_regression(emr_extract("track2", stime = 10, etime = 20, keepref = FALSE, iterator = t), "extract.30")
    expect_regression(emr_extract("track2", stime = 10, etime = 20, keepref = TRUE, iterator = t), "extract.31")
})

test_that("emr_extract #2", {
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = TRUE), "extract.32")
    expect_regression(emr_extract("track2_sparse", stime = 10, etime = 1000, keepref = FALSE), "extract.33")
    expect_regression(emr_extract("track2_sparse", stime = 10, etime = 1000, keepref = TRUE), "extract.34")
})

test_that("emr_extract with track expression", {
    expect_regression(emr_extract("2 * track2", stime = 10, etime = 1000, keepref = TRUE, filter = "track1"), "extract.35")
})

test_that("emr_extract with filter", {
    expect_regression(emr_extract("track2_sparse", stime = 10, etime = 1000, keepref = TRUE, filter = "track1_sparse"), "extract.36")
})

test_that("emr_extract #3", {
    set.seed(17)
    n <- 10000
    df <- data.frame(id = sample(c(0:1005), n, replace = TRUE), stime = sample(c(0:11000), n, replace = TRUE), etime = 0)
    df$etime <- df$stime + sample(c(0:1000), n, replace = TRUE)
    expect_regression(emr_extract("1", iterator = df), "extract.37")
})

test_that("emr_extract with or filter", {
    set.seed(17)
    n <- 10000
    df <- data.frame(id = sample(c(0:1005), n, replace = TRUE), stime = sample(c(0:11000), n, replace = TRUE), etime = 0)
    df$etime <- df$stime + sample(c(0:1000), n, replace = TRUE)
    idx1 <- sample(1:n, n / 2)
    idx2 <- setdiff(1:n, idx1)
    df1 <- df[idx1, ]
    df2 <- df[idx2, ]
    expect_error(emr_extract("1", iterator = 1, filter = "df1|df2"))
    expect_regression(emr_extract("1", iterator = 1, stime = 100, etime = 5000, filter = "df1|df2"), "extract.38")
})

test_that("name inference works", {
    emr_vtrack.create("vtrack1", src = "track1", keepref = TRUE)
    emr_track.logical.create("ltrack1", src = "track1")
    a <- emr_extract("track1 * vtrack1 * ltrack1", keepref = TRUE, iterator = "track1", names = "track1")
    b <- emr_extract("track1^3", keepref = TRUE, iterator = "track1", names = "track1")
    expect_equal(a, b, ignore_attr = TRUE)
})
