
test_that("emr_extract works", {
    r1 <- emr_extract("track1", stime = 10, etime = 10000, keepref = T)
    expect_regression(r1)
    r2 <- emr_extract("track2", stime = 10, etime = 10000, keepref = T)
    expect_regression(r2)
    expect_regression(emr_annotate(r1, r2))
})

test_that("adding a column to emr_annotate", {
    r1 <- emr_extract("track1", stime = 10, etime = 100, keepref = T)
    r2 <- emr_extract("track2", stime = 10, etime = 100, keepref = T)
    r2$blabla <- paste("bla", 1:nrow(r2))
    expect_regression(emr_annotate(r1, r2))
})

test_that("adding a factor column to emr_annotate", {
    r1 <- emr_extract("track1", stime = 10, etime = 100, keepref = T)
    r2 <- emr_extract("track2", stime = 10, etime = 100, keepref = T)
    r2$blabla <- as.factor(paste("bla", 1:nrow(r2)))
    expect_regression(emr_annotate(r1, r2))
    expect_regression(emr_annotate(r2, r1))
})

test_that("emr_annotate with keepref=T and keepref=F", {
    r1 <- emr_extract("track1", stime = 10, etime = 10000, keepref = F)
    r2 <- emr_extract("track2", stime = 10, etime = 10000, keepref = T)
    expect_regression(emr_annotate(r1, r2))
    expect_regression(emr_annotate(r2, r1))
})

test_that("emr_annotate with keepref=T and keepref=F #2", {
    r1 <- emr_extract("track1", stime = 10, etime = 10000, keepref = T)
    r2 <- emr_extract("track2", stime = 10, etime = 10000, keepref = F)
    expect_regression(emr_annotate(r1, r2))
    expect_regression(emr_annotate(r2, r1))
})

test_that("emr_annotate with keepref=F and keepref=F #2", {
    r1 <- emr_extract("track1", stime = 10, etime = 10000, keepref = F)
    r2 <- emr_extract("track2", stime = 10, etime = 10000, keepref = F)
    expect_regression(emr_annotate(r1, r2))
    expect_regression(emr_annotate(r2, r1))
})

test_that("emr_annotate with sampled data fails", {
    set.seed(17)
    r1 <- emr_extract("track1", stime = 10, etime = 10000, keepref = T)
    r1 <- r1[sample(1:nrow(r1)), ]
    r2 <- emr_extract("track2", stime = 10, etime = 10000, keepref = T)
    expect_error(emr_annotate(r1, r2))
})

test_that("emr_annotate with sampled data fails #2", {
    set.seed(17)
    r1 <- emr_extract("track1", stime = 10, etime = 10000, keepref = T)
    r2 <- emr_extract("track2", stime = 10, etime = 10000, keepref = T)
    r2 <- r2[sample(1:nrow(r2)), ]
    expect_error(emr_annotate(r1, r2))
})

test_that("emr_annotate #1", {
    r1 <- emr_extract("track1", stime = 10, etime = 10000, keepref = T)
    r1$ref <- NULL
    r1 <- r1[!duplicated(r1[, 1:2]), ]
    r2 <- emr_extract("track2", stime = 10, etime = 10000, keepref = T)
    expect_regression(emr_annotate(r1, r2))
})

test_that("emr_annotate without ref", {
    r1 <- emr_extract("track1", stime = 10, etime = 10000, keepref = T)
    r1$ref <- NULL
    r1 <- r1[!duplicated(r1[, 1:2]), ]
    r2 <- emr_extract("track2", stime = 10, etime = 10000, keepref = T)
    expect_regression(emr_annotate(r2, r1))
})

test_that("emr_extract fails when track doesn't exist", {
    expect_error(emr_extract("bla"))
})

test_that("emr_extract #1", {
    expect_error(emr_extract("track2", stime = 1000, etime = 10))
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = F))
    expect_error(emr_extract(c("track1", "track2"), stime = 10, etime = 1000))
    expect_regression(emr_extract(c("track1", "track2"), stime = 10, etime = 1000, iterator = 100))
    expect_regression(emr_extract(c("track1", "track2"), iterator = "track0", tidy = T, keepref = T))
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = F))
    expect_error(emr_extract(c("track1", "track2"), stime = 10, etime = 1000))
    expect_regression(emr_extract(c("track1", "track2"), stime = 10, etime = 1000, iterator = 100))
    expect_regression(emr_extract(c("track1", "track2"), iterator = "track0", tidy = T, keepref = T))
})

test_that("emr_extract with names", {
    expect_regression(emr_extract(c("track1", "track2"), iterator = "track0", tidy = T, keepref = T, names = c("t1", "t2")))
    expect_regression(emr_extract(c("track1", "track2"), iterator = "track0", keepref = T, names = c("t1", "t2")))
})

test_that("emr_screen works", {
    i <- emr_screen("track1>990", stime = 500, etime = 2000)
    expect_regression(i)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = F, iterator = i))
})

test_that("emr_screen works without ref", {
    i <- emr_screen("track1>990", stime = 500, etime = 2000)
    i$ref <- NULL
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = F, iterator = i))
})

test_that("emr_screen works with keepref=T", {
    i <- emr_screen("track1>990", stime = 500, etime = 2000)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = T, iterator = i))
})

test_that("emr_screen works with keepref=F", {
    i <- emr_screen("track1>990", stime = 5000, etime = 20000)
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = F, iterator = i))
})

test_that("emr_extract fails with dataframe iterator with non-unique ids", {
    i <- emr_screen("track1>990", stime = 500, etime = 2000)
    i <- data.frame(id = i$id)
    expect_error(emr_extract("track2", stime = 10, etime = 1000, keepref = T, iterator = i))
})

test_that("emr_extract works with dataframe iterator", {
    i <- emr_screen("track1>990", stime = 500, etime = 2000)
    i <- data.frame(id = unique(i$id))
    expect_regression(emr_extract("track2", stime = 10, etime = 11, keepref = T, iterator = i))
})

test_that("emr_extract works with dataframe iterator keepref=F", {
    i <- emr_screen("track1>990", stime = 500, etime = 2000)
    i <- data.frame(id = unique(i$id))
    expect_regression(emr_extract("track2", stime = 10, etime = 20, keepref = F, iterator = i))
})

test_that("emr_extract works with dataframe stime and etime iterator", {
    t <- data.frame(stime = c(5, 50), etime = c(10, 52))
    expect_regression(emr_extract("track2", stime = 10, etime = 20, keepref = F, iterator = t))
    expect_regression(emr_extract("track2", stime = 10, etime = 20, keepref = T, iterator = t))
})

test_that("emr_extract #2", {
    expect_regression(emr_extract("track2", stime = 10, etime = 1000, keepref = T))
    expect_regression(emr_extract("track2_sparse", stime = 10, etime = 1000, keepref = F))
    expect_regression(emr_extract("track2_sparse", stime = 10, etime = 1000, keepref = T))
})

test_that("emr_extract with track expression", {
    expect_regression(emr_extract("2 * track2", stime = 10, etime = 1000, keepref = T, filter = "track1"))
})

test_that("emr_extract with filter", {
    expect_regression(emr_extract("track2_sparse", stime = 10, etime = 1000, keepref = T, filter = "track1_sparse"))
})

test_that("emr_extract #3", {
    set.seed(17)
    n <- 10000
    df <- data.frame(id = sample(c(0:1005), n, replace = T), stime = sample(c(0:11000), n, replace = T), etime = 0)
    df$etime <- df$stime + sample(c(0:1000), n, replace = T)
    expect_regression(emr_extract("1", iterator = df))
})

test_that("emr_extract with or filter", {
    n <- 10000
    df <- data.frame(id = sample(c(0:1005), n, replace = T), stime = sample(c(0:11000), n, replace = T), etime = 0)
    df$etime <- df$stime + sample(c(0:1000), n, replace = T)
    idx1 <- sample(1:n, n / 2)
    idx2 <- setdiff(1:n, idx1)
    df1 <- df[idx1, ]
    df2 <- df[idx2, ]
    expect_error(emr_extract("1", iterator = 1, filter = "df1|df2"))
    expect_regression(emr_extract("1", iterator = 1, stime = 100, etime = 5000, filter = "df1|df2"))
})
