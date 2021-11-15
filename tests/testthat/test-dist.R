
test_that("emr_dist works", {
    dst <- emr_dist("track2", c(100, 300, 500, 900, 2000, 3000))
    expect_equal(
        dst,
        structure(c(
            `(100,300]` = 430934, `(300,500]` = 474818, `(500,900]` = 906870,
            `(900,2000]` = 197080, `(2000,3000]` = 0
        ), .Dim = 5L, .Dimnames = list(
            c("(100,300]", "(300,500]", "(500,900]", "(900,2000]", "(2000,3000]")
        ), breaks = list(c(100, 300, 500, 900, 2000, 3000)))
    )
})

test_that("emr_dist works keepref=T", {
    dst <- emr_dist("track2", c(100, 300, 500, 900, 2000, 3000), keepref = T)
    expect_equal(
        dst,
        structure(c(
            `(100,300]` = 500340, `(300,500]` = 499412, `(500,900]` = 1000417,
            `(900,2000]` = 247373, `(2000,3000]` = 0
        ), .Dim = 5L, .Dimnames = list(
            c("(100,300]", "(300,500]", "(500,900]", "(900,2000]", "(2000,3000]")
        ), breaks = list(c(100, 300, 500, 900, 2000, 3000)))
    )
})

test_that("emr_dist works 2d", {
    expect_error(emr_dist("track1", c(100, 300, 500, 900, 2000, 3000), "track2", c(50, 60, 80, 90)))
    dst <- emr_dist("track1", c(100, 300, 500, 900, 2000, 3000), "track2", c(50, 60, 80, 90), iterator = "track1")
    expect_equal(
        dst,
        structure(c(
            186, 198, 400, 80, 0, 380, 416, 771, 182, 0, 212,
            195, 363, 107, 0
        ), .Dim = c(5L, 3L), .Dimnames = list(c(
            "(100,300]",
            "(300,500]", "(500,900]", "(900,2000]", "(2000,3000]"
        ), c(
            "(50,60]",
            "(60,80]", "(80,90]"
        )), breaks = list(c(
            100, 300, 500, 900, 2000,
            3000
        ), c(50, 60, 80, 90)))
    )
})

test_that("emr_dist with NULL breaks", {
    expect_error(emr_dist("track1", NULL, "track2", c(50, 60, 80, 90), iterator = "track1"))
    dst <- emr_dist("track6", NULL, "track2", c(50, 60, 80, 90), iterator = "track6")
    expect_equal(
        dst,
        structure(c(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 2, 0, 0,
            0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0
        ), .Dim = c(10L, 3L), .Dimnames = list(
            c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"), c(
                "(50,60]",
                "(60,80]", "(80,90]"
            )
        ), breaks = list(0:9, c(
            50, 60, 80,
            90
        )))
    )
})

test_that("emr_dist with vtrack dt1.earliest", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track7", func = "dt1.earliest", time.shift = c(-10, 10))
    expect_error(emr_dist("v1", NULL, "track2", c(50, 60, 80, 90), iterator = "track1"))
})

test_that("emr_dist with vtrack frequent", {
    EMR_VTRACKS <<- list()
    emr_vtrack.create("v1", "track7", func = "frequent", time.shift = c(-10, 10))
    dst <- emr_dist("v1", NULL, "track2", c(50, 60, 80, 90), iterator = "track1")
    expect_equal(
        dst,
        structure(c(
            17, 17, 25, 19, 18, 20, 10, 22, 10, 15, 37, 30, 41,
            30, 35, 30, 40, 33, 28, 37, 18, 16, 14, 18, 17, 11, 11, 14, 20,
            17
        ), .Dim = c(10L, 3L), .Dimnames = list(c(
            "0", "1", "2", "3",
            "4", "5", "6", "7", "8", "9"
        ), c("(50,60]", "(60,80]", "(80,90]")), breaks = list(0:9, c(50, 60, 80, 90)))
    )
})

test_that("emr_dist with vtrack sample", {
    EMR_VTRACKS <<- list()
    set.seed(17)
    emr_vtrack.create("v1", "track7", func = "sample", time.shift = c(-10, 10))
    dst <- emr_dist("v1", NULL, "track2", c(50, 60, 80, 90), iterator = "track1")
    expect_equal(
        dst,
        structure(c(
            20, 19, 26, 23, 19, 21, 14, 22, 11, 18, 41, 32, 42,
            33, 38, 32, 42, 35, 32, 41, 18, 18, 16, 20, 19, 11, 14, 15, 21,
            19
        ), .Dim = c(10L, 3L), .Dimnames = list(c(
            "0", "1", "2", "3",
            "4", "5", "6", "7", "8", "9"
        ), c("(50,60]", "(60,80]", "(80,90]")), breaks = list(0:9, c(50, 60, 80, 90)))
    )
})

test_that("emr_dist with vtrack sample.time", {
    EMR_VTRACKS <<- list()
    set.seed(17)
    emr_vtrack.create("v1", "track7", func = "sample.time", time.shift = c(-10, 10))
    dst <- emr_dist("v1", NULL, "track2", c(50, 60, 80, 90), iterator = "track1")
    expect_equal(
        dst,
        structure(c(
            0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
        ), .Dim = c(10L, 3L), .Dimnames = list(
            c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"), c(
                "(50,60]",
                "(60,80]", "(80,90]"
            )
        ), breaks = list(0:9, c(
            50, 60, 80,
            90
        )))
    )
})

test_that("emr_dist works when dataframe = TRUE", {
    dst <- emr_dist("track2", c(100, 300, 500, 900, 2000, 3000), dataframe = TRUE)
    dst_non_df <- emr_dist("track2", c(100, 300, 500, 900, 2000, 3000), dataframe = FALSE)
    expect_true(all(dst$n == dst_non_df))
    expect_true(all(dst$track2 == names(dst_non_df)))
    expect_true(is.factor(dst$track2))
    expect_true(all(levels(dst$track2) == names(dst_non_df)))

    expect_equal(
        dst,
        structure(list(
            track2 = structure(1:5, .Label = c(
                "(100,300]",
                "(300,500]", "(500,900]", "(900,2000]", "(2000,3000]"
            ), class = "factor"),
            n = c(430934, 474818, 906870, 197080, 0)
        ), class = "data.frame", row.names = c(
            NA,
            -5L
        ))
    )
})

test_that("emr_dist 2d works when dataframe = TRUE", {
    dst <- emr_dist("track1", c(100, 300, 500, 900, 2000, 3000), "track2", c(50, 60, 80, 90), iterator = "track1", dataframe = TRUE)
    expect_equal(colnames(dst), c("track1", "track2", "n"))
    dst_non_df <- emr_dist("track1", c(100, 300, 500, 900, 2000, 3000), "track2", c(50, 60, 80, 90), iterator = "track1", dataframe = FALSE)
    expect_true(all(dst$n == dst_non_df))
    expect_true(is.factor(dst$track2))
    expect_true(is.factor(dst$track1))
    expect_true(all(levels(dst$track2) == colnames(dst_non_df)))
    expect_true(all(levels(dst$track1) == rownames(dst_non_df)))

    expect_equal(
        dst,
        structure(list(
            track1 = structure(c(
                1L, 2L, 3L, 4L, 5L, 1L, 2L,
                3L, 4L, 5L, 1L, 2L, 3L, 4L, 5L
            ), .Label = c(
                "(100,300]", "(300,500]",
                "(500,900]", "(900,2000]", "(2000,3000]"
            ), class = "factor"),
            track2 = structure(c(
                1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
                2L, 3L, 3L, 3L, 3L, 3L
            ), .Label = c(
                "(50,60]", "(60,80]",
                "(80,90]"
            ), class = "factor"), n = c(
                186, 198, 400, 80, 0,
                380, 416, 771, 182, 0, 212, 195, 363, 107, 0
            )
        ), class = "data.frame", row.names = c(
            NA,
            -15L
        ))
    )
})

test_that("emr_dist dataframe = TRUE with names", {
    dst <- emr_dist("track1", c(100, 300, 500, 900, 2000, 3000), "track2", c(50, 60, 80, 90), iterator = "track1", dataframe = TRUE, names = c("mytrack1", "mytrack2"))
    expect_equal(colnames(dst), c("mytrack1", "mytrack2", "n"))

    dst1 <- emr_dist("track1", c(100, 300, 500, 900, 2000, 3000), "track2", c(50, 60, 80, 90), iterator = "track1", dataframe = TRUE)
    colnames(dst1) <- c("mytrack1", "mytrack2", "n")
    expect_equal(dst, dst1)
})


test_that("emr_dist works with fractions", {
    df <- emr_extract("track1", keepref = TRUE) %>%
        mutate(value = runif(n())) %>%
        select(id, time, ref, value)
    emr_track.import("dist_test", space = "global", categorical = FALSE, src = df)
    withr::defer(emr_track.rm("dist_test", TRUE))
    dist_res <- emr_dist("dist_test", seq(0, 1, length.out = 5), dataframe = TRUE, right = FALSE, keepref = TRUE)
    df_res <- df %>%
        mutate(dist_test = cut(value, seq(0, 1, length.out = 5), right = FALSE)) %>%
        count(dist_test)
    expect_equal(dist_res, df_res)

    dist_res <- emr_dist("dist_test", seq(0, 1, length.out = 5), dataframe = TRUE, right = TRUE, keepref = TRUE)
    df_res <- df %>%
        mutate(dist_test = cut(value, seq(0, 1, length.out = 5), right = TRUE)) %>%
        count(dist_test)
    expect_equal(dist_res, df_res)
})
