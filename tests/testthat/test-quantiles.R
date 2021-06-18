test_that("emr_quantiles works", {
    expect_equal(
        emr_quantiles("track1", c(0.1, 0.2, 0.5, 0.9)),
        c(`0.1` = 101, `0.2` = 202, `0.5` = 498, `0.9` = 897)
    )
})

test_that("emr_quantiles fails without iterator", {    
    expect_error(emr_quantiles("track1 + track2", c(0.1, 0.2, 0.5, 0.9)))
})

test_that("emr_quantiles works", {
    expect_equal(
        emr_quantiles("track1 + track2", c(0.1, 0.2, 0.5, 0.9)),
        c(`0.1` = 458, `0.2` = 637, `0.5` = 998, `0.9` = 1540)
    )
})

