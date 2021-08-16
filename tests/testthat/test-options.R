
test_that("emr_multitasking works", {
    withr::local_options(emr_multitasking = TRUE)
    r1 <- emr_summary("track2", iterator = 4, stime = 20, etime = 9000, keepref = T, filter = "track0|track1|track3|track4|track5|track6|track7|track8")
    expect_regression(r1, "options.1")
    withr::local_options(emr_multitasking = FALSE)
    r2 <- emr_summary("track2", iterator = 4, stime = 20, etime = 9000, keepref = T, filter = "track0|track1|track3|track4|track5|track6|track7|track8")
    expect_regression(r2, "options.2")
    expect_equal(r1, r2)
})

test_that("emr_max.data.size works", {
    withr::local_options(emr_multitasking = TRUE)
    withr::local_options(emr_max.data.size = 200000000)
    r1 <- emr_extract("track2", sort = T, iterator = 4, stime = 20, etime = 9000, keepref = T, filter = "track0|track1|track3|track4|track5|track6|track7|track8")
    expect_regression(r1, "options.3")
    withr::local_options(emr_multitasking = FALSE)
    withr::local_options(emr_max.data.size = 10000000)
    expect_error(emr_extract("track2", sort = T, iterator = 4, stime = 20, etime = 9000, keepref = T, filter = "track0|track1|track3|track4|track5|track6|track7|track8"))
})
