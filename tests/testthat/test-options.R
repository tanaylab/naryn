
test_that("emr_multitasking works", {
    options(emr_multitasking = T)
    r1 <- emr_summary("track2", iterator = 4, stime = 20, etime = 9000, keepref = T, filter = "track0|track1|track3|track4|track5|track6|track7|track8")
    expect_regression(r1)
    options(emr_multitasking = F)
    r2 <- emr_summary("track2", iterator = 4, stime = 20, etime = 9000, keepref = T, filter = "track0|track1|track3|track4|track5|track6|track7|track8")
    expect_regression(r2)
    expect_equal(r1, r2)
})

test_that("emr_max.data.size works", {
    options(emr_multitasking = T)
    options(emr_max.data.size = 200000000)
    r1 <- emr_extract("track2", sort = T, iterator = 4, stime = 20, etime = 9000, keepref = T, filter = "track0|track1|track3|track4|track5|track6|track7|track8")
    expect_regression(r1)
    options(emr_multitasking = F)
    options(emr_max.data.size = 10000000)
    expect_error(emr_extract("track2", sort = T, iterator = 4, stime = 20, etime = 9000, keepref = T, filter = "track0|track1|track3|track4|track5|track6|track7|track8"))
})
