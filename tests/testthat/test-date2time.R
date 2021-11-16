test_that("emr_date2time fails with invalid dates", {
    expect_error(emr_date2time(0, 0, 0, 0))
    expect_error(emr_date2time(1, 1, 1000, 0))
    expect_error(emr_date2time(1, 1, 1000, 0))
    expect_error(emr_date2time(1, 1, 300000, 0))
})

test_that("emr_date2time works", {
    expect_equal(emr_date2time(1, 3, 1867, 0), 0)
    expect_equal(emr_date2time(29, 2, 1868, 10), 8770)
    expect_equal(emr_date2time(28, 2, 1868, 10), 8746)
    expect_equal(emr_date2time(15, c(2, 5, 6), 1869, 10), c(17218L, 19354L, 20098L))
})

test_that("emr_date2time with hour and month vector", {
    expect_equal(emr_date2time(15, c(2, 5, 6), 1869, c(10, 20, 23)), c(17218L, 19364L, 20111L))
})

test_that("emr_date2time fails when length of hour vector is different than month", {
    expect_error(emr_date2time(15, c(2, 5, 6), 1869, c(10, 20)))
})

test_that("emr_time2dayofmonth works", {
    r <- emr_extract("track2_sparse", stime = 10, etime = 1000, keepref = F)
    expect_regression(emr_time2dayofmonth(r$time), "date2time.1")
})

test_that("emr_time2hour works", {
    r <- emr_extract("track2_sparse", stime = 10, etime = 1000, keepref = F)
    expect_regression(emr_time2hour(r$time), "date2time.2")
})

test_that("emr_time2month works", {
    r <- emr_extract("track2_sparse", stime = 10, etime = 1000, keepref = F)
    expect_regression(emr_time2month(r$time), "date2time.3")
})

test_that("emr_time2year works", {
    r <- emr_extract("track2_sparse", stime = 10, etime = 1000, keepref = F)
    expect_regression(emr_time2year(r$time), "date2time.4")
})

test_that("emr_time works", {
    expect_equal(emr_time(5), 5 * 365 * 24)
    expect_equal(emr_time(months = 4), 4 * 30 * 24)
    expect_equal(emr_time(1, 4, 2), c(365 + 4 * 30 + 2) * 24)
    expect_equal(year(), 365 * 24)
    expect_equal(years(5), 5 * 365 * 24)
    expect_equal(month(), 30 * 24)
    expect_equal(months(5), 5 * 30 * 24)
    expect_equal(day(), 24)
    expect_equal(days(9), 9 * 24)
    expect_equal(week(), 7 * 24)
    expect_equal(weeks(2), 14 * 24)
    expect_equal(hour(), 1)
    expect_equal(hours(5), 5)

    expect_equal(5 * year(), years(5))
    expect_equal(5 * month(), months(5))
    expect_equal(5 * day(), days(5))
    expect_equal(5 * week(), weeks(5))

    expect_equal(emr_time(5), years(5))
    expect_equal(emr_time(0, 4, 0), months(4))
    expect_equal(emr_time(0, 0, 4), days(4))
})
