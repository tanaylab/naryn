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

test_that("emr_time2date works", {
    t1 <- emr_date2time(30, 1, 1938, 6)
    t2 <- emr_date2time(2, 9, 2016, 7)
    expect_equal(
        emr_time2date(c(t1, t2)),
        data.frame(
            year = c(1938, 2016),
            month = c(1, 9),
            day = c(30, 2),
            hour = c(6, 7)
        )
    )
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

test_that("emr_yearly_iterator works", {
    iter <- emr_yearly_iterator(emr_date2time(1, 1, 2002), emr_date2time(30, 9, 2019), 50)
    stime <- emr_time2date(iter$stime)
    expect_equal(stime$year, 2002:2019)
    expect_true(all(stime$month == 1))
    expect_true(all(stime$day == 1))
    expect_true(all(stime$hour == 0))

    etime <- emr_time2date(iter$etime)
    expect_equal(etime$year, 2002:2019)
    expect_true(all(etime$month == 1))
    expect_true(all(etime$day == 1))
    expect_true(all(etime$hour == 0))

    iter <- emr_yearly_iterator(emr_date2time(1, 1, 2002), NULL, 3)
    stime <- emr_time2date(iter$stime)
    expect_equal(stime$year, 2002:2005)
    expect_true(all(stime$month == 1))
    expect_true(all(stime$day == 1))
    expect_true(all(stime$hour == 0))

    etime <- emr_time2date(iter$etime)
    expect_equal(etime$year, 2002:2005)
    expect_true(all(etime$month == 1))
    expect_true(all(etime$day == 1))
    expect_true(all(etime$hour == 0))

    iter <- emr_yearly_iterator(emr_date2time(1, 1, 2002), emr_date2time(30, 9, 2019), NULL)
    stime <- emr_time2date(iter$stime)
    expect_equal(stime$year, 2002:2019)
    expect_true(all(stime$month == 1))
    expect_true(all(stime$day == 1))
    expect_true(all(stime$hour == 0))

    etime <- emr_time2date(iter$etime)
    expect_equal(etime$year, 2002:2019)
    expect_true(all(etime$month == 1))
    expect_true(all(etime$day == 1))
    expect_true(all(etime$hour == 0))
})

test_that("emr_yearly_iterator works as iterator", {
    iter <- emr_yearly_iterator(emr_date2time(1, 1, 2002), emr_date2time(30, 9, 2019))
    a <- emr_extract("track1", iterator = iter)
    expect_equal(
        emr_time2date(a$time) %>% dplyr::distinct(year, month, day, hour),
        emr_time2date(iter$stime)
    )
})

test_that("emr_monthly_iterator works", {
    iter <- emr_monthly_iterator(emr_date2time(1, 1, 2002), emr_date2time(30, 9, 2019), 11)
    stime <- emr_time2date(iter$stime)
    expect_true(all(stime$year == 2002))
    expect_true(all(stime$month == 1:12))
    expect_true(all(stime$day == 1))
    expect_true(all(stime$hour == 0))

    etime <- emr_time2date(iter$etime)
    expect_true(all(etime$year == 2002))
    expect_true(all(etime$month == 1:12))
    expect_true(all(etime$day == 1))
    expect_true(all(etime$hour == 0))

    iter <- emr_monthly_iterator(emr_date2time(1, 1, 2002), NULL, 3)
    stime <- emr_time2date(iter$stime)
    expect_true(all(stime$year == 2002))
    expect_true(all(stime$month == 1:4))
    expect_true(all(stime$day == 1))
    expect_true(all(stime$hour == 0))

    etime <- emr_time2date(iter$etime)
    expect_true(all(etime$year == 2002))
    expect_true(all(etime$month == 1:4))
    expect_true(all(etime$day == 1))
    expect_true(all(etime$hour == 0))

    iter <- emr_monthly_iterator(emr_date2time(1, 1, 2002), emr_date2time(1, 1, 2019), NULL)
    stime <- emr_time2date(iter$stime)
    expect_true(all(stime$year %in% 2002:2019))
    expect_true(all(etime$month %in% 1:12))
    expect_true(all(stime$day == 1))
    expect_true(all(stime$hour == 0))

    etime <- emr_time2date(iter$etime)
    expect_true(all(etime$year %in% 2002:2019))
    expect_true(all(etime$month %in% 1:12))
    expect_true(all(etime$day == 1))
    expect_true(all(etime$hour == 0))
})

test_that("emr_monthly_iterator works as iterator", {
    iter <- emr_monthly_iterator(emr_date2time(1, 1, 2002), emr_date2time(30, 9, 2019))
    a <- emr_extract("track1", iterator = iter)
    expect_equal(
        emr_time2date(a$time) %>% dplyr::distinct(year, month, day, hour),
        emr_time2date(iter$stime)
    )
})
