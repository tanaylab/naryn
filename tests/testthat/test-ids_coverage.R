
test_that("emr_ids_coverage works", {
    expect_equal(
        emr_ids_coverage(data.frame(id = 0:200), c("track7", "track6")),
        c(track7 = 201L, track6 = 63L)
    )
})

test_that("emr_ids_coverage works with filter", {
    expect_equal(
        emr_ids_coverage(data.frame(id = 0:200), c("track7", "track6"), filter = "track2"),
        c(track7 = 201L, track6 = 24L)
    )
})

test_that("emr_ids_coverage works with data frame", {
    r <- emr_extract("track4")
    expect_equal(
        emr_ids_coverage(r, c("track7", "track6")),
        c(track7 = 409L, track6 = 131L)
    )
    expect_equal(
        emr_ids_coverage(r, c("track7", "track6"), filter = "track2"),
        c(track7 = 409L, track6 = 41L)
    )
})

test_that("emr_ids_coverage with filter", {
    expect_equal(
        emr_ids_coverage("track5", c("track7", "track6"), filter = "track1"),
        c(track7 = 909L, track6 = 11L)
    )
})

test_that("emr_ids_vals_coverage works", {
    expect_regression(emr_ids_vals_coverage(data.frame(id = 0:200), c("track7", "track6")))
})

test_that("emr_ids_vals_coverage works with filter", {
    expect_regression(emr_ids_vals_coverage(data.frame(id = 0:200), c("track7", "track6"), filter = "track2"))
})

test_that("emr_ids_vals_coverage works with filter emr_extract df ", {
    r <- emr_extract("track4")
    expect_regression(emr_ids_vals_coverage(r, c("track7", "track6")))
})

test_that("emr_ids_vals_coverage works with emr_extract df and a filter", {
    r <- emr_extract("track4")
    expect_regression(emr_ids_vals_coverage(r, c("track7", "track6"), filter = "track2"))
})

test_that("emr_ids_vals_coverage works with track name and filter", {
    expect_regression(emr_ids_vals_coverage("track5", c("track7", "track6"), filter = "track1"))
})
