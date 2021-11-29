
test_that("emr_screen fails without logical expr", {
    expect_error(emr_screen("track1"))
})

test_that("emr_screen works when expr doesn't return data", {
    expect_equal(
        emr_screen("track1 < 0"),
        structure(list(id = integer(0), time = integer(0), ref = integer(0)), class = "data.frame", row.names = integer(0))
    )
})

test_that("emr_screen works ", {
    expect_regression(emr_screen("track1 > 990"), "screen.1")
})
