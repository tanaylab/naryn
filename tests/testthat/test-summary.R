test_that("emr_summary works", {
    expect_equal(
        emr_summary("track1"),
        c(
            `Total values` = 487512, `NaN values` = 0, Min = 0, Max = 999,
            Sum = 243128239.75, Mean = 498.712318363446, `Std dev` = 286.952161649683
        )
    )
})

test_that("emr_summary works", {
    expect_equal(
        emr_summary("track4"),
        c(
            `Total values` = 500, `NaN values` = 0, Min = 1, Max = 997,
            Sum = 255122, Mean = 510.244, `Std dev` = 282.149813926924
        )
    )
})
