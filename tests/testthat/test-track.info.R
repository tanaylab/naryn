
test_that("error when track doesn't exist", {
    expect_error(emr_track.info("blabla"))
})

test_that("emr_track.info works dense track", {
    track_info <- emr_track.info("track4")
    track_info$path <- NULL
    expect_equal(
        track_info,
        list(
            type = "dense", data.type = "float", categorical = FALSE,
            num.vals = 500L, num.unique.vals = 401L, min.val = 1, max.val = 997,
            min.id = 2L, max.id = 998L, min.time = 6L, max.time = 9996L
        )
    )
})

test_that("emr_track.info works sparse track", {
    track_info <- emr_track.info("track1_sparse")
    track_info$path <- NULL
    expect_equal(
        track_info,
        list(
            type = "sparse", data.type = "float", categorical = FALSE,
            num.vals = 500001L, num.unique.vals = 1000L, min.val = 0,
            max.val = 999, min.id = 0L, max.id = 2510L, min.time = 0L,
            max.time = 9999L
        )
    )
})
