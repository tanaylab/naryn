test_that("emr_track.ids works", {
    a <- emr_extract("track1", keepref = TRUE) %>%
        dplyr::distinct(id)
    b <- emr_track.ids("track1")
    expect_equal(a, b)

    a <- emr_extract("track1_sparse", keepref = TRUE) %>%
        dplyr::distinct(id)
    b <- emr_track.ids("track1_sparse")
    expect_equal(a, b)
})
