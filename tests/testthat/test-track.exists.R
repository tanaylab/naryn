
test_that("emr_track.exists work", {
    expect_true(emr_track.exists("track1"))
    expect_true(emr_track.exists("track1_sparse"))
    expect_false(emr_track.exists("blahblahblah"))
})
