load_test_db()


test_that("emr_track.exists work", {
    expect_true(emr_track.exists("track1"))
    expect_true(emr_track.exists("track1_sparse"))
    expect_false(emr_track.exists("blahblahblah"))

    expect_equal(emr_track.exists(emr_track.ls()), rep(TRUE, 20))
    expect_equal(emr_track.exists(c("saba", "savta", emr_track.ls())), c(FALSE, FALSE, rep(TRUE, 20)))
})
