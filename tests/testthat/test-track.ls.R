test_that("emr_track.ls works", {
    expect_equal(
        emr_track.ls(),
        c("patients.dob", "stam1", "track0", "track0_sparse", "track1",
"track1_sparse", "track2", "track2_sparse", "track3", "track4",
"track4_sparse", "track5", "track5_sparse", "track6", "track7",
"track7_sparse", "track8", "track8_sparse")
    )
})

test_that("emr_track.ls works with regex", {
    expect_equal(
        emr_track.ls("*sparse"),
        c("track0_sparse", "track1_sparse", "track2_sparse", "track4_sparse",
"track5_sparse", "track7_sparse", "track8_sparse")
    )    
})

test_that("emr_track.global.ls works", {
    expect_equal(
        emr_track.global.ls(),
        c("patients.dob", "stam1", "track0", "track0_sparse", "track1",
"track1_sparse", "track2", "track2_sparse", "track3", "track4",
"track4_sparse", "track5", "track5_sparse", "track6", "track7",
"track7_sparse", "track8", "track8_sparse")
    )
})

test_that("emr_track.user.ls works", {
    expect_equal(
        emr_track.user.ls(),
        character(0)
    )
})
