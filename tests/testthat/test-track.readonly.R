
test_that("user tracks are not readonly", {
    expect_false(emr_track.readonly("track0"))
})

test_that("create readonly tracks", {
    if (emr_track.exists("test_track1_ro")) {
        emr_track.readonly("test_track1_ro", FALSE)
        emr_track.rm("test_track1_ro", TRUE)
    }

    emr_track.create("test_track1_ro", "user", FALSE, "track0+2", keepref = FALSE)
    emr_track.readonly("test_track1_ro", TRUE)
    expect_true(emr_track.readonly("test_track1_ro"))

    expect_error(emr_track.rm("test_track1_ro", TRUE), "Cannot remove track test_track1_ro: it is read-only.")

    emr_track.readonly("test_track1_ro", FALSE)
    emr_track.rm("test_track1_ro", TRUE)
})
