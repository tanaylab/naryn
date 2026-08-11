load_test_db()

# Rewriting a track in its own db used to be impossible: both emr_track.import and
# emr_track.create errored with "Track already exists" regardless of `override`, which only
# covered shadowing a track from another db. Callers had to emr_track.rm() first, leaving the
# track absent for the whole rebuild - anything reading it in that window failed.

test_that("emr_track.import can rewrite a track in the same db with override", {
    a <- emr_extract("track1", keepref = TRUE, names = "value")
    emr_track.import("ovr_track", "global", categorical = FALSE, src = a)
    withr::defer(emr_track.rm("ovr_track", force = TRUE))

    expect_error(emr_track.import("ovr_track", "global", categorical = FALSE, src = a))

    b <- emr_extract("track2", keepref = TRUE, names = "value")
    emr_track.import("ovr_track", "global", categorical = FALSE, src = b, override = TRUE)

    expect_true(emr_track.exists("ovr_track"))
    got <- emr_extract("ovr_track", keepref = TRUE, names = "value")
    expect_equal(got$value, b$value)
})

test_that("emr_track.create can rewrite a track in the same db with override", {
    emr_track.create("ovr_ctrack", "global", categorical = FALSE, expr = "track1")
    withr::defer(emr_track.rm("ovr_ctrack", force = TRUE))
    before <- emr_extract("ovr_ctrack", names = "value")$value

    expect_error(emr_track.create("ovr_ctrack", "global", categorical = FALSE, expr = "track1 + 1"))

    # Same expression shifted by one, so the comparison stays inside a single iterator shape
    # and only tests that the rewrite actually replaced the stored data.
    emr_track.create("ovr_ctrack", "global", categorical = FALSE, expr = "track1 + 1", override = TRUE)

    expect_true(emr_track.exists("ovr_ctrack"))
    expect_equal(emr_extract("ovr_ctrack", names = "value")$value, before + 1)
})

test_that("a read-only track is not overridden in its own db", {
    a <- emr_extract("track1", keepref = TRUE, names = "value")
    emr_track.import("ovr_ro_track", "global", categorical = FALSE, src = a)
    withr::defer({
        emr_track.readonly("ovr_ro_track", FALSE)
        emr_track.rm("ovr_ro_track", force = TRUE)
    })
    emr_track.readonly("ovr_ro_track", TRUE)

    # rename(2) consults the directory's permissions, not the target file's, so the read-only
    # mode on the track file does not stop the write by itself - hence the explicit check.
    b <- emr_extract("track2", keepref = TRUE, names = "value")
    expect_error(emr_track.import("ovr_ro_track", "global", categorical = FALSE, src = b, override = TRUE), "read-only")
    expect_error(emr_track.create("ovr_ro_track", "global", categorical = FALSE, expr = "track2", override = TRUE), "read-only")

    expect_true(emr_track.readonly("ovr_ro_track"))
    expect_equal(emr_extract("ovr_ro_track", keepref = TRUE, names = "value")$value, a$value)
})

test_that("overriding a track keeps its permissions, attributes and vars", {
    a <- emr_extract("track1", keepref = TRUE, names = "value")
    emr_track.import("ovr_keep_track", "global", categorical = FALSE, src = a)
    withr::defer(emr_track.rm("ovr_keep_track", force = TRUE))

    fname <- file.path(emr_db.ls()[1], "ovr_keep_track.nrtrack")
    Sys.chmod(fname, "640", use_umask = FALSE)
    emr_track.attr.set("ovr_keep_track", "description", "before")
    emr_track.var.set("ovr_keep_track", "v", "before")

    b <- emr_extract("track2", keepref = TRUE, names = "value")
    emr_track.import("ovr_keep_track", "global", categorical = FALSE, src = b, override = TRUE)

    # The rename replaces the target inode, so the mode has to be carried over explicitly.
    expect_equal(as.character(file.info(fname)$mode), "640")
    # Unlike emr_track.rm() + write, an in-place override does not discard these.
    expect_equal(emr_track.attr.get("ovr_keep_track", "description"), "before")
    expect_equal(emr_track.var.get("ovr_keep_track", "v"), "before")
})

test_that("overwriting leaves no staging files behind", {
    a <- emr_extract("track1", keepref = TRUE, names = "value")
    emr_track.import("ovr_tmp_track", "global", categorical = FALSE, src = a)
    withr::defer(emr_track.rm("ovr_tmp_track", force = TRUE))
    emr_track.import("ovr_tmp_track", "global", categorical = FALSE, src = a, override = TRUE)
    emr_track.create("ovr_tmp_ctrack", "global", categorical = FALSE, expr = "track1", override = TRUE)
    withr::defer(emr_track.rm("ovr_tmp_ctrack", force = TRUE))

    db_dir <- emr_db.ls()[1]
    expect_length(list.files(db_dir, pattern = "\\.tmp\\."), 0)
})
