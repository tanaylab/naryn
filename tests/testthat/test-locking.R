# Guards for the 2.7.0 locking holes. Readers take no lock and rely on every write being
# committed by a rename, so most of the failure modes here need two processes inside the same
# few microseconds and are not reachable from a test. These two are.

fresh_db <- function(name) {
    db <- file.path(tempdir(), name)
    unlink(db, recursive = TRUE)
    dir.create(db)
    emr_db.connect(db)
    df <- data.frame(id = 1, time = 1, value = 1)
    emr_track.import("ta", space = "global", categorical = FALSE, src = df)
    emr_track.import("tb", space = "global", categorical = FALSE, src = df)
    db
}

test_that("a failed commit raises instead of reporting success", {
    db <- fresh_db("lock_db_failed_commit")
    emr_track.attr.set("ta", "description", "first")

    # Every metadata write stages to a temporary file and commits it with a rename. Putting a
    # directory where the target belongs is the one way to fail that rename, and only that
    # rename, without root: staging still succeeds, and rename(2) refuses to replace a
    # directory. The commit failure used to be dropped on the floor - the update was discarded,
    # the old file stayed, and R saw success.
    agg <- file.path(db, ".attrs")
    unlink(agg)
    dir.create(agg)
    withr::defer(unlink(agg, recursive = TRUE))

    expect_error(emr_track.attr.set("ta", "description", "second"), "Failed to commit")
    # The rename's errno, not whatever the cleanup unlink left behind.
    expect_error(emr_track.attr.set("ta", "description", "second"), "Is a directory")
})

test_that("a write through a missing track list keeps its lock and rebuilds the list", {
    db <- fresh_db("lock_db_missing_list")

    # emr_track.import holds the track-list lock and then reaches create_track_list_file, which
    # asks for the same lock. That nested acquire used to open and close a second fd on the file,
    # and closing any fd drops every fcntl lock the process holds on it - so the rest of the
    # write ran unprotected. The nested acquire now no-ops instead.
    # The dropped lock is silent on its own, so this only asserts the path still works: it
    # catches a recursive acquire that blocks or fails outright, not the missing exclusion.
    unlink(file.path(db, ".naryn"))

    df <- data.frame(id = 2, time = 2, value = 2)
    emr_track.import("tc", space = "global", categorical = FALSE, src = df, override = TRUE)

    expect_true(file.exists(file.path(db, ".naryn")))
    expect_true(emr_track.exists("tc"))
    emr_db.reload()
    expect_true(all(c("ta", "tb", "tc") %in% emr_track.ls()))
})
