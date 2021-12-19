test_that("creating a db and tracks is always done in umask 007", {
    tmp_db <- file.path(tempdir(), "db")
    dir.create(tmp_db)
    emr_db.connect(tmp_db)

    expect_equal(as.character(file.info(file.path(tmp_db, ".naryn"))$mode), "660")
    expect_equal(as.character(file.info(file.path(tmp_db, ".logical_tracks"))$mode), "660")

    df <- data.frame(id = 1, time = 1, value = 1)
    emr_track.import("tr", space = "global", categorical = FALSE, src = df)
    expect_equal(as.character(file.info(file.path(tmp_db, "tr.nrtrack"))$mode), "660")
})

test_that("creating a track variable is done in umask 007", {
    tmp_db <- file.path(tempdir(), "db2")
    dir.create(tmp_db)
    emr_db.connect(tmp_db)

    df <- data.frame(id = 1, time = 1, value = 1)
    emr_track.import("tr", space = "global", categorical = FALSE, src = df)
    emr_track.var.set("tr", "v", "val")

    expect_equal(as.character(file.info(file.path(tmp_db, ".tr.var", "v"))$mode), "660")
    expect_equal(as.character(file.info(file.path(tmp_db, ".tr.var"))$mode), "770")
})
