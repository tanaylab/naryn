test_that("emr_entries.reload() should reload entries from disk", {
    emr_db.init_examples()
    expect_true(!is.null(.naryn$entries[[emr_db.ls()[1]]]))
})

test_that("emr_entries.get() should return the expected entry", {
    emr_db.init_examples()
    expect_equal(emr_entries.get("entry1"), "value1")
})

test_that("emr_entries.get_all() should return a list of entries", {
    emr_db.init_examples()
    expect_equal(emr_entries.get_all(), list(entry1 = "value1", entry2 = 60427L, entry3 = c(
        "value1",
        "value2", "value3"
    )))
})

test_that("emr_entries.set() should set the value of the specified entry", {
    emr_db.init_examples()
    emr_entries.set("entry1", "new value")
    expect_equal(emr_entries.get("entry1"), "new value")
})

test_that("emr_entries.ls() should return a list of entry keys", {
    emr_db.init_examples()
    expect_equal(emr_entries.ls(), c("entry1", "entry2", "entry3"))
})

test_that("emr_entries.ls() works with a specified db_dir", {
    emr_db.init_examples(2)
    expect_equal(emr_entries.ls(db_dir = emr_db.ls()[2]), c("entry1", "entry2", "entry3"))
})

test_that("emr_entries.rm() should remove the specified entry", {
    emr_db.init_examples()
    emr_entries.rm("entry1")
    expect_equal(emr_entries.ls(), c("entry2", "entry3"))
})

test_that("emr_entries.rm() works with a specified db_dir", {
    emr_db.init_examples(2)
    emr_entries.rm("entry1", db_dir = emr_db.ls()[2])
    expect_equal(emr_entries.ls(db_dir = emr_db.ls()[2]), c("entry2", "entry3"))
})

test_that("emr_entries.rm_all() should remove all entries", {
    emr_db.init_examples()
    emr_entries.rm_all()
    expect_equal(emr_entries.ls(), character(0))
})

test_that("emr_entries.rm_all() works with a specified db_dir", {
    emr_db.init_examples(2)
    emr_entries.rm_all(db_dir = emr_db.ls()[2])
    expect_equal(emr_entries.ls(db_dir = emr_db.ls()[2]), character(0))
})

test_that("emr_entries.get() workds with a specified db_dir", {
    emr_db.init_examples(2)
    expect_equal(emr_entries.get("entry1", db_dir = emr_db.ls()[2]), "value1")
})

test_that("emr_entries.get_all() works with a specified db_dir", {
    emr_db.init_examples(2)
    expect_equal(emr_entries.get_all(db_dir = emr_db.ls()[2]), list(entry1 = "value1", entry2 = 60427L, entry3 = c("value1", "value2", "value3")))
})

test_that("emr_entries.set() works with a specified db_dir", {
    emr_db.init_examples(2)
    emr_entries.set("entry1", "new value", db_dir = emr_db.ls()[2])
    expect_equal(emr_entries.get("entry1", db_dir = emr_db.ls()[2]), "new value")
})

test_that("emr_entries.set() works when entries.yaml file doesn't exist", {
    emr_db.init_examples()
    track_db <- emr_db.ls()[1]
    unlink(file.path(track_db, "entries.yaml"))

    emr_entries.set("entry1", "new value", db_dir = track_db)
    expect_equal(emr_entries.get("entry1", db_dir = track_db), "new value")
})

test_that("emr_entries.get() is reloaded when file is changed on disk", {
    emr_db.init_examples()
    track_db <- emr_db.ls()[1]
    entries <- yaml::read_yaml(file.path(track_db, "entries.yaml"))
    entries$entry1 <- "new value"
    yaml::write_yaml(entries, file.path(track_db, "entries.yaml"))

    expect_equal(emr_entries.get("entry1", db_dir = track_db), "new value")
})

test_that("emr_entries.get_all() is reloaded when file is changed on disk", {
    emr_db.init_examples()
    track_db <- emr_db.ls()[1]
    entries <- yaml::read_yaml(file.path(track_db, "entries.yaml"))
    entries$entry1 <- "new value"
    yaml::write_yaml(entries, file.path(track_db, "entries.yaml"))

    expect_equal(emr_entries.get_all(db_dir = track_db), list(entry1 = "new value", entry2 = 60427L, entry3 = c("value1", "value2", "value3")))
})

test_that("timestamp is not changed after reloading twice", {
    emr_db.init_examples()
    ts <- .naryn$entries_timestamp[[emr_db.ls()[1]]]
    emr_db.reload()
    ts1 <- .naryn$entries_timestamp[[emr_db.ls()[1]]]
    expect_equal(ts, ts1)
})

test_that("timestamp is changed after changing the file", {
    emr_db.init_examples()
    ts <- .naryn$entries_timestamp[[emr_db.ls()[1]]]
    track_db <- emr_db.ls()[1]
    entries <- yaml::read_yaml(file.path(track_db, "entries.yaml"))
    entries$entry1 <- "new value"
    yaml::write_yaml(entries, file.path(track_db, "entries.yaml"))
    emr_db.reload()
    ts1 <- .naryn$entries_timestamp[[emr_db.ls()[1]]]
    expect_true(ts != ts1)
})

test_that("timestamp is changed after setting a value", {
    emr_db.init_examples()
    ts <- .naryn$entries_timestamp[[emr_db.ls()[1]]]
    emr_entries.set("entry1", "new value")
    ts1 <- .naryn$entries_timestamp[[emr_db.ls()[1]]]
    expect_true(ts != ts1)
})



