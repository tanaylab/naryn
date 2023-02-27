test_that("emr_entries.reload() should reload entries from disk", {
    emr_db.init_examples()
    expect_true(!is.null(.naryn$entries[[emr_db.ls()[1]]]))
})

test_that("emr_entries.get() should return the expected record", {
    emr_db.init_examples()
    expect_equal(emr_entries.get("record1"), "value1")
})

test_that("emr_entries.get_all() should return a list of entries", {
    emr_db.init_examples()
    expect_equal(emr_entries.get_all(), list(record1 = "value1", record2 = 60427L, record3 = c(
        "value1",
        "value2", "value3"
    )))
})

test_that("emr_entries.set() should set the value of the specified record", {
    emr_db.init_examples()
    emr_entries.set("record1", "new value")
    expect_equal(emr_entries.get("record1"), "new value")
})

test_that("emr_entries.ls() should return a list of record keys", {
    emr_db.init_examples()
    expect_equal(emr_entries.ls(), c("record1", "record2", "record3"))
})

test_that("emr_entries.ls() works with a specified db_dir", {
    emr_db.init_examples(2)
    expect_equal(emr_entries.ls(db_dir = emr_db.ls()[2]), c("record1", "record2", "record3"))
})

test_that("emr_entries.rm() should remove the specified record", {
    emr_db.init_examples()
    emr_entries.rm("record1")
    expect_equal(emr_entries.ls(), c("record2", "record3"))
})

test_that("emr_entries.rm() works with a specified db_dir", {
    emr_db.init_examples(2)
    emr_entries.rm("record1", db_dir = emr_db.ls()[2])
    expect_equal(emr_entries.ls(db_dir = emr_db.ls()[2]), c("record2", "record3"))
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
    expect_equal(emr_entries.get("record1", db_dir = emr_db.ls()[2]), "value1")
})

test_that("emr_entries.get_all() works with a specified db_dir", {
    emr_db.init_examples(2)
    expect_equal(emr_entries.get_all(db_dir = emr_db.ls()[2]), list(record1 = "value1", record2 = 60427L, record3 = c("value1", "value2", "value3")))
})

test_that("emr_entries.set() works with a specified db_dir", {
    emr_db.init_examples(2)
    emr_entries.set("record1", "new value", db_dir = emr_db.ls()[2])
    expect_equal(emr_entries.get("record1", db_dir = emr_db.ls()[2]), "new value")
})

test_that("emr_entries.set() works when entries.yaml file doesn't exist", {
    emr_db.init_examples()
    track_db <- emr_db.ls()[1]
    unlink(file.path(track_db, "entries.yaml"))

    emr_entries.set("record1", "new value", db_dir = track_db)
    expect_equal(emr_entries.get("record1", db_dir = track_db), "new value")
})
