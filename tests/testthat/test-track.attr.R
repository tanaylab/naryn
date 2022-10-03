load_test_db()

clean_attributes()

test_that("emr_track.attr.set fails when track doesn't exist", {
    expect_error(emr_track.attr.set("trackaaaa", "var1", "val1"))
})

test_that("emr_track.attr.export returns correct output", {
    withr::defer(clean_attributes())
    emr_track.attr.set("track1", "var1", "val1")
    expect_equal(emr_track.attr.export(), structure(list(track = "track1", attr = "var1", value = "val1"), row.names = 1L, class = "data.frame"))
})

test_that("emr_track.attr.get returns correct output", {
    withr::defer(clean_attributes())
    emr_track.attr.set("track1", "var1", "val1")
    expect_equal(emr_track.attr.get("track1", "var1"), "val1")
})

test_that("emr_track.attr.get returns correct output", {
    withr::defer(clean_attributes())
    emr_track.attr.set("track1", "var1", "val1")
    emr_track.attr.set("track1", "var2", "val2")
    expect_equal(emr_track.attr.get("track1", "var1"), "val1")
})

test_that("emr_track.attr.set works multiple times", {
    withr::defer(clean_attributes())
    emr_track.attr.set("track1", "var1", "val1")
    emr_track.attr.set("track1", "var2", "val2")
    emr_track.attr.set("track1", "var3", "val3")
    emr_track.attr.set("track2", "var2", "baba")
    emr_track.attr.set("track7", "var1", "val3")
    emr_track.attr.set("track7", "var2", "")
    expect_equal(emr_track.attr.export(), structure(list(track = c("track1", "track1", "track1", "track2", "track7", "track7"), attr = c("var1", "var2", "var3", "var2", "var1", "var2"), value = c("val1", "val2", "val3", "baba", "val3", "")), row.names = c(NA, 6L), class = "data.frame"))
})

test_that("emr_track.attr.export works for specific track", {
    withr::defer(clean_attributes())
    emr_track.attr.set("track1", "var1", "val1")
    emr_track.attr.set("track1", "var2", "val2")
    emr_track.attr.set("track1", "var3", "val3")
    emr_track.attr.set("track2", "var2", "baba")
    emr_track.attr.set("track7", "var1", "val3")
    emr_track.attr.set("track7", "var2", "")
    expect_equal(
        emr_track.attr.export("track1"),
        structure(list(track = c("track1", "track1", "track1"), attr = c("var1", "var2", "var3"), value = c("val1", "val2", "val3")), row.names = c(NA, 3L), class = "data.frame")
    )
})

test_that("emr_track.attr.export works for multiple tracks", {
    withr::defer(clean_attributes())
    emr_track.attr.set("track1", "var1", "val1")
    emr_track.attr.set("track1", "var2", "val2")
    emr_track.attr.set("track1", "var3", "val3")
    emr_track.attr.set("track2", "var2", "baba")
    emr_track.attr.set("track7", "var1", "val3")
    emr_track.attr.set("track7", "var2", "")
    expect_equal(
        emr_track.attr.export(c("track1", "track7")),
        structure(list(
            track = c(
                "track1", "track1", "track1", "track7",
                "track7"
            ), attr = c("var1", "var2", "var3", "var1", "var2"),
            value = c("val1", "val2", "val3", "val3", "")
        ), row.names = c(
            NA,
            5L
        ), class = "data.frame")
    )

    expect_equal(
        emr_track.attr.export(c("track1", "track7"), include_missing = TRUE),
        structure(list(track = c(
            "track1", "track1", "track1", "track7",
            "track7", "track7"
        ), attr = c(
            "var1", "var2", "var3", "var1",
            "var2", "var3"
        ), value = c(
            "val1", "val2", "val3", "val3", "",
            NA
        )), row.names = c(NA, -6L), class = "data.frame")
    )

    expect_equal(
        emr_track.attr.export(c("track1", "track7", "savta"), include_missing = TRUE),
        structure(list(track = c(
            "track1", "track1", "track1", "track7",
            "track7", "track7", "savta", "savta", "savta"
        ), attr = c(
            "var1",
            "var2", "var3", "var1", "var2", "var3", "var1", "var2", "var3"
        ), value = c(
            "val1", "val2", "val3", "val3", "", NA, NA, NA,
            NA
        )), row.names = c(NA, -9L), class = "data.frame")
    )

    expect_error(
        emr_track.attr.export(c("track1", "track7", "savta"), include_missing = FALSE)
    )

    expect_equal(
        emr_track.attr.export("savta", c("val2", "val1"), include_missing = TRUE),
        structure(list(track = c("savta", "savta"), attr = c(
            "val2",
            "val1"
        ), value = c(NA_character_, NA_character_)), row.names = c(
            NA,
            -2L
        ), class = "data.frame")
    )

    expect_equal(
        emr_track.attr.export("savta", include_missing = TRUE),
        structure(list(track = character(0), attr = character(0), value = character(0)), row.names = integer(0), class = "data.frame")
    )
})

test_that("emr_track.attr.export works for multiple tracks and vars", {
    withr::defer(clean_attributes())
    emr_track.attr.set("track1", "var1", "val1")
    emr_track.attr.set("track1", "var2", "val2")
    emr_track.attr.set("track1", "var3", "val3")
    emr_track.attr.set("track2", "var2", "baba")
    emr_track.attr.set("track7", "var1", "val3")
    emr_track.attr.set("track7", "var2", "")
    expect_equal(
        emr_track.attr.export(c("track1", "track7"), c("var2", "var3")),
        structure(list(track = c("track1", "track1", "track7"), attr = c(
            "var2",
            "var3", "var2"
        ), value = c("val2", "val3", "")), row.names = c(
            NA,
            3L
        ), class = "data.frame")
    )
})

test_that("emr_track.attr.export works by attributes", {
    withr::defer(clean_attributes())
    emr_track.attr.set("track1", "var1", "val1")
    emr_track.attr.set("track1", "var2", "val2")
    emr_track.attr.set("track1", "var3", "val3")
    emr_track.attr.set("track2", "var2", "baba")
    emr_track.attr.set("track7", "var1", "val3")
    emr_track.attr.set("track7", "var2", "")
    expect_equal(
        emr_track.attr.export(attr = c("var2", "var3")),
        structure(list(track = c("track1", "track1", "track2", "track7"), attr = c("var2", "var3", "var2", "var2"), value = c(
            "val2",
            "val3", "baba", ""
        )), row.names = c(NA, 4L), class = "data.frame")
    )
})

test_that("emr_track.attr.rm works", {
    withr::defer(clean_attributes())
    emr_track.attr.set("track1", "var1", "val1")

    emr_track.attr.rm("track1", "var1")

    expect_equal(
        emr_track.attr.export("track1", "var1"),
        data.frame(track = character(0), attr = character(0), value = character(0))
    )
})

test_that("emr_track.attr.rm works in batch mode", {
    withr::defer(clean_attributes())
    emr_track.attr.set("track1", "var1", "val1")
    emr_track.attr.set("track1", "var2", "val2")
    emr_track.attr.set("track1", "var3", "val3")
    emr_track.attr.set("track2", "var2", "baba")
    emr_track.attr.set("track7", "var1", "val3")
    emr_track.attr.set("track7", "var2", "")

    emr_track.attr.rm(c("track1", "track2", "track7"), "var1")

    expect_equal(
        emr_track.attr.export(c("track1", "track2", "track7"), "var1"),
        data.frame(track = character(0), attr = character(0), value = character(0))
    )
})


test_that("emr_track.ls finds tracks by var", {
    withr::defer(clean_attributes())
    emr_track.attr.set("track1", "var1", "val1")
    emr_track.attr.set("track1", "var2", "val2")
    emr_track.attr.set("track1", "var3", "val3")
    emr_track.attr.set("track2", "var2", "baba")
    emr_track.attr.set("track7", "var1", "val3")
    emr_track.attr.set("track7", "var2", "")
    expect_equal(emr_track.ls(var1 = ""), c("track1", "track7"))
})

test_that("emr_track.ls finds tracks by var and value", {
    withr::defer(clean_attributes())
    emr_track.attr.set("track1", "var1", "val1")
    emr_track.attr.set("track1", "var2", "val2")
    emr_track.attr.set("track1", "var3", "val3")
    emr_track.attr.set("track2", "var2", "baba")
    emr_track.attr.set("track7", "var1", "kuku")
    emr_track.attr.set("track7", "var2", "")
    expect_equal(emr_track.ls(var1 = "kuku"), "track7")
})

test_that("emr_track.ls finds tracks by multiple vars and values with regex", {
    withr::defer(clean_attributes())
    emr_track.attr.set("track1", "var1", "val1")
    emr_track.attr.set("track1", "var2", "val2")
    emr_track.attr.set("track1", "var3", "val3")
    emr_track.attr.set("track2", "var2", "baba")
    emr_track.attr.set("track7", "var1", "val3")
    emr_track.attr.set("track7", "var2", "")
    expect_equal(emr_track.ls(var1 = "val*", var2 = ""), c("track1", "track7"))
})

test_that("emr_track.rm removes the track attributes", {
    withr::defer(clean_attributes())
    initial_attrs <- emr_track.attr.export()
    df <- data.frame(id = 1, time = c(1, 2, 2), value = c(-1, 4, 3), ref = c(0, 0, 1))
    emr_track.import("tmp", space = "user", categorical = TRUE, src = df)
    withr::defer(emr_track.rm("tmp", force = TRUE))
    emr_track.attr.set("tmp", "var1", "val1")
    attrs_file <- file.path(.naryn$EMR_GROOT, "utest", ".tmp.attrs")
    expect_true(file.exists(attrs_file))
    expect_equal(emr_track.attr.get("tmp", "var1"), "val1")
    emr_track.rm("tmp", force = TRUE)
    expect_error(emr_track.attr.get("tmp", "var1"))
    expect_equal(emr_track.attr.export(), initial_attrs)
    expect_false(file.exists(attrs_file))
})

test_that("emr_track.rm for multiple tracks removes the track attributes", {
    withr::defer(clean_attributes())
    initial_attrs <- emr_track.attr.export()
    df <- data.frame(id = 1, time = c(1, 2, 2), value = c(-1, 4, 3), ref = c(0, 0, 1))
    emr_track.import("tmp", space = "user", categorical = TRUE, src = df)
    emr_track.import("tmp2", space = "user", categorical = TRUE, src = df)
    withr::defer(emr_track.rm("tmp", force = TRUE))
    withr::defer(emr_track.rm("tmp2", force = TRUE))
    emr_track.attr.set("tmp", "var1", "val1")
    emr_track.attr.set("tmp2", "var1", "val1")
    attrs_file1 <- file.path(.naryn$EMR_GROOT, "utest", ".tmp.attrs")
    attrs_file2 <- file.path(.naryn$EMR_GROOT, "utest", ".tmp2.attrs")
    expect_true(file.exists(attrs_file1))
    expect_true(file.exists(attrs_file2))
    expect_equal(emr_track.attr.get("tmp", "var1"), "val1")
    expect_equal(emr_track.attr.get("tmp2", "var1"), "val1")
    emr_track.rm(c("tmp", "tmp2"), force = TRUE)
    expect_error(emr_track.attr.get("tmp", "var1"))
    expect_error(emr_track.attr.get("tmp2", "var1"))
    expect_equal(emr_track.attr.export(), initial_attrs)
    expect_false(file.exists(attrs_file1))
    expect_false(file.exists(attrs_file2))
})

test_that("emr_track.attr.set in batch mode works", {
    withr::defer(clean_attributes())
    emr_track.attr.set(c("track1", "track2", "track3"), c("var1", "var1", "var2"), c("val1", "val1", "val2"))
    expect_equal(
        emr_track.attr.export(),
        structure(list(
            track =
                c("track1", "track2", "track3"),
            attr = c(
                "var1",
                "var1",
                "var2"
            ), value = c("val1", "val1", "val2")
        ), row.names = c(
            NA,
            3L
        ), class = "data.frame")
    )
    expect_equal(emr_track.ls(var1 = "val*"), c("track1", "track2"))
    expect_equal(emr_track.ls(var2 = "val*"), c("track3"))

    expect_error(emr_track.attr.set(c("track1", "track2", "track3"), "var4", "val2"))
    expect_error(emr_track.attr.set(c("track1", "track2", "track3"), rep("var4", 3), "val2"))
})

test_that("emr_track.attr.set in batch mode works with repeated tracks", {
    withr::defer(clean_attributes())
    emr_track.attr.set(c("track1", "track2", "track3", "track1"), c("var1", "var1", "var2", "var3"), c("val1", "val1", "val2", "val3"))
    expect_equal(
        emr_track.attr.export(),
        structure(list(track = c("track1", "track1", "track2", "track3"), attr = c("var1", "var3", "var1", "var2"), value = c(
            "val1",
            "val3", "val1", "val2"
        )), row.names = c(NA, 4L), class = "data.frame")
    )
})

test_that("emr_track.attr.set in batch mode works with an empty value", {
    withr::defer(clean_attributes())
    emr_track.attr.set(c("track1", "track2", "track3"), rep("var1", 3), value = rep("", 3))
    expect_equal(
        emr_track.attr.export(),
        structure(list(track = c("track1", "track2", "track3"), attr = c(
            "var1",
            "var1", "var1"
        ), value = c("", "", "")), row.names = c(NA, 3L), class = "data.frame")
    )
    expect_equal(emr_track.ls(var1 = ""), c("track1", "track2", "track3"))
})

test_that("emr_track.attr.set fails with non-ascii characters", {
    withr::defer(clean_attributes())
    v1 <- "\x8c\x81\x8f"
    expect_error(emr_track.attr.set("track1", "var1", v1))
    expect_error(emr_track.attr.set("track1", v1, "val1"))

    expect_error(emr_track.attr.set(c("track1", "track2"), c("v1", v1), c("val1", "val1")))
    expect_error(emr_track.attr.set(c("track1", "track2"), c("v1", "v1"), c("val1", v1)))
})
