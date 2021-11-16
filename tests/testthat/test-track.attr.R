
clean_attributes <- function() {
    if (nrow(emr_track.attr.export())) {
        apply(emr_track.attr.export(), 1, function(x) {
            emr_track.attr.rm(x[1], x[2])
        })
    }
}
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
    expect_equal(emr_track.attr.get("track1", "var1"), "var1")
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

test_that("emr_track.attr.set works for specific track", {
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

test_that("emr_track.attr.set works for multiple tracks", {
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
})

test_that("emr_track.attr.set works for multiple tracks and vars", {
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

test_that("emr_track.attr.set works by attributes", {
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
