
test_that("error is returned when track doesn't exist", {
    expect_error(emr_track.var.get("aaa", "blablablabla"))
    expect_error(emr_track.var.get("aaa", "blablablabla", 5))
})

test_that("error is returned when track variable doesn't exist", {
    expect_error(emr_track.var.get("track1", "blablablabla"))
})

test_that("emr_track.var.set works", {
    emr_track.var.set("track1", "v1", c(1, 4))
    withr::defer(emr_track.var.rm("track1", "v1"))
    expect_equal(emr_track.var.get("track1", "v1"), c(1, 4))
})

test_that("emr_track.var.set named variable works", {
    emr_track.var.set("track1", "v1", c(1, 4))
    withr::defer(emr_track.var.rm("track1", "v1"))
    expect_equal(emr_track.var.get("track1", "v1"), c(1, 4))
})

test_that("emr_track.var.ls returns nothing when there are no variables", {
    expect_equal(emr_track.var.ls("track1"), character(0))
})

test_that("emr_track.var.ls works", {
    emr_track.var.set("track1", "v1", c(1, 4))
    emr_track.var.set("track1", "v2", 5)
    withr::defer(emr_track.var.rm("track1", "v1"))
    withr::defer(emr_track.var.rm("track1", "v2"))
    expect_equal(emr_track.var.ls("track1"), c("v1", "v2"))
})

test_that("emr_track.var.rm error is returned when track doesn't exist", {
    expect_error(emr_track.var.rm("blabla", "v"))
})

test_that("emr_track.var.rm error is returned when track variable doesn't exist", {
    expect_error(emr_track.var.rm("track1", "v"))
})

test_that("emr_track.var.rm works", {
    emr_track.var.set("track1", "v1", c(1, 4))
    emr_track.var.set("track1", "v2", 5)
    emr_track.var.rm("track1", "v1")
    expect_equal(emr_track.var.ls("track1"), "v2")
    emr_track.var.rm("track1", "v2")
    expect_equal(emr_track.var.ls("track1"), character(0))
})
