test_that("emr_track.create works", {
    emr_track.rm("test_track1", TRUE)
    emr_track.create("test_track1", "user", FALSE, "track0", keepref = TRUE)
    withr::defer(emr_track.rm("test_track1", T))

    e0 <- emr_extract("track0", keepref = TRUE)
    e1 <- emr_extract(c("test_track1", "track0"), iterator = "test_track1", keepref = TRUE)

    expect_equal(nrow(e1), 100000)
    expect_identical(e1$test_track1, e1$track0)
    expect_equal(e1$track0, e1$test_track1)
    expect_equal(e0$track0, e1$test_track1)
    expect_equal(e0$id, e1$id)
    expect_equal(e0$time, e1$time)
    expect_equal(e0$ref, e1$ref)

    e2 <- emr_extract(c("test_track1", "track0"), iterator = "track0", keepref = TRUE)
    expect_identical(e1, e2)

    track.info <- emr_track.info("test_track1")
    track.info$path <- NULL
    expect_equal(
        track.info,
        list(
            type = "dense", data.type = "float", categorical = FALSE,
            num.vals = 100000L, num.unique.vals = 1000L, min.val = 0,
            max.val = 999, min.id = 0L, max.id = 999L, min.time = 0L,
            max.time = 9999L
        )
    )
    track.info$path <- NULL
})

# test_that("test", {
#     emr_track.rm("test_track1", T)
#     emr_track.create("test_track1", "user", F, "track0+2", keepref=F)
#     r<-emr_extract("test_track1", keepref=T)
#     emr_track.rm("test_track1", T)
#     r
# })

# test_that("test", {
#     emr_track.rm("test_track1", T)
#     emr_track.create("test_track1", "user", F, "track0", filter="!track0")
#     r<-emr_extract("test_track1", keepref=T)
#     emr_track.rm("test_track1", T)
#     r
# })

# test_that("test", {
#     emr_track.rm("test_track1", T)
#     emr_track.create("test_track1", "user", F, "track0+2", keepref=F)
#     emr_track.mv("test_track1", "test_track2")
#     r<-list(emr_track.global.ls(), emr_track.user.ls())
#     emr_track.rm("test_track2", T)
#     r
# })

# test_that("test", {
#     emr_track.rm("test_track1", T)
#     emr_track.create("test_track1", "global", F, "track0+2", keepref=F)
#     emr_track.mv("test_track1", "test_track2", "user")
#     r<-list(emr_track.global.ls(), emr_track.user.ls())
#     emr_track.rm("test_track2", T)
#     r
# })
