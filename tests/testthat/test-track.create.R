test_that("emr_track.create works", {
    emr_track.rm("test_track1", TRUE)
    emr_track.create("test_track1", "user", FALSE, "track0", keepref = TRUE)
    expect_true(emr_track.exists("test_track1"))
    withr::defer(emr_track.rm("test_track1", TRUE))

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

test_that("create and remove categorical", {
    emr_track.rm("test_track1", TRUE)
    r_extract <- emr_extract("track0+2", keepref=TRUE, names="test_track1")
    emr_track.create("test_track1", "user", FALSE, "track0+2", keepref=TRUE)
    expect_true(emr_track.exists("test_track1"))    
    r_create <- emr_extract("test_track1", keepref=TRUE)
    expect_equal(r_extract, r_create)
    emr_track.rm("test_track1", TRUE)
    expect_false(emr_track.exists("test_track1"))
})

test_that("create categorical keepref=FALSE", {
    emr_track.rm("test_track1", TRUE)
    r_extract <- emr_extract("track0+2", keepref=FALSE, names="test_track1")
    emr_track.create("test_track1", "user", FALSE, "track0+2", keepref=FALSE)
    expect_true(emr_track.exists("test_track1"))
    withr::defer(emr_track.rm("test_track1", TRUE))
    
    r_create <- emr_extract("test_track1", keepref=TRUE)
    expect_equal(nrow(r_create), 99508)
    expect_equal(r_extract, r_create, tolerance = 1e-7)  
})

test_that("create categorical with filter", {
    emr_track.rm("test_track1", TRUE)    
    emr_track.create("test_track1", "user", FALSE, "track0", filter="!track0")
    expect_true(emr_track.exists("test_track1"))
    withr::defer(emr_track.rm("test_track1", TRUE))
    
    r_create <- emr_extract("test_track1", keepref=TRUE)
    expect_equal(nrow(r_create), 0)    
})

test_that("emr_track.mv works", {
    emr_track.rm("test_track1", TRUE)
    emr_track.create("test_track1", "user", F, "track0+2", keepref=F)
    emr_track.mv("test_track1", "test_track2")
    expect_false(emr_track.exists("test_track1"))
    expect_true(emr_track.exists("test_track2"))
    withr::defer(emr_track.rm("test_track2", TRUE))    
})

test_that("emr_track.mv works with different values", {
    emr_track.rm("test_track1", TRUE)
    emr_track.create("test_track1", "global", F, "track0+2", keepref=F)
    emr_track.mv("test_track1", "test_track2", "user")
    expect_false(emr_track.exists("test_track1"))
    expect_true(emr_track.exists("test_track2"))    
    withr::defer(emr_track.rm("test_track2", TRUE))    
})
