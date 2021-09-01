clean_logical_tracks()

# emr_vtrack.info
test_that("emr_vtrack.info works on logical tracks", {
    EMR_VTRACKS <<- list()
    withr::defer(clean_logical_tracks())    
    emr_track.create_logical("l1", "ph1", c(15, 16))

    emr_vtrack.create("vt_l", src="l1", func="earliest.time", time.shift=c(-100, 50), params=c(-1, 14, 16, 30))

    vt_info <- emr_vtrack.info("vt_l")

    expect_equal(vt_info, list(src="l1", time_shift=c(-100, 50), func="earliest.time", params=c(-1, 14, 16, 30), keepref=FALSE, id_map=NULL, filter=NULL))
})

# emr_vtrack.create

test_that("empty emr_vtrack.create works on logical track with all keepref combinations", {

    EMR_VTRACKS <<- list()
    withr::defer(clean_logical_tracks())

    emr_filter.create("f1", src="ph1", val=c(15, 16), keepref=TRUE)
    df <- emr_extract("ph1", names=c("value"), keepref=TRUE, filter="f1")
    emr_track.import("l1_ph", space="global", categorical=TRUE, src=df)
    emr_track.create_logical("l1", "ph1", c(15, 16))

    emr_vtrack.create("vt_p", src="l1_ph")
    emr_vtrack.create("vt_l", src="l1")

    t1 <- emr_extract("vt_p", names=c("val"))
    t2 <- emr_extract("vt_l", names=c("val"))

    expect_equal(t1, t2)

    withr::defer(emr_vtrack.rm("vt_p"))
    withr::defer(emr_vtrack.rm("vt_l"))

    emr_vtrack.create("vt_p", src="l1_ph", keepref=TRUE)
    emr_vtrack.create("vt_l", src="l1", keepref=TRUE)

    t1 <- emr_extract("vt_p", names=c("val"), keepref=TRUE)
    t2 <- emr_extract("vt_l", names=c("val"), keepref=TRUE)

    expect_equal(t1, t2)

    withr::defer(emr_vtrack.rm("vt_p"))
    withr::defer(emr_vtrack.rm("vt_l"))

    emr_vtrack.create("vt_p", src="l1_ph", keepref=FALSE)
    emr_vtrack.create("vt_l", src="l1", keepref=FALSE)

    t1 <- emr_extract("vt_p", names=c("val"), keepref=TRUE)
    t2 <- emr_extract("vt_l", names=c("val"), keepref=TRUE)

    expect_equal(t1, t2)

    withr::defer(emr_vtrack.rm("vt_p"))
    withr::defer(emr_vtrack.rm("vt_l"))

    emr_vtrack.create("vt_p", src="l1_ph", keepref=TRUE)
    emr_vtrack.create("vt_l", src="l1", keepref=TRUE)

    t1 <- emr_extract("vt_p", names=c("val"), keepref=FALSE)
    t2 <- emr_extract("vt_l", names=c("val"), keepref=FALSE)

    expect_equal(t1, t2)

    withr::defer(emr_track.rm("l1_ph", force = TRUE))
  
})

test_that("emr_vtrack functions work on logical tracks as expected", {
    EMR_VTRACKS <<- list()
    withr::defer(clean_logical_tracks())

    emr_filter.create("f1", src="ph1", val=c(15, 16), keepref=TRUE)
    df <- emr_extract("ph1", names=c("value"), keepref=TRUE, filter="f1")
    emr_track.import("l1_ph", space="global", categorical=TRUE, src=df)
    emr_track.create_logical("l1", "ph1", c(15, 16))

    emr_vtrack.create("vt_p", src="l1_ph", params=c(15))
    emr_vtrack.create("vt_l", src="l1", params=c(15))

    t1 <- emr_extract("vt_p", names=c("val"))
    t2 <- emr_extract("vt_l", names=c("val"))

    expect_equal(t1, t2)

    withr::defer(emr_vtrack.rm("vt_p"))
    withr::defer(emr_vtrack.rm("vt_l"))

    emr_vtrack.create("vt_p", src="l1_ph", func='closest.earlier.time', params=c(15))
    emr_vtrack.create("vt_l", src="l1", func='closest.earlier.time', params=c(15))

    t1 <- emr_extract("vt_p", names=c("val"))
    t2 <- emr_extract("vt_l", names=c("val"))

    expect_equal(t1, t2)

    withr::defer(emr_vtrack.rm("vt_p"))
    withr::defer(emr_vtrack.rm("vt_l"))
    # will not work with 'exists'!!!
    emr_vtrack.create("vt_p", src="l1_ph", func='size', params=c(19))
    emr_vtrack.create("vt_l", src="l1", func='size', params=c(19))

    t1 <- emr_extract("vt_p", names=c("val"))
    t2 <- emr_extract("vt_l", names=c("val"))

    expect_equal(t1, t2)

    withr::defer(emr_vtrack.rm("vt_p"))
    withr::defer(emr_vtrack.rm("vt_l"))

    emr_vtrack.create("vt_p", src="l1_ph", func='earliest.time', time.shift=c(-100, 50))
    emr_vtrack.create("vt_l", src="l1", func='earliest.time', time.shift=c(-100, 50))

    t1 <- emr_extract("vt_p", names=c("val"))
    t2 <- emr_extract("vt_l", names=c("val"))

    expect_equal(t1, t2)

    withr::defer(emr_track.rm("l1_ph", force = TRUE))
})


test_that("emr_vtrack functions work on logical tracks with keepref combinations", {
    EMR_VTRACKS <<- list()
    withr::defer(clean_logical_tracks())

    emr_filter.create("f1", src="ph1", val=c(15, 16), keepref=TRUE)
    df <- emr_extract("ph1", names=c("value"), keepref=TRUE, filter="f1")
    emr_track.import("l1_ph", space="global", categorical=TRUE, src=df)
    emr_track.create_logical("l1", "ph1", c(15, 16))

    withr::defer(emr_vtrack.rm("vt_p"))
    withr::defer(emr_vtrack.rm("vt_l"))

    emr_vtrack.create("vt_p", src="l1_ph", func='closest.earlier.time', params=c(15), keepref=TRUE)
    emr_vtrack.create("vt_l", src="l1", func='closest.earlier.time', params=c(15), keepref=TRUE)

    t1 <- emr_extract("vt_p", names=c("val"), keepref=FALSE)
    t2 <- emr_extract("vt_l", names=c("val"), keepref=FALSE)

    expect_equal(t1, t2)

    withr::defer(emr_vtrack.rm("vt_p"))
    withr::defer(emr_vtrack.rm("vt_l"))

    emr_vtrack.create("vt_p", src="l1_ph", func='frequent', time.shift=c(-100, 200), keepref=FALSE)
    emr_vtrack.create("vt_l", src="l1", func='frequent', time.shift=c(-100, 200), keepref=FALSE)

    t1 <- emr_extract("vt_p", names=c("val"), keepref=TRUE)
    t2 <- emr_extract("vt_l", names=c("val"), keepref=TRUE)

    expect_equal(t1, t2)

    withr::defer(emr_vtrack.rm("vt_p"))
    withr::defer(emr_vtrack.rm("vt_l"))

    emr_vtrack.create("vt_p", src="l1_ph", func='value', params=16, keepref=TRUE)
    emr_vtrack.create("vt_l", src="l1", func='value', params=16, keepref=TRUE)

    t1 <- emr_extract("vt_p", names=c("val"), keepref=TRUE)
    t2 <- emr_extract("vt_l", names=c("val"), keepref=TRUE)

    expect_equal(t1, t2)

    withr::defer(emr_track.rm("l1_ph", force = TRUE))
})

test_that("emr_vtrack.create with filter works on logical tracks", {
    EMR_VTRACKS <<- list()
    EMR_FILTERS <<- list()
    withr::defer(clean_logical_tracks())

    emr_filter.create("f1", src="ph1", val=seq(4, 16, 1), keepref=TRUE)
    df <- emr_extract("ph1", names=c("value"), keepref=TRUE, filter="f1")
    emr_track.import("l1_ph", space="global", categorical=TRUE, src=df)
    emr_track.create_logical("l1", "ph1", seq(4, 16, 1))

    emr_filter.create("f1", src="ph1", val=c(15, 16))

    emr_vtrack.create("vt_p", src="l1_ph", filter="f1")
    emr_vtrack.create("vt_l", src="l1", filter="f1")

    t1 <- emr_extract("vt_p", names=c("val"))
    t2 <- emr_extract("vt_l", names=c("val"))

    expect_equal(t1, t2)

    withr::defer(emr_vtrack.rm("vt_p"))
    withr::defer(emr_vtrack.rm("vt_l"))

    emr_filter.create("f1", src="l1_ph", val=c(4))
    emr_filter.create("f2", src="l1", val=c(4))

    emr_vtrack.create("vt_p", src="l1_ph", filter="f1")
    emr_vtrack.create("vt_l", src="l1", filter="f2")

    t1 <- emr_extract("vt_p", names=c("val"))
    t2 <- emr_extract("vt_l", names=c("val"))

    expect_equal(t1, t2)

    withr::defer(emr_vtrack.rm("vt_p"))
    withr::defer(emr_vtrack.rm("vt_l"))

    emr_filter.create("f1", src="l1_ph", val=c(4, 5))
    emr_filter.create("f2", src="l1", val=c(4, 5))

    emr_vtrack.create("vt_p", src="l1_ph", func='closest', params=c(15), filter="f1")
    emr_vtrack.create("vt_l", src="l1", func='closest', params=c(15), filter="f2")

    t1 <- emr_extract("vt_p", names=c("val"))
    t2 <- emr_extract("vt_l", names=c("val"))

    expect_equal(t1, t2)

    withr::defer(emr_vtrack.rm("vt_p"))
    withr::defer(emr_vtrack.rm("vt_l"))

    emr_filter.create("f1", src="l1_ph", val=c(4, 5))
    emr_filter.create("f2", src="l1", val=c(4, 5))

    emr_vtrack.create("vt_p", src="l1_ph", func='closest', params=c(19), filter="f1")
    emr_vtrack.create("vt_l", src="l1", func='closest', params=c(19), filter="f2")

    t1 <- emr_extract("vt_p", names=c("val"), keepref=TRUE)
    t2 <- emr_extract("vt_l", names=c("val"), keepref=TRUE)

    expect_equal(t1, t2)

    withr::defer(emr_vtrack.rm("vt_p"))
    withr::defer(emr_vtrack.rm("vt_l"))

    emr_filter.create("f1", src="l1_ph", val=c(19))
    emr_filter.create("f2", src="l1", val=c(19))

    emr_vtrack.create("vt_p", src="l1_ph", func='closest', filter="f1")
    emr_vtrack.create("vt_l", src="l1", func='closest', filter="f2")

    t1 <- emr_extract("vt_p", names=c("val"), keepref=TRUE)
    t2 <- emr_extract("vt_l", names=c("val"), keepref=TRUE)

    expect_equal(t1, t2)

    withr::defer(emr_track.rm("l1_ph", force = TRUE))

})

test_that("emr_vtrack works on logical tracks with filter on extrac", {
    EMR_VTRACKS <<- list()
    EMR_FILTERS <<- list()
    withr::defer(clean_logical_tracks())

    emr_filter.create("f1", src="ph1", val=seq(4, 16, 1), keepref=TRUE)
    df <- emr_extract("ph1", names=c("value"), keepref=TRUE, filter="f1")
    emr_track.import("l1_ph", space="global", categorical=TRUE, src=df)
    emr_track.create_logical("l1", "ph1", seq(4, 16, 1))

    emr_filter.create("f1", src="l1_ph", val=c(15, 16))
    emr_filter.create("f2", src="l1", val=c(15, 16))

    emr_vtrack.create("vt_p", src="l1_ph")
    emr_vtrack.create("vt_l", src="l1")

    t1 <- emr_extract("vt_p", names=c("val"), filter="f1")
    t2 <- emr_extract("vt_l", names=c("val"), filter="f2")

    expect_equal(t1, t2)

    withr::defer(emr_vtrack.rm("vt_p"))
    withr::defer(emr_vtrack.rm("vt_l"))

    emr_filter.create("f1", src="l1_ph", val=c(5, 10, 30))
    emr_filter.create("f2", src="l1", val=c(5, 10, 30))
    emr_filter.create("f3", src="ph1", val=c(16))

    emr_vtrack.create("vt_p", src="l1_ph", func='closest', filter="f1")
    emr_vtrack.create("vt_l", src="l1", func='closest', filter="f2")

    t1 <- emr_extract("vt_p", names=c("val"), filter='f3')
    t2 <- emr_extract("vt_l", names=c("val"), filter='f3')

    expect_equal(t1, t2)

    withr::defer(emr_vtrack.rm("vt_p"))
    withr::defer(emr_vtrack.rm("vt_l"))

    emr_filter.create("f1", src="l1_ph", val=c(5, 10, 30))
    emr_filter.create("f2", src="l1", val=c(5, 10, 30))
    emr_filter.create("f3", src="ph1", val=c(16))

    emr_vtrack.create("vt_p", src="l1_ph", func='closest', filter="f1")
    emr_vtrack.create("vt_l", src="l1", func='closest', filter="f2")

    t1 <- emr_extract("vt_p", names=c("val"), filter='f3', keepref=TRUE)
    t2 <- emr_extract("vt_l", names=c("val"), filter='f3', keepref=TRUE)

    expect_equal(t1, t2)

    withr::defer(emr_track.rm("l1_ph", force = TRUE))
})

# emr_vtrack.attr.src

test_that("emr_vtrack.attr.src works with vtracks on logical tracks", {
    EMR_VTRACKS <<- list()
    withr::defer(clean_logical_tracks())

    emr_filter.create("f1", src="ph1", val=seq(4, 16, 1), keepref=TRUE)
    df <- emr_extract("ph1", names=c("value"), keepref=TRUE, filter="f1")
    emr_track.import("l2_ph", space="global", categorical=TRUE, src=df)

    emr_track.create_logical("l1", "ph1", c(15, 16))
    emr_track.create_logical("l2", "ph1", seq(4, 16, 1))

    emr_vtrack.create("vt1", src="l1")
    src <- emr_vtrack.attr.src("vt1")

    expect_equal(src, "l1")

    emr_vtrack.attr.src("vt1", "l2")
    src <- emr_vtrack.attr.src("vt1")

    expect_equal(src, "l2")

    withr::defer(emr_vtrack.rm("vt_1"))

    emr_vtrack.create("vt1", src="l1", func='value', params=16, keepref=TRUE)

    emr_vtrack.attr.src("vt1", "l2")
    t1 <- emr_extract("vt1")

    emr_vtrack.attr.src("vt1", "l2_ph")
    t2 <- emr_extract("vt1")

    expect_equal(t1, t2)

    withr::defer(emr_track.rm("l1_ph", force = TRUE))
    withr::defer(emr_track.rm("l2_ph", force = TRUE))
})

# emr_vtrack.attr.params

test_that("emr_vtrack.attr.params works on logical tracks", {
    EMR_VTRACKS <<- list()
    withr::defer(clean_logical_tracks())

    emr_filter.create("f1", src="ph1", val=seq(4, 16, 1), keepref=TRUE)
    df <- emr_extract("ph1", names=c("value"), keepref=TRUE, filter="f1")
    emr_track.import("l1_ph", space="global", categorical=TRUE, src=df)

    emr_track.create_logical("l1", "ph1", seq(4, 16, 1))

    emr_vtrack.create("vt_p", src="l1_ph", func='closest', params=c(15))
    emr_vtrack.create("vt_l", src="l1", func='closest', params=c(15))

    params <- emr_vtrack.attr.params("vt_l")

    expect_equal(params, c(15))

    p_params <- emr_vtrack.attr.params("vt_p")
    l_params <- emr_vtrack.attr.params("vt_l")

    expect_equal(p_params, l_params)

    emr_vtrack.create("vt_p1", src="l1_ph", func='closest', params=c(10, 16, 30))
    emr_vtrack.attr.params("vt_l", c(10, 16, 30))

    t1 <- emr_extract("vt_p1", names=c("val"))
    t2 <- emr_extract("vt_l", names=c("val"))

    expect_equal(t1, t2)

    withr::defer(emr_track.rm("l1_ph", force = TRUE))
})