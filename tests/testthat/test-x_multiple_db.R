load_test_dbs()

# At the beginning, track1 is in dbs 1, 2 and 4
# At the beginning, track7 is in dbs 1, 2, 3, 4

test_that("db.connect works with overlapping namespace", {
    expect_true("track1" %in% emr_track.ls())
    expect_equal(sum(emr_track.ls() == "track1"), 1)
    expect_true(emr_track.exists("track1", normalizePath(EMR_UROOT))) 
})

test_that("deletion of overriding track loads back the overridden track", {
    expect_true("track1" %in% emr_track.ls())
    expect_true(emr_track.exists("track1", EMR_UROOT)) 

    emr_track.rm(track="track1", force=TRUE)
    expect_false(emr_track.exists("track1", EMR_UROOT)) 

    expect_true("track1" %in% emr_track.ls())
    expect_true(emr_track.exists("track1", EMR_ROOTS[2])) 

    emr_track.rm(track="track1", force=TRUE)
    expect_false(emr_track.exists("track1", EMR_ROOTS[2]))

    expect_true("track1" %in% emr_track.ls())
    expect_true(emr_track.exists("track1", EMR_GROOT))

    emr_track.rm(track="track1", force=TRUE)
    expect_false(emr_track.exists("track1", EMR_GROOT))

    expect_false("track1" %in% emr_track.ls())
})


test_that("creation of track overrides existing track", {
    
    # track2_2 is in db 2, we are creating a new track2_2 in EMR_UROOT
    expect_true("track2_2" %in% emr_track.ls())
    expect_true(emr_track.exists("track2_2", EMR_ROOTS[2]))
    t1 <- emr_extract("track2_2")

    emr_track.create(track="track2_2", space=EMR_UROOT, categorical=FALSE, exp="track2_2*2", keepref=TRUE, override=TRUE)

    expect_true("track2_2" %in% emr_track.ls())
    expect_true(emr_track.exists("track2_2", EMR_UROOT))
    expect_false(emr_track.exists("track2_2", EMR_ROOTS[2]))

    t2 <- emr_extract("track2_2")
    
    expect_equal(t2, t1 %>% dplyr::mutate(track2_2 = 2*track2_2))
})


test_that("overriding hierarchy on connect works as expected", {

    original_roots <- EMR_ROOTS

    emr_db.connect(original_roots[1])
    
    expect_true(emr_track.exists("track7"))
    expect_true(emr_track.exists("track7", original_roots[1]))

    emr_db.connect(original_roots[1:2])

    expect_true(emr_track.exists("track7"))
    expect_false(emr_track.exists("track7", original_roots[1]))
    expect_true(emr_track.exists("track7", original_roots[2]))

    emr_db.connect(original_roots[1:3])

    expect_true(emr_track.exists("track7"))
    expect_false(emr_track.exists("track7", original_roots[1]))
    expect_false(emr_track.exists("track7", original_roots[2]))
    expect_true(emr_track.exists("track7", original_roots[3]))
    
    emr_db.connect(original_roots[1:4])

    expect_true(emr_track.exists("track7"))
    expect_false(emr_track.exists("track7", original_roots[1]))
    expect_false(emr_track.exists("track7", original_roots[2]))
    expect_false(emr_track.exists("track7", original_roots[3]))
    expect_true(emr_track.exists("track7", original_roots[4]))

})

test_that("overriding mechanism works wiht mv, when a track is renamed it is no longer overriding/overridden", {

    # track7 is in all dbs, change its name in db4
    # it should allow the once overridden track7 in 
    # db3 to be discoverable 
    emr_track.mv("track7", "track7_4")

    expect_true(emr_track.exists("track7"))
    expect_true(emr_track.exists("track7_4"))

    emr_track.mv("track7", "track7_3")

    expect_true(emr_track.exists("track7"))
    expect_true(emr_track.exists("track7_3"))

    emr_track.mv("track7", "track7_2")

    expect_true(emr_track.exists("track7"))
    expect_true(emr_track.exists("track7_2"))

    emr_track.mv("track7", "track7_1")

    # no more track 7
    expect_false(emr_track.exists("track7"))
    expect_true(emr_track.exists("track7_1"))
})


test_that("read_only is also overridden when overriding a track", {

    expect_true(emr_track.exists("stam1_1", EMR_ROOTS[1]))
    expect_false(emr_track.readonly("stam1_1"))

    emr_track.readonly("stam1_1", readonly = TRUE)
    expect_true(emr_track.readonly("stam1_1"))

    emr_track.create(track="stam1_1", space=EMR_UROOT, categorical=FALSE, keepref=F, exp="stam1_1", override=TRUE)

    expect_false(emr_track.exists("stam1_1", EMR_ROOTS[1]))
    expect_true(emr_track.exists("stam1_1", EMR_ROOTS[4]))

    expect_false(emr_track.readonly("stam1_1"))
    emr_track.rm("stam1_1", force=TRUE)

    expect_true(emr_track.readonly("stam1_1"))

})

test_that("", {
    
})