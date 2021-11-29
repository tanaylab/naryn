load_test_dbs()

# At the beginning, track1 is in dbs 1, 2 and 4
# At the beginning, track7 is in dbs 1, 2, 3, 4

test_that("db.connect works with overlapping namespace", {
    expect_true("track1" %in% emr_track.ls())
    expect_equal(sum(emr_track.ls() == "track1"), 1)
    expect_true(emr_track.exists("track1", normalizePath(EMR_UROOT)))
})

test_that("emr_track.dbs works as expected", {
    expect_equal(emr_track.dbs("track0_1"), EMR_GROOT)
    expect_equal(emr_track.dbs("track0"), EMR_UROOT)
    expect_equal(emr_track.dbs("track7"), EMR_ROOTS)
    expect_equal(emr_track.dbs("track1"), EMR_ROOTS[-3])
})

test_that("deletion of overriding track loads back the overridden track", {
    expect_true("track1" %in% emr_track.ls())
    expect_true(emr_track.exists("track1", EMR_UROOT))

    emr_track.rm(track = "track1", force = TRUE)
    expect_false(emr_track.exists("track1", EMR_UROOT))

    expect_true("track1" %in% emr_track.ls())
    expect_true(emr_track.exists("track1", EMR_ROOTS[2]))

    emr_track.rm(track = "track1", force = TRUE)
    expect_false(emr_track.exists("track1", EMR_ROOTS[2]))

    expect_true("track1" %in% emr_track.ls())
    expect_true(emr_track.exists("track1", EMR_GROOT))

    emr_track.rm(track = "track1", force = TRUE)
    expect_false(emr_track.exists("track1", EMR_GROOT))

    expect_false("track1" %in% emr_track.ls())
})


test_that("emr_track.create overrides existing track", {

    # track2_2 is in db 2, we are creating a new track2_2 in EMR_UROOT
    expect_true("track2_2" %in% emr_track.ls())
    expect_true(emr_track.exists("track2_2", EMR_ROOTS[2]))
    t1 <- emr_extract("track2_2")

    emr_track.create(track = "track2_2", space = EMR_UROOT, categorical = FALSE, exp = "track2_2*2", keepref = TRUE, override = TRUE)
    withr::defer(emr_track.rm("track2_2", force = TRUE))

    expect_true("track2_2" %in% emr_track.ls())
    expect_true(emr_track.exists("track2_2", EMR_UROOT))
    expect_false(emr_track.exists("track2_2", EMR_ROOTS[2]))

    t2 <- emr_extract("track2_2")

    expect_equal(t2, t1 %>% dplyr::mutate(track2_2 = 2 * track2_2))
})


test_that("emr_track.import overrides existing track", {
    # track2_2 is in db 2, we are creating a new track2_2 in EMR_UROOT
    expect_true("track2_2" %in% emr_track.ls())
    expect_true(emr_track.exists("track2_2", EMR_ROOTS[2]))
    t1 <- emr_extract("track2_2")

    emr_track.import(
        track = "track2_2",
        space = EMR_UROOT,
        categorical = FALSE,
        src = t1 %>% dplyr::mutate(track2_2 = track2_2 * 2) %>% dplyr::rename(value = track2_2),
        override = TRUE
    )

    withr::defer(emr_track.rm("track2_2", force = TRUE))

    expect_true("track2_2" %in% emr_track.ls())
    expect_true(emr_track.exists("track2_2", EMR_UROOT))
    expect_false(emr_track.exists("track2_2", EMR_ROOTS[2]))

    t2 <- emr_extract("track2_2")

    expect_equal(t2, t1 %>% dplyr::mutate(track2_2 = 2 * track2_2), tolerance = 1e-6)
})

test_that("emr_track.create overrides existing track - not user dir", {
    # track2_2 is in db 2, we are creating a new track2_2 in db 3
    expect_true("track2_2" %in% emr_track.ls())
    expect_true(emr_track.exists("track2_2", EMR_ROOTS[2]))
    t1 <- emr_extract("track2_2")

    emr_track.create(track = "track2_2", space = EMR_ROOTS[3], categorical = FALSE, exp = "track2_2*2", keepref = TRUE, override = TRUE)
    withr::defer(emr_track.rm("track2_2", force = TRUE))

    expect_true("track2_2" %in% emr_track.ls())
    expect_true(emr_track.exists("track2_2", EMR_ROOTS[3]))
    expect_false(emr_track.exists("track2_2", EMR_ROOTS[2]))

    t2 <- emr_extract("track2_2")

    expect_equal(t2, t1 %>% dplyr::mutate(track2_2 = 2 * track2_2))
})

test_that("emr_track.import overrides existing track - not user dir", {
    # track2_2 is in db 2, we are creating a new track2_2 in db 3
    expect_true("track2_2" %in% emr_track.ls())
    expect_true(emr_track.exists("track2_2", EMR_ROOTS[2]))
    t1 <- emr_extract("track2_2")

    emr_track.import(
        track = "track2_2",
        space = EMR_ROOTS[3],
        categorical = FALSE,
        src = t1 %>% dplyr::mutate(track2_2 = track2_2 * 2) %>% dplyr::rename(value = track2_2),
        override = TRUE
    )

    withr::defer(emr_track.rm("track2_2", force = TRUE))

    expect_true("track2_2" %in% emr_track.ls())
    expect_true(emr_track.exists("track2_2", EMR_ROOTS[3]))
    expect_false(emr_track.exists("track2_2", EMR_ROOTS[2]))

    t2 <- emr_extract("track2_2")

    expect_equal(t2, t1 %>% dplyr::mutate(track2_2 = 2 * track2_2), tolerance = 1e-6)
})

test_that("overriding hierarchy on connect works as expected", {
    original_roots <- EMR_ROOTS

    emr_db.connect(original_roots[1])

    expect_true(emr_track.exists("track7"))
    expect_true(emr_track.exists("track7", original_roots[1]))
    expect_equal(emr_track.dbs("track7"), original_roots[1])

    emr_db.connect(original_roots[1:2])

    expect_true(emr_track.exists("track7"))
    expect_false(emr_track.exists("track7", original_roots[1]))
    expect_true(emr_track.exists("track7", original_roots[2]))
    expect_equal(emr_track.dbs("track7"), original_roots[1:2])

    emr_db.connect(original_roots[1:3])

    expect_true(emr_track.exists("track7"))
    expect_false(emr_track.exists("track7", original_roots[1]))
    expect_false(emr_track.exists("track7", original_roots[2]))
    expect_true(emr_track.exists("track7", original_roots[3]))
    expect_equal(emr_track.dbs("track7"), original_roots[1:3])

    emr_db.connect(original_roots[1:4])

    expect_true(emr_track.exists("track7"))
    expect_false(emr_track.exists("track7", original_roots[1]))
    expect_false(emr_track.exists("track7", original_roots[2]))
    expect_false(emr_track.exists("track7", original_roots[3]))
    expect_true(emr_track.exists("track7", original_roots[4]))
    expect_equal(emr_track.dbs("track7"), original_roots)
})

test_that("overriding mechanism works with mv, when a track is renamed it is no longer overriding/overridden", {

    # track7 is in all dbs, change its name in db4
    # it should allow the once overridden track7 in
    # db3 to be discoverable
    emr_track.mv("track7", "track7_4")

    expect_true(emr_track.exists("track7"))
    expect_true(emr_track.exists("track7_4"))

    expect_equal(emr_track.dbs("track7"), EMR_ROOTS[-4])

    emr_track.mv("track7", "track7_3")

    expect_true(emr_track.exists("track7"))
    expect_true(emr_track.exists("track7_3"))

    expect_equal(emr_track.dbs("track7"), EMR_ROOTS[-c(3, 4)])

    emr_track.mv("track7", "track7_2")

    expect_true(emr_track.exists("track7"))
    expect_true(emr_track.exists("track7_2"))

    expect_equal(emr_track.dbs("track7"), EMR_ROOTS[1])

    emr_track.mv("track7", "track7_1")

    # no more track 7
    expect_false(emr_track.exists("track7"))
    expect_true(emr_track.exists("track7_1"))

    expect_error(emr_track.dbs("track7"))
})

test_that("mv to override works as expected", {
    expect_error(emr_track.mv("track8_2", "track8_2"))
    expect_error(emr_track.mv("track8_2", "track8_3", EMR_ROOTS[3]))
    expect_error(emr_track.mv("track8_3", "track8_2", EMR_ROOTS[2]))

    emr_track.mv("track8_2", "track8_1")
    expect_equal(emr_track.dbs("track8_1"), EMR_ROOTS[1:2])

    emr_track.mv("track8_3", "track8_1")
    expect_equal(emr_track.dbs("track8_1"), EMR_ROOTS[1:3])

    # mv to reveal underlying track
    emr_track.mv("track8_1", "track8_3")
    expect_equal(emr_track.dbs("track8_1"), EMR_ROOTS[1:2])

    # mv two levels up
    emr_track.mv("track5_3", "track5_1")
    expect_equal(emr_track.dbs("track5_1"), EMR_ROOTS[c(1,3)])
})


test_that("read_only is also overridden when overriding a track", {
    expect_true(emr_track.exists("stam1_1", EMR_ROOTS[1]))
    expect_false(emr_track.readonly("stam1_1"))

    emr_track.readonly("stam1_1", readonly = TRUE)
    expect_true(emr_track.readonly("stam1_1"))

    emr_track.create(track = "stam1_1", space = EMR_UROOT, categorical = FALSE, keepref = FALSE, exp = "stam1_1", override = TRUE)

    expect_false(emr_track.exists("stam1_1", EMR_ROOTS[1]))
    expect_true(emr_track.exists("stam1_1", EMR_ROOTS[4]))

    expect_false(emr_track.readonly("stam1_1"))
    emr_track.rm("stam1_1", force = TRUE)

    expect_true(emr_track.readonly("stam1_1"))
})


test_that("emr_track.vars are overridden correctly", {
    expect_length(emr_track.var.ls("track2_2"), 0)
    emr_track.var.set("track2_2", "coffee_hours", c(8, 14, 17))
    expect_equal(emr_track.var.get("track2_2", "coffee_hours"), c(8, 14, 17))

    emr_track.create(track = "track2_2", space = EMR_UROOT, categorical = FALSE, exp = "track2_2*2", keepref = TRUE, override = TRUE)

    expect_length(emr_track.var.ls("track2_2"), 0)
    emr_track.var.set("track2_2", "coffee_hours", c(9, 15, 18))
    expect_equal(emr_track.var.get("track2_2", "coffee_hours"), c(9, 15, 18))

    emr_track.rm("track2_2", force = TRUE)

    expect_length(emr_track.var.ls("track2_2"), 1)
    expect_equal(emr_track.var.get("track2_2", "coffee_hours"), c(8, 14, 17))

    emr_track.create(track = "track2_2", space = EMR_UROOT, categorical = FALSE, exp = "track2_2*2", keepref = TRUE, override = TRUE)
    withr::defer(emr_track.rm("track2_2", force = TRUE))

    expect_length(emr_track.var.ls("track2_2"), 0)
})

test_that("emr_track.attrs are overridden correctly", {
    expect_equal(emr_track.attr.export(attr = "coffee") %>% nrow(), 0)

    emr_track.attr.set("track2_2", "coffee", "bad")
    emr_track.attr.set("track2", "coffee", "bad")

    expect_equal(emr_track.attr.export(attr = "coffee"), data.frame(track = c("track2", "track2_2"), attr = c("coffee", "coffee"), value = c("bad", "bad")))

    emr_track.create(track = "track2_2", space = EMR_UROOT, categorical = FALSE, exp = "track2_2*2", keepref = TRUE, override = TRUE)

    expect_equal(emr_track.attr.export(attr = "coffee"), data.frame(track = "track2", attr = "coffee", value = "bad"))
    emr_track.attr.set("track2_2", "coffee", "good")
    expect_equal(emr_track.attr.export(attr = "coffee"), data.frame(track = c("track2", "track2_2"), attr = c("coffee", "coffee"), value = c("bad", "good")))

    emr_track.rm("track2_2", force = TRUE)

    expect_equal(emr_track.attr.export(attr = "coffee"), data.frame(track = c("track2", "track2_2"), attr = c("coffee", "coffee"), value = c("bad", "bad")))
})

test_that("vtracks work on an overridden track, without changing source", {
    emr_vtrack.create(vtrack = "size", src = "track2_2", func = "size")
    emr_vtrack.create(vtrack = "avg", src = "track2_2", func = "avg")

    size1 <- emr_extract("size")
    avg1 <- emr_extract("avg")

    emr_track.create(track = "track2_2", space = EMR_UROOT, categorical = FALSE, exp = "track2_2*2", keepref = TRUE, override = TRUE)
    withr::defer(emr_track.rm("track2_2", force = TRUE))

    size2 <- emr_extract("size")
    avg2 <- emr_extract("avg")

    expect_equal(size1, size2)
    expect_equal(avg1 %>% dplyr::mutate(avg = avg * 2), avg2, tolerance = 1e-6)
})

test_that("filters work on an overridden track, without changing source", {
    emr_track.import(
        track = "kcart",
        space = EMR_GROOT,
        categorical = TRUE,
        src = data.frame(id = 14, time = emr_date2time(27, 12, 1952), value = 100),
    )

    withr::defer(emr_track.rm("kcart", force = TRUE))

    emr_filter.create(filter = "f1", src = "kcart", val = 100)
    expect_equal(emr_extract("kcart", filter = "f1") %>% nrow(), 1)

    emr_track.import(
        track = "kcart",
        space = EMR_UROOT,
        categorical = TRUE,
        src = data.frame(id = 14, time = emr_date2time(27, 12, 1952), value = 101),
        override = TRUE
    )

    withr::defer(emr_track.rm("kcart", force = TRUE))

    expect_equal(emr_extract("kcart", filter = "f1") %>% nrow(), 0)
})

test_that("subset works with overridden tracks", {
    set.seed(60427)
    all_ids <- emr_extract("track2_2") %>% dplyr::distinct(id)

    ids <- all_ids %>%
        dplyr::sample_frac(0.2) %>%
        dplyr::select(id)

    emr_db.subset(ids, fraction = 1, complementary = FALSE)

    withr::defer(emr_db.subset(NULL))

    expect_equal(
        emr_db.subset.info(),
        list(src = "<Ids Table>", fraction = 1, complementary = FALSE)
    )

    expect_equal(emr_db.subset.ids(), ids %>% dplyr::arrange(id))

    track2_2 <- emr_extract("track2_2")
    expect_true(all(track2_2$id %in% ids$id))

    # override track2_2 with new track, shift ids by the max val -1, leaving only one id in the intersection
    emr_track.import(
        track = "track2_2",
        space = EMR_UROOT,
        categorical = FALSE,
        src = track2_2 %>%
            dplyr::mutate(track2_2 = track2_2 * 2) %>%
            dplyr::rename(value = track2_2) %>%
            dplyr::mutate(id = id + (track2_2 %>% dplyr::pull("track2_2") %>% max()) - 1),
        override = TRUE
    )

    track2_2 <- emr_extract("track2_2")
    expect_true(!any(track2_2$id %in% ids$id))

    emr_track.rm("track2_2", force = TRUE)

    # reset the subset
    emr_db.subset(NULL)
    expect_null(emr_db.subset.info())
    expect_null(emr_db.subset.ids())

    a <- emr_extract("track2_2")
    expect_true(all(all_ids$id %in% a$id))
})

test_that("trying to override not explicitly throws an error", {
    expect_error(emr_track.create(
        track = "track2_2",
        space = EMR_UROOT,
        categorical = FALSE,
        exp = "track2_2*2",
        keepref = TRUE
    ))
})

test_that("trying to override a track in the same db throws an error", {
    expect_error(emr_track.create(
        track = "track2",
        space = EMR_UROOT,
        categorical = FALSE,
        exp = "track2*2",
        keepref = TRUE
    ))
})

test_that("trying to create a vtrack with the name of an overridden track throws an error", {
    emr_track.create(track = "track2_2", space = EMR_UROOT, categorical = FALSE, exp = "track2_2*2", keepref = TRUE, override = TRUE)
    withr::defer(emr_track.rm("track2_2", force = TRUE))
    expect_error(emr_vtrack.create("track2_2", "track2_2"))
})

test_that("trying to create a logical track with the name of an overridden track throws an error", {
    emr_track.create(track = "track2_2", space = EMR_UROOT, categorical = FALSE, exp = "track2_2*2", keepref = TRUE, override = TRUE)
    withr::defer(emr_track.rm("track2_2", force = TRUE))
    expect_error(emr_track.logical.create("track2_2", "track2_2"))
})

test_that("trying to connect with non unique dbs throws an error", {
    expect_error(emr_db.connect(c(EMR_UROOT, EMR_UROOT)))
})


test_that("emr_track.import throws error when trying to override existing track if created in lower order db", {

    # track2_2 is in db 2, we are creating a new track2_2 in db 1
    expect_true("track2_2" %in% emr_track.ls())
    expect_true(emr_track.exists("track2_2", EMR_ROOTS[2]))
    t1 <- emr_extract("track2_2")

    expect_error(emr_track.import(
        track = "track2_2",
        space = EMR_ROOTS[1],
        categorical = FALSE,
        src = t1 %>% dplyr::mutate(track2_2 = track2_2 * 2) %>% dplyr::rename(value = track2_2),
        override = TRUE
    ))
})

test_that("emr_track.create throws error when trying to override existing track if created in lower order db", {

    # track2_2 is in db 2, we are creating a new track2_2 in db 1
    expect_true("track2_2" %in% emr_track.ls())
    expect_true(emr_track.exists("track2_2", EMR_ROOTS[2]))
    t1 <- emr_extract("track2_2")

    expect_error(emr_track.create(track = "track2_2", space = EMR_ROOTS[1], categorical = FALSE, exp = "track2_2*2", keepref = TRUE, override = TRUE))
})

test_that("trying to override patients.dob throws error in any case", {
    expect_error(emr_track.mv("track4_1", "patients.dob"))
    expect_error(emr_track.mv("track4_1", "patients.dob", EMR_ROOTS[2]))
    expect_error(emr_track.create(
        track = "patients.dob", 
        space = EMR_ROOTS[3], 
        categorical = FALSE, 
        exp = "patients.dob*2", 
        keepref = TRUE, 
        override = TRUE)
        )
    expect_error(emr_track.import(
        track = "patients.dob",
        space = EMR_ROOTS[4],
        categorical = FALSE,
        src = "patients.dob*2",
        override = TRUE
    ))  
})


test_that("emr_ids_coverage works with multiple dbs", {
    expect_equal(
        emr_ids_coverage(data.frame(id = 0:200), c("track2_1", "track2_2")),
        c(track2_1 = 201L, track2_2 = 201L)
    )
})


test_that("emr_ids_coverage works with multiple dbs", {
    expect_equal(emr_ids_coverage(data.frame(id = 0:999), c("track2_1")), c(track2_1 = 1000L))
    track2_1 <- emr_extract("track2_1", names=c("value"))
    
    #originally, ids range 0-999, add 998 to ids, 0 becomes 998, and 1 becomes 999, ...
    emr_track.import(
        track = "track2_1",
        space = EMR_ROOTS[3],
        categorical = FALSE,
        src = track2_1 %>% dplyr::mutate(id = id + max(track2_1$id) - 1),
        override = TRUE
    )

    withr::defer(emr_track.rm("track2_1", force=TRUE))
    expect_equal(emr_ids_coverage(data.frame(id = 0:999), c("track2_1")), c(track2_1 = 2L))
})


test_that("emr_ids_coverage works with filter and overriding", {
    #track2 and track2_1 are the same - should result in full coverage
    expect_equal(emr_ids_coverage(data.frame(id = 0:999), c("track2"), filter="track2_1"), c(track2 = 1000L))
    track2_1 <- emr_extract("track2_1", names=c("value"))
    
    #originally, ids range 0-999, add 998 to ids, 0 becomes 998, and 1 becomes 999, ...
    emr_track.import(
        track = "track2_1",
        space = EMR_ROOTS[3],
        categorical = FALSE,
        src = track2_1 %>% dplyr::mutate(id = id + max(track2_1$id) - 1),
        override = TRUE
    )

    withr::defer(emr_track.rm("track2_1", force=TRUE))

    #track2_1 was overridden, noew coverage should change avcordingly
    expect_equal(emr_ids_coverage(data.frame(id = 0:999), c("track2"), filter="track2_1"), c(track2 = 2L))
})
