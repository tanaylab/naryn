load_test_dbs()

test_that("load_on_demand=FALSE loads tracks into shared memory", {
    cmd <- glue::glue("lsof -ad mem {file}", file = file.path(.naryn$EMR_UROOT, "track0.nrtrack"))
    # in the beginning - track0 is not loaded to memory
    expect_equal(system(cmd), 1)

    # reload the db which contains track0
    load_on_demand <- rep(TRUE, length(.naryn$EMR_ROOTS))
    load_on_demand[which(.naryn$EMR_ROOTS == emr_track.dbs("track0"))] <- FALSE
    emr_db.connect(.naryn$EMR_ROOTS, load_on_demand = load_on_demand)

    # now the track should be in shared memory
    expect_equal(system(cmd), 0)
})

test_that("emr_db.connect recycles load_on_demand parameter", {
    emr_db.connect(.naryn$EMR_ROOTS, load_on_demand = TRUE)
    expect_error(emr_db.connect(.naryn$EMR_ROOTS, load_on_demand = c(TRUE, TRUE)))
})

test_that("emr_db.connect fails when load_on_demand is not logical", {
    expect_error(emr_db.connect(.naryn$EMR_ROOTS, load_on_demand = "savta"))
})


test_that("emr_track.ls works with multiple dbs", {
    expect_equal(
        emr_track.ls(),
        c(
            "patients.dob", "ph1", "ph1_3", "physical_track_subset_15",
            "physical_track_subset_15_3", "stam1", "stam1_1", "stam1_2",
            "stam1_3", "track0", "track0_1", "track0_2", "track0_3", "track0_sparse",
            "track0_sparse_1", "track0_sparse_2", "track0_sparse_3", "track1",
            "track1_3", "track1_sparse", "track1_sparse_1", "track1_sparse_2",
            "track1_sparse_3", "track2", "track2_1", "track2_2", "track2_3",
            "track2_sparse", "track2_sparse_1", "track2_sparse_2", "track2_sparse_3",
            "track3", "track3_1", "track3_2", "track3_3", "track4", "track4_1",
            "track4_2", "track4_3", "track4_sparse", "track4_sparse_1", "track4_sparse_2",
            "track4_sparse_3", "track5", "track5_1", "track5_2", "track5_3",
            "track5_sparse", "track5_sparse_1", "track5_sparse_2", "track5_sparse_3",
            "track6", "track6_1", "track6_2", "track6_3", "track7", "track7_sparse",
            "track7_sparse_1", "track7_sparse_2", "track7_sparse_3", "track8",
            "track8_1", "track8_2", "track8_3", "track8_sparse", "track8_sparse_1",
            "track8_sparse_2", "track8_sparse_3"
        )
    )
})

test_that("emr_track.ls works with db_id", {
    ls_tracks <- emr_track.ls(db_id = .naryn$EMR_ROOTS[2])
    tracks <- gsub(".nrtrack$", "", list.files(.naryn$EMR_ROOTS[2], pattern = ".nrtrack"))

    # remove overridden tracks
    files_tracks <- purrr::discard(tracks, ~ {
        dbs <- emr_track.dbs(.x)
        dbs[length(dbs)] != .naryn$EMR_ROOTS[2]
    })

    expect_setequal(ls_tracks, files_tracks)
})

test_that("emr_track.ls works with db_id which is the global db", {
    ls_tracks <- emr_track.ls(db_id = .naryn$EMR_GROOT)
    tracks <- gsub(".nrtrack$", "", list.files(.naryn$EMR_GROOT, pattern = ".nrtrack"))

    # remove overridden tracks
    files_tracks <- purrr::discard(tracks, ~ {
        dbs <- emr_track.dbs(.x)
        dbs[length(dbs)] != .naryn$EMR_GROOT
    })

    expect_setequal(ls_tracks, files_tracks)
})

test_that("emr_track.ls works with db_id which is the user db", {
    ls_tracks <- emr_track.ls(db_id = .naryn$EMR_UROOT)
    tracks <- gsub(".nrtrack$", "", list.files(.naryn$EMR_UROOT, pattern = ".nrtrack"))

    # remove overridden tracks
    files_tracks <- purrr::discard(tracks, ~ {
        dbs <- emr_track.dbs(.x)
        dbs[length(dbs)] != .naryn$EMR_UROOT
    })

    expect_setequal(ls_tracks, files_tracks)
})

# At the beginning, track1 is in dbs 1, 2 and 4
# At the beginning, track7 is in dbs 1, 2, 3, 4

test_that("emr_track.dbs works", {
    track_dbs <- c(
        track1 = .naryn$EMR_ROOTS[1],
        track1 = .naryn$EMR_ROOTS[2],
        track1 = .naryn$EMR_ROOTS[4],
        track7 = .naryn$EMR_ROOTS[1],
        track7 = .naryn$EMR_ROOTS[2],
        track7 = .naryn$EMR_ROOTS[3],
        track7 = .naryn$EMR_ROOTS[4],
        track0_1 = .naryn$EMR_ROOTS[1]
    )
    expect_equal(emr_track.dbs(c("track1", "track7", "track0_1")), track_dbs)
    expect_equal(
        emr_track.dbs(c("track1", "track7", "track0_1"), dataframe = TRUE),
        as.data.frame(tibble::enframe(track_dbs, "track", "db"))
    )

    # a single track
    expect_equal(emr_track.dbs("track0_1"), track_dbs[8])
    expect_equal(
        emr_track.dbs("track0_1", dataframe = TRUE),
        as.data.frame(tibble::enframe(track_dbs[8], "track", "db"))
    )
})

test_that("emr_track.current_db works", {
    track_dbs <- c(
        track1 = .naryn$EMR_ROOTS[4],
        track7 = .naryn$EMR_ROOTS[4],
        track0_2 = .naryn$EMR_ROOTS[2]
    )
    expect_equal(emr_track.current_db(c("track1", "track7", "track0_2")), track_dbs)
    expect_equal(
        emr_track.current_db(c("track1", "track7", "track0_2"), dataframe = TRUE),
        as.data.frame(tibble::enframe(track_dbs, "track", "db"))
    )

    # a single track
    expect_equal(emr_track.current_db("track1"), track_dbs[1])
    expect_equal(
        emr_track.current_db("track1", dataframe = TRUE),
        as.data.frame(tibble::enframe(track_dbs[1], "track", "db"))
    )
})

test_that("db.connect works with overlapping namespace", {
    expect_true("track1" %in% emr_track.ls())
    expect_equal(sum(emr_track.ls() == "track1"), 1)
    expect_true(emr_track.exists("track1", normalizePath(.naryn$EMR_UROOT)))
})

test_that("emr_track.dbs works as expected", {
    expect_equal(emr_track.dbs("track0_1"), .naryn$EMR_GROOT, ignore_attr = TRUE)
    expect_equal(emr_track.dbs("track0"), .naryn$EMR_UROOT, ignore_attr = TRUE)
    expect_equal(emr_track.dbs("track7"), .naryn$EMR_ROOTS, ignore_attr = TRUE)
    expect_equal(emr_track.dbs("track1"), .naryn$EMR_ROOTS[-3], ignore_attr = TRUE)
})

test_that("deletion of overriding track loads back the overridden track", {
    expect_true("track1" %in% emr_track.ls())
    expect_true(emr_track.exists("track1", .naryn$EMR_UROOT))

    emr_track.rm(track = "track1", force = TRUE)
    expect_false(emr_track.exists("track1", .naryn$EMR_UROOT))

    expect_true("track1" %in% emr_track.ls())
    expect_true(emr_track.exists("track1", .naryn$EMR_ROOTS[2]))

    emr_track.rm(track = "track1", force = TRUE)
    expect_false(emr_track.exists("track1", .naryn$EMR_ROOTS[2]))

    expect_true("track1" %in% emr_track.ls())
    expect_true(emr_track.exists("track1", .naryn$EMR_GROOT))

    emr_track.rm(track = "track1", force = TRUE)
    expect_false(emr_track.exists("track1", .naryn$EMR_GROOT))

    expect_false("track1" %in% emr_track.ls())
})

test_that("emr_track.create overrides existing track", {
    # track2_2 is in db 2, we are creating a new track2_2 in EMR_UROOT
    expect_true("track2_2" %in% emr_track.ls())
    expect_true(emr_track.exists("track2_2", .naryn$EMR_ROOTS[2]))
    t1 <- emr_extract("track2_2")

    emr_track.create(track = "track2_2", space = .naryn$EMR_UROOT, categorical = FALSE, exp = "track2_2*2", keepref = TRUE, override = TRUE)
    withr::defer(emr_track.rm("track2_2", force = TRUE))

    expect_true("track2_2" %in% emr_track.ls())
    expect_true(emr_track.exists("track2_2", .naryn$EMR_UROOT))
    expect_false(emr_track.exists("track2_2", .naryn$EMR_ROOTS[2]))

    t2 <- emr_extract("track2_2")

    expect_equal(t2, t1 %>% dplyr::mutate(track2_2 = 2 * track2_2))
})


test_that("emr_track.import overrides existing track", {
    # track2_2 is in db 2, we are creating a new track2_2 in EMR_UROOT
    expect_true("track2_2" %in% emr_track.ls())
    expect_true(emr_track.exists("track2_2", .naryn$EMR_ROOTS[2]))
    t1 <- emr_extract("track2_2")

    emr_track.import(
        track = "track2_2",
        space = .naryn$EMR_UROOT,
        categorical = FALSE,
        src = t1 %>% dplyr::mutate(track2_2 = track2_2 * 2) %>% dplyr::rename(value = track2_2),
        override = TRUE
    )

    withr::defer(emr_track.rm("track2_2", force = TRUE))

    expect_true("track2_2" %in% emr_track.ls())
    expect_true(emr_track.exists("track2_2", .naryn$EMR_UROOT))
    expect_false(emr_track.exists("track2_2", .naryn$EMR_ROOTS[2]))

    t2 <- emr_extract("track2_2")

    expect_equal(t2, t1 %>% dplyr::mutate(track2_2 = 2 * track2_2), tolerance = 1e-6)
})

test_that("emr_track.create overrides existing track - not user dir", {
    # track2_2 is in db 2, we are creating a new track2_2 in db 3
    expect_true("track2_2" %in% emr_track.ls())
    expect_true(emr_track.exists("track2_2", .naryn$EMR_ROOTS[2]))
    t1 <- emr_extract("track2_2")

    emr_track.create(track = "track2_2", space = .naryn$EMR_ROOTS[3], categorical = FALSE, exp = "track2_2*2", keepref = TRUE, override = TRUE)
    withr::defer(emr_track.rm("track2_2", force = TRUE))

    expect_true("track2_2" %in% emr_track.ls())
    expect_true(emr_track.exists("track2_2", .naryn$EMR_ROOTS[3]))
    expect_false(emr_track.exists("track2_2", .naryn$EMR_ROOTS[2]))

    t2 <- emr_extract("track2_2")

    expect_equal(t2, t1 %>% dplyr::mutate(track2_2 = 2 * track2_2))
})

test_that("emr_track.import overrides existing track - not user dir", {
    # track2_2 is in db 2, we are creating a new track2_2 in db 3
    expect_true("track2_2" %in% emr_track.ls())
    expect_true(emr_track.exists("track2_2", .naryn$EMR_ROOTS[2]))
    t1 <- emr_extract("track2_2")

    emr_track.import(
        track = "track2_2",
        space = .naryn$EMR_ROOTS[3],
        categorical = FALSE,
        src = t1 %>% dplyr::mutate(track2_2 = track2_2 * 2) %>% dplyr::rename(value = track2_2),
        override = TRUE
    )

    withr::defer(emr_track.rm("track2_2", force = TRUE))

    expect_true("track2_2" %in% emr_track.ls())
    expect_true(emr_track.exists("track2_2", .naryn$EMR_ROOTS[3]))
    expect_false(emr_track.exists("track2_2", .naryn$EMR_ROOTS[2]))

    t2 <- emr_extract("track2_2")

    expect_equal(t2, t1 %>% dplyr::mutate(track2_2 = 2 * track2_2), tolerance = 1e-6)
})

test_that("overriding hierarchy on connect works as expected", {
    original_roots <- .naryn$EMR_ROOTS

    emr_db.connect(original_roots[1])

    expect_true(emr_track.exists("track7"))
    expect_true(emr_track.exists("track7", original_roots[1]))
    expect_equal(emr_track.dbs("track7"), original_roots[1], ignore_attr = TRUE)

    emr_db.connect(original_roots[1:2])

    expect_true(emr_track.exists("track7"))
    expect_false(emr_track.exists("track7", original_roots[1]))
    expect_true(emr_track.exists("track7", original_roots[2]))
    expect_equal(emr_track.dbs("track7"), original_roots[1:2], ignore_attr = TRUE)

    emr_db.connect(original_roots[1:3])

    expect_true(emr_track.exists("track7"))
    expect_false(emr_track.exists("track7", original_roots[1]))
    expect_false(emr_track.exists("track7", original_roots[2]))
    expect_true(emr_track.exists("track7", original_roots[3]))
    expect_equal(emr_track.dbs("track7"), original_roots[1:3], ignore_attr = TRUE)

    emr_db.connect(original_roots[1:4])

    expect_true(emr_track.exists("track7"))
    expect_false(emr_track.exists("track7", original_roots[1]))
    expect_false(emr_track.exists("track7", original_roots[2]))
    expect_false(emr_track.exists("track7", original_roots[3]))
    expect_true(emr_track.exists("track7", original_roots[4]))
    expect_equal(emr_track.dbs("track7"), original_roots, ignore_attr = TRUE)
})

test_that("overriding mechanism works with mv, when a track is renamed it is no longer overriding/overridden", {
    # track7 is in all dbs, change its name in db4
    # it should allow the once overridden track7 in
    # db3 to be discoverable
    emr_track.mv("track7", "track7_4")

    expect_true(emr_track.exists("track7"))
    expect_true(emr_track.exists("track7_4"))

    expect_equal(emr_track.dbs("track7"), .naryn$EMR_ROOTS[-4], ignore_attr = TRUE)

    emr_track.mv("track7", "track7_3")

    expect_true(emr_track.exists("track7"))
    expect_true(emr_track.exists("track7_3"))

    expect_equal(emr_track.dbs("track7"), .naryn$EMR_ROOTS[-c(3, 4)], ignore_attr = TRUE)

    emr_track.mv("track7", "track7_2")

    expect_true(emr_track.exists("track7"))
    expect_true(emr_track.exists("track7_2"))

    expect_equal(emr_track.dbs("track7"), .naryn$EMR_ROOTS[1], ignore_attr = TRUE)

    emr_track.mv("track7", "track7_1")

    # no more track 7
    expect_false(emr_track.exists("track7"))
    expect_true(emr_track.exists("track7_1"))

    expect_error(emr_track.dbs("track7"))
})

test_that("mv to override works as expected", {
    expect_error(emr_track.mv("track8_2", "track8_2"))
    expect_error(emr_track.mv("track8_2", "track8_3", .naryn$EMR_ROOTS[3]))
    expect_error(emr_track.mv("track8_3", "track8_2", .naryn$EMR_ROOTS[2]))

    emr_track.mv("track8_2", "track8_1")
    expect_equal(emr_track.dbs("track8_1"), .naryn$EMR_ROOTS[1:2], ignore_attr = TRUE)

    emr_track.mv("track8_3", "track8_1")
    expect_equal(emr_track.dbs("track8_1"), .naryn$EMR_ROOTS[1:3], ignore_attr = TRUE)

    # mv to reveal underlying track
    emr_track.mv("track8_1", "track8_3")
    expect_equal(emr_track.dbs("track8_1"), .naryn$EMR_ROOTS[1:2], ignore_attr = TRUE)

    # mv two levels up
    emr_track.mv("track5_3", "track5_1")
    expect_equal(emr_track.dbs("track5_1"), .naryn$EMR_ROOTS[c(1, 3)], ignore_attr = TRUE)
})


test_that("read_only is also overridden when overriding a track", {
    expect_true(emr_track.exists("stam1_1", .naryn$EMR_ROOTS[1]))
    expect_false(emr_track.readonly("stam1_1"))

    emr_track.readonly("stam1_1", readonly = TRUE)
    expect_true(emr_track.readonly("stam1_1"))

    emr_track.create(track = "stam1_1", space = .naryn$EMR_UROOT, categorical = FALSE, keepref = FALSE, exp = "stam1_1", override = TRUE)

    expect_false(emr_track.exists("stam1_1", .naryn$EMR_ROOTS[1]))
    expect_true(emr_track.exists("stam1_1", .naryn$EMR_ROOTS[4]))

    expect_false(emr_track.readonly("stam1_1"))
    emr_track.rm("stam1_1", force = TRUE)

    expect_true(emr_track.readonly("stam1_1"))
})


test_that("emr_track.vars are overridden correctly", {
    expect_length(emr_track.var.ls("track2_2"), 0)
    emr_track.var.set("track2_2", "coffee_hours", c(8, 14, 17))
    expect_equal(emr_track.var.get("track2_2", "coffee_hours"), c(8, 14, 17))

    emr_track.create(track = "track2_2", space = .naryn$EMR_UROOT, categorical = FALSE, exp = "track2_2*2", keepref = TRUE, override = TRUE)

    expect_length(emr_track.var.ls("track2_2"), 0)
    emr_track.var.set("track2_2", "coffee_hours", c(9, 15, 18))
    expect_equal(emr_track.var.get("track2_2", "coffee_hours"), c(9, 15, 18))

    emr_track.rm("track2_2", force = TRUE)

    expect_length(emr_track.var.ls("track2_2"), 1)
    expect_equal(emr_track.var.get("track2_2", "coffee_hours"), c(8, 14, 17))

    emr_track.create(track = "track2_2", space = .naryn$EMR_UROOT, categorical = FALSE, exp = "track2_2*2", keepref = TRUE, override = TRUE)
    withr::defer(emr_track.rm("track2_2", force = TRUE))

    expect_length(emr_track.var.ls("track2_2"), 0)
})

test_that("emr_track.attrs are overridden correctly", {
    withr::defer(clean_attributes())
    expect_equal(emr_track.attr.export(attr = "coffee") %>% nrow(), 0)

    emr_track.attr.set("track2_2", "coffee", "bad")
    emr_track.attr.set("track2", "coffee", "bad")

    expect_equal(emr_track.attr.export(attr = "coffee"), data.frame(track = c("track2", "track2_2"), attr = c("coffee", "coffee"), value = c("bad", "bad")))

    emr_track.create(track = "track2_2", space = .naryn$EMR_UROOT, categorical = FALSE, exp = "track2_2*2", keepref = TRUE, override = TRUE)

    expect_equal(emr_track.attr.export(attr = "coffee"), data.frame(track = "track2", attr = "coffee", value = "bad"))
    emr_track.attr.set("track2_2", "coffee", "good")
    expect_equal(emr_track.attr.export(attr = "coffee"), data.frame(track = c("track2", "track2_2"), attr = c("coffee", "coffee"), value = c("bad", "good")))

    emr_track.rm("track2_2", force = TRUE)

    expect_equal(emr_track.attr.export(attr = "coffee"), data.frame(track = c("track2", "track2_2"), attr = c("coffee", "coffee"), value = c("bad", "bad")))
})

test_that("emr_track.attrs created correctly in emr_track.attr.set batch mode", {
    withr::defer(clean_attributes())
    emr_track.attr.set(c("track2_2", "track2", "track1_3"), rep("coffee", 3), rep("bad", 3))

    expect_equal(emr_track.attr.export(attr = "coffee"), data.frame(track = c("track1_3", "track2", "track2_2"), attr = rep("coffee", 3), value = rep("bad", 3)))

    emr_track.attr.rm(c("track2_2", "track2", "track1_3"), "coffee")
    expect_equal(emr_track.attr.export(attr = "coffee"), data.frame(track = character(0), attr = character(0), value = character(0)))
})

test_that("vtracks work on an overridden track, without changing source", {
    emr_vtrack.create(vtrack = "size", src = "track2_2", func = "size")
    emr_vtrack.create(vtrack = "avg", src = "track2_2", func = "avg")

    size1 <- emr_extract("size")
    avg1 <- emr_extract("avg")

    emr_track.create(track = "track2_2", space = .naryn$EMR_UROOT, categorical = FALSE, exp = "track2_2*2", keepref = TRUE, override = TRUE)
    withr::defer(emr_track.rm("track2_2", force = TRUE))

    size2 <- emr_extract("size")
    avg2 <- emr_extract("avg")

    expect_equal(size1, size2)
    expect_equal(avg1 %>% dplyr::mutate(avg = avg * 2), avg2, tolerance = 1e-6)
})

test_that("filters work on an overridden track, without changing source", {
    emr_track.import(
        track = "kcart",
        space = .naryn$EMR_GROOT,
        categorical = TRUE,
        src = data.frame(id = 14, time = emr_date2time(27, 12, 1952), value = 100),
    )

    withr::defer(emr_track.rm("kcart", force = TRUE))

    emr_filter.create(filter = "f1", src = "kcart", val = 100)
    expect_equal(emr_extract("kcart", filter = "f1") %>% nrow(), 1)

    emr_track.import(
        track = "kcart",
        space = .naryn$EMR_UROOT,
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
        space = .naryn$EMR_UROOT,
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
        space = .naryn$EMR_UROOT,
        categorical = FALSE,
        exp = "track2_2*2",
        keepref = TRUE
    ))
})

test_that("trying to override a track in the same db throws an error", {
    expect_error(emr_track.create(
        track = "track2",
        space = .naryn$EMR_UROOT,
        categorical = FALSE,
        exp = "track2*2",
        keepref = TRUE
    ))
})

test_that("trying to create a vtrack with the name of an overridden track throws an error", {
    emr_track.create(track = "track2_2", space = .naryn$EMR_UROOT, categorical = FALSE, exp = "track2_2*2", keepref = TRUE, override = TRUE)
    withr::defer(emr_track.rm("track2_2", force = TRUE))
    expect_error(emr_vtrack.create("track2_2", "track2_2"))
})

test_that("trying to create a logical track with the name of an overridden track throws an error", {
    emr_track.create(track = "track2_2", space = .naryn$EMR_UROOT, categorical = FALSE, exp = "track2_2*2", keepref = TRUE, override = TRUE)
    withr::defer(emr_track.rm("track2_2", force = TRUE))
    expect_error(emr_track.logical.create("track2_2", "track2_2"))
})

test_that("trying to connect with non unique dbs throws an error", {
    expect_error(emr_db.connect(c(.naryn$EMR_UROOT, .naryn$EMR_UROOT)))
})


test_that("emr_track.import throws error when trying to override existing track if created in lower order db", {
    # track2_2 is in db 2, we are creating a new track2_2 in db 1
    expect_true("track2_2" %in% emr_track.ls())
    expect_true(emr_track.exists("track2_2", .naryn$EMR_ROOTS[2]))
    t1 <- emr_extract("track2_2")

    expect_error(emr_track.import(
        track = "track2_2",
        space = .naryn$EMR_ROOTS[1],
        categorical = FALSE,
        src = t1 %>% dplyr::mutate(track2_2 = track2_2 * 2) %>% dplyr::rename(value = track2_2),
        override = TRUE
    ))
})

test_that("emr_track.create throws error when trying to override existing track if created in lower order db", {
    # track2_2 is in db 2, we are creating a new track2_2 in db 1
    expect_true("track2_2" %in% emr_track.ls())
    expect_true(emr_track.exists("track2_2", .naryn$EMR_ROOTS[2]))
    t1 <- emr_extract("track2_2")

    expect_error(emr_track.create(track = "track2_2", space = .naryn$EMR_ROOTS[1], categorical = FALSE, exp = "track2_2*2", keepref = TRUE, override = TRUE))
})

test_that("trying to override patients.dob throws error in any case", {
    expect_error(emr_track.mv("track4_1", "patients.dob"))
    expect_error(emr_track.mv("track4_1", "patients.dob", .naryn$EMR_ROOTS[2]))
    expect_error(emr_track.create(
        track = "patients.dob",
        space = .naryn$EMR_ROOTS[3],
        categorical = FALSE,
        exp = "patients.dob*2",
        keepref = TRUE,
        override = TRUE
    ))
    expect_error(emr_track.import(
        track = "patients.dob",
        space = .naryn$EMR_ROOTS[4],
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
    track2_1 <- emr_extract("track2_1", names = c("value"))

    # originally, ids range 0-999, add 998 to ids, 0 becomes 998, and 1 becomes 999, ...
    emr_track.import(
        track = "track2_1",
        space = .naryn$EMR_ROOTS[3],
        categorical = FALSE,
        src = track2_1 %>% dplyr::mutate(id = id + max(track2_1$id) - 1),
        override = TRUE
    )

    withr::defer(emr_track.rm("track2_1", force = TRUE))
    expect_equal(emr_ids_coverage(data.frame(id = 0:999), c("track2_1")), c(track2_1 = 2L))
})


test_that("emr_ids_coverage works with filter and overriding", {
    # track2 and track2_1 are the same - should result in full coverage
    expect_equal(emr_ids_coverage(data.frame(id = 0:999), c("track2"), filter = "track2_1"), c(track2 = 1000L))
    track2_1 <- emr_extract("track2_1", names = c("value"))

    # originally, ids range 0-999, add 998 to ids, 0 becomes 998, and 1 becomes 999, ...
    emr_track.import(
        track = "track2_1",
        space = .naryn$EMR_ROOTS[3],
        categorical = FALSE,
        src = track2_1 %>% dplyr::mutate(id = id + max(track2_1$id) - 1),
        override = TRUE
    )

    withr::defer(emr_track.rm("track2_1", force = TRUE))

    # track2_1 was overridden, new coverage should change accordingly
    expect_equal(emr_ids_coverage(data.frame(id = 0:999), c("track2"), filter = "track2_1"), c(track2 = 2L))
})

test_that("creating logical tracks happens only on the global db", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("l1", "track0_1")
    expect_true(logical_track_ok("l1", "track0_1"))
})

test_that("logical tracks on non-global db are not shown at emr_track.ls", {
    withr::defer(clean_logical_tracks())
    emr_track.logical.create("l1", "track0_1")
    old_roots <- .naryn$EMR_ROOTS
    withr::defer(emr_db.connect(old_roots))
    new_roots <- .naryn$EMR_ROOTS[c(4, 2, 3, 1)]
    emr_db.connect(new_roots)
    expect_length(emr_track.ls("l1"), 0)
    emr_db.connect(old_roots)
    expect_equal(emr_track.ls("l1"), "l1")
})


test_that("cannot create a logical track pointing to non-global db track", {
    withr::defer(clean_logical_tracks())
    df <- data.frame(id = 1, time = c(1, 2, 2), value = c(-1, 4, 3), ref = c(0, 0, 1))
    emr_track.import("tmp", space = "user", categorical = TRUE, src = df)
    withr::defer(emr_track.rm("tmp", force = TRUE))
    expect_error(emr_track.logical.create("ltmp", "tmp"))
})


test_that("emr_db.connect fails when directory doesn't have read permissions", {
    prev_roots <- .naryn$EMR_ROOTS
    db <- copy_test_db(.naryn$EMR_ROOTS[1])
    system(glue::glue("chmod a-r {db}"))
    withr::defer({
        system(glue::glue("chmod a+r {db}"))
        emr_db.connect(prev_roots)
    })
    expect_error(emr_db.connect(db))
})

test_that("emr_db.connect fails when directory doesn't have search permissions", {
    prev_roots <- .naryn$EMR_ROOTS
    db <- copy_test_db(.naryn$EMR_ROOTS[1])
    system(glue::glue("chmod a-x {db}"))
    withr::defer({
        system(glue::glue("chmod a+x {db}"))
        emr_db.connect(prev_roots)
    })
    expect_error(emr_db.connect(db))
})

test_that("emr_db.connect fails when db_id is a file instead of directory", {
    prev_roots <- .naryn$EMR_ROOTS
    fn <- tempfile()
    file.create(fn)
    expect_error(emr_db.connect(fn))
    withr::defer({
        emr_db.connect(prev_roots)
    })
})

test_that("emr_db.connect fails when .naryn doesn't have read permissions", {
    prev_roots <- .naryn$EMR_ROOTS
    db <- copy_test_db(.naryn$EMR_ROOTS[1])
    file.create(file.path(db, ".naryn"))
    system(glue::glue("chmod a-r {db}/.naryn"))
    withr::defer({
        system(glue::glue("chmod a+r {db}/.naryn"))
        emr_db.connect(prev_roots)
    })
    expect_error(emr_db.connect(db))
})
