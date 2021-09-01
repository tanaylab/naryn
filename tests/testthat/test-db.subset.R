test_that("emr_db.subset works with ids data frame", {
    set.seed(60427)
    all_ids <- emr_extract("track1") %>%
        dplyr::distinct(id)
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

    a <- emr_extract("track1")
    expect_true(all(a$id %in% ids$id))

    # reset the subset
    emr_db.subset(NULL)
    expect_null(emr_db.subset.info())
    expect_null(emr_db.subset.ids())

    a <- emr_extract("track1")
    expect_true(all(all_ids$id %in% a$id))
})

test_that("emr_db.subset works with track", {
    set.seed(60427)
    emr_db.subset("track1", fraction = 0.8, complementary = FALSE)
    withr::defer(emr_db.subset(NULL))
    ids <- emr_db.subset.ids()
    expect_equal(nrow(ids), 800)
    a <- emr_extract("track1")
    expect_true(all(a$id %in% ids$id))

    expect_equal(
        emr_db.subset.info(),
        list(src = "track1", fraction = 0.8, complementary = FALSE)
    )
})

test_that("emr_db.subset works with track and complementary=TRUE", {
    set.seed(60427)
    emr_db.subset("track1", fraction = 0.8, complementary = TRUE)
    withr::defer(emr_db.subset(NULL))
    ids <- emr_db.subset.ids()
    expect_equal(nrow(ids), 200)
    a <- emr_extract("track1")
    expect_true(all(a$id %in% ids$id))

    expect_equal(
        emr_db.subset.info(),
        list(src = "track1", fraction = 0.8, complementary = TRUE)
    )
})
