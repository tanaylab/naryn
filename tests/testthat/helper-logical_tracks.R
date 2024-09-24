create_mock_logical_track <- function() {
    set.seed(60427)
    testdb_dir <- "/net/mraid20/export/tgdata/db/tgdb/emr/naryn_testdb"
    emr_db.init(c(testdb_dir, file.path(testdb_dir, "utest")))
    emr_db.reload()

    df <- emr_extract("track1_sparse", keepref = TRUE, names = "val")
    df <- df %>%
        dplyr::mutate(new_val = sample(c(1, 11:19), size = dplyr::n(), prob = c(0.5, rep(0.5 / 9, 9)), replace = TRUE)) %>%
        dplyr::select(id, time, ref, value = new_val)

    emr_track.import("ph1", "global", categorical = TRUE, src = df)

    df1 <- df %>%
        dplyr::filter(value == 15)

    emr_track.import("ph1_subset_15", "global", categorical = TRUE, src = df1)
}

clean_logical_tracks <- function() {
    purrr::walk(emr_track.logical.ls(), emr_track.logical.rm, force = TRUE)
}

logical_track_path <- function(track) {
    root <- get("EMR_GROOT", envir = .naryn)
    file.path(root, "logical", glue::glue("{track}.ltrack"))
}

logical_track_ok <- function(track, source, values = NULL) {
    expect_true(track %in% emr_track.logical.ls())
    expect_true(track %in% emr_track.ls())
    expect_true(track %in% emr_track.global.ls())
    expect_true(emr_track.logical.exists(track))
    expect_equal(emr_track.logical.info(track)$source, source)
    if (is.null(values) || length(values) == 0) {
        expect_null(emr_track.logical.info(track)$values)
    } else {
        expect_equal(emr_track.logical.info(track)$values, values)
    }

    expect_true(emr_track.exists(track))
    expect_true(file.exists(logical_track_path(track)))
}
