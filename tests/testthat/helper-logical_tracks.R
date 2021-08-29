create_mock_logical_track <- function() {
    set.seed(60427)
    testdb_dir <- "/net/mraid14/export/tgdata/db/tgdb/emr/naryn_testdb"
    emr_db.init(testdb_dir, file.path(testdb_dir, "utest"))
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
