# Note: we should change this with download.file from a public link
library(dplyr, warn.conflicts = FALSE)
library(glue, warn.conflicts = FALSE)

testdb_dir <- test_path("../testdb")
if (dir.exists(testdb_dir)) {
    system(glue("rm -rf {testdb_dir}"))
}
system(glue("cp -rf /net/mraid14/export/tgdata/db/tgdb/emr/naryn_testdb {testdb_dir}"))

emr_db.init(testdb_dir, file.path(testdb_dir, "utest"))
emr_db.reload()

#' Tests if an object was changed since the last run
#' @param obj an R object
#' @param snapshot_dir directory with rds file containing snapshot of previous versions
expect_regression <- function(obj, snapshot_dir = "/net/mraid14/export/tgdata/db/tgdb/emr/naryn_snapshot") {
    obj_hash <- digest::digest(obj, algo = "md5")
    regression_file <- file.path(snapshot_dir, glue("{obj_hash}.rds"))

    if (!file.exists(regression_file)) {
        readr::write_rds(obj, regression_file)
    }

    # this is not part of an else statement since we need testthat to always find the `expect` statement (otherwise - the test would be skipped)
    old <- readr::read_rds(regression_file)
    expect_identical(old, obj)
}
