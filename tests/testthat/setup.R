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

# Clean examples db
withr::defer(
    {
        emr_db.init_examples()
        system(glue("rm -rf {testdb_dir}"))
    },
    teardown_env()
)

#' Tests if an object was changed since the last run.
#' If an rds file named \code{snapshot_dir/id.rds} exists its contents are compared with \{obj},
#' otherwise the file is created.
#'
#' @param obj an R object
#' @param id unique test id.
#' @param snapshot_dir directory with rds file containing snapshot of previous versions
expect_regression <- function(obj, id, snapshot_dir = "/net/mraid14/export/tgdata/db/tgdb/emr/naryn_snapshot") {
    regression_file <- file.path(snapshot_dir, glue("{id}.rds"))

    if (!file.exists(regression_file)) {
        readr::write_rds(obj, regression_file)
        system(glue("chmod a-w {regression_file}"))
    }

    # We need testthat to always find the `expect` statement (otherwise - the test would be skipped)
    old <- readr::read_rds(regression_file)
    expect_identical(old, obj)
}
