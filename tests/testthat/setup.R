# Note: we should change this with download.file from a public link
library(dplyr, warn.conflicts = FALSE)
library(glue, warn.conflicts = FALSE)

load_test_db()

# Clean examples db
withr::defer(
    {
        emr_db.init_examples()
        testdb_dir <- test_path("../testdb")
        system(glue("rm -rf {testdb_dir}"))
        purrr::walk(c(1:4), ~{system(glue("rm -rf {testdb_dir}_{.x}"))})
    },
    teardown_env()
)
