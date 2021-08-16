# Note: we should change this with download.file from a public link
library(dplyr, warn.conflicts = FALSE)
library(glue, warn.conflicts = FALSE)

load_test_db()

# Clean examples db
withr::defer(
    {
        emr_db.init_examples()
        system(glue("rm -rf {testdb_dir}"))
    },
    teardown_env()
)
