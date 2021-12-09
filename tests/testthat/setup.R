# Note: we should change this with download.file from a public link
library(dplyr, warn.conflicts = FALSE)
library(glue, warn.conflicts = FALSE)

load_test_db()

# Clean examples db
withr::defer(
    {        
        .emr_call("emr_dbunload", silent = TRUE)
        testdb_dir <- test_path("../testdb")
        unlink(testdb_dir, recursive = TRUE)
        purrr::walk(c(1:4), ~ {
            unlink(glue("{testdb_dir}_{.x}"), recursive = TRUE)
        })
    },
    teardown_env()
)
