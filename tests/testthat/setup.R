# Note: we should change this with download.file from a public link
library(dplyr, warn.conflicts = FALSE)
library(glue, warn.conflicts = FALSE)

# Clean examples db
withr::defer(
    {
        db_dirs <- list.files(test_path(".."), pattern = "testdb_*", full.names = TRUE)
        purrr::walk(db_dirs, ~ {
            emr_db.connect(.x)
            emr_db.unload()
            unlink(.x, recursive = TRUE)
        })
        emr_db.init_examples()
    },
    teardown_env()
)
