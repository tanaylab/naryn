# Note: we should change this with download.file from a public link
library(dplyr, warn.conflicts = FALSE)
library(glue, warn.conflicts = FALSE)

options(timeout = 1e4)

tar_path <- test_path("../dbs.tar.gz")

download.file(
    "https://naryn.s3.eu-west-1.amazonaws.com/dbs.tar.gz",
    destfile=tar_path
)

untar(tar_path, exdir=test_path("../"))

load_test_db()

# Clean examples db
withr::defer({
        emr_db.init_examples()
        
        testdb_dir <- test_path("../naryn_testdb")
        system(glue("rm -rf {testdb_dir}"))

        testdb_dir <- test_path("../nr_test_db")
        purrr::walk(c(1:4), ~{system(glue("rm -rf {testdb_dir}_{.x}"))})

        testdb_tar <-  test_path("../dbs.tar.gz")
        system(glue("rm -rf {testdb_tar}"))

    },
    teardown_env()
)
