load_test_db <- function(testdb_dir = tempfile(pattern = "testdb_", tmpdir = test_path(".."))) {
    dir.create(testdb_dir)

    # Note: we should change this to download.file from a public link
    system(glue::glue("cp -rf /net/mraid14/export/tgdata/db/tgdb/emr/naryn_testdb/* {testdb_dir}/"))

    emr_db.connect(c(testdb_dir, file.path(testdb_dir, "utest")))
    emr_db.reload()
}


load_test_dbs <- function() {
    testdb_dir <- test_path("../testdb")
    testdb_dirs <- purrr::map_chr(c(1:4), ~ {
        glue::glue("{testdb_dir}_{.x}")
    })

    purrr::walk(testdb_dirs, ~ {
        if (dir.exists(.x)) {
            system(glue::glue("rm -rf {.x}"))
        }
    })

    purrr::walk2(c(1:4), testdb_dirs, ~ {
        system(glue::glue("cp -rf /net/mraid14/export/tgdata/db/tgdb/emr/nr_test_db_{.x}/ {.y}"))
    })

    emr_db.connect(db_dirs = testdb_dirs, do_reload = TRUE)
}
