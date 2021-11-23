load_test_db <- function() {
    testdb_dir <- test_path("../testdb")
    if (dir.exists(testdb_dir)) {
        system(glue::glue("rm -rf {testdb_dir}"))
    }
    system(glue::glue("cp -rf /net/mraid14/export/tgdata/db/tgdb/emr/naryn_testdb {testdb_dir}"))

    emr_db.init(testdb_dir, file.path(testdb_dir, "utest"))
    emr_db.reload()
}
