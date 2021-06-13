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
