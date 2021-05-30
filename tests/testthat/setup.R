# Note: we should change this with download.file from a public link
if (dir.exists("../testdb")) {
    system("rm -rf ../testdb")
}
system("cp -rf /net/mraid14/export/tgdata/db/tgdb/emr/naryn_testdb ../testdb")

emr_db.init("../testdb", "../testdb/utest")
emr_db.reload()
