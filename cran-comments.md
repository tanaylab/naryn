## R CMD check results

0 errors | 0 warnings | 3 notes

* This is a new release.
* The hidden files under inst/naryndb/test/ are part of the examples database which is included in the package. The database uses those files to index the tracks and their attributes. 
* The calls to stderr, stdout and exit are used only for forked processes while running in parallel mode, and are not used in the main process. Note that the parallel mode is not the default. 
* The tests rely on a large database which is not included in the package, and therefore they are skipped on CRAN. Note that the package is well tested - there are more than 1400 tests which are run before any release. In addition, there are examples on every function that are being invoked on CRAN.
