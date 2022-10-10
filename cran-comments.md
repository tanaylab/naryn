## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.
* The phrasing of the package description was changed as suggested by CRAN.
* options(warn=-1) calls were replaced with suppressWarnings() calls.
* "Weizmann Institute of Science" was added as "cph". 
* The '\dontrun{}' examples were changed to '\donttest{}', except for `emr_traceback()` which should not run because it requires a specific error to be thrown.
