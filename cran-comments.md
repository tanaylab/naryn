## R CMD check results

0 errors | 0 warnings | 0 notes

* Fix CRAN notes about a redirected URL, missing documentation and C++11 specification.
* Regarding cross-platform availability, the package implements a database that is based on shared memory files, and therefore includes many unix-specific system calls. In addition, many parallel algorithms rely on the unix forking mechanism.
