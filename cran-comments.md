## R CMD check results

0 errors | 0 warnings | 0 notes

* Fixed C++20 deprecation warnings: removed `std::rel_ops` usage and fixed deprecated enum arithmetic.
* As was written in the previous submissions, the package implements a database that is based on shared memory files and therefore includes many unix-specific system calls. In addition, many parallel algorithms used in the package rely on the unix forking mechanism, therefore the package is not fully portable to Windows.
