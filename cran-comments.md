## R CMD check results

0 errors | 0 warnings | 0 notes

* Fixed UBSAN's memory misalignment errors. 
* Since this keeps arising every submission, attached are the reasons for the unix OS requirement: The package implements a database that is based on shared memory files and therefore includes many unix-specific system calls. In addition, many parallel algorithms used in the package rely on the unix forking mechanism.
