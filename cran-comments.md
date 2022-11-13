## R CMD check results

0 errors | 0 warnings | 0 notes

* As noted at a reply to the previous submission, I think the 
gcc-ASAN and clang-ASAN tests were run on the wrong version of the 
package, one that was different then the one on CRAN ftp. 
This defintely could be my fault for not incrementing the version number, 
so I apologize in advance for re-submitting the package with 
incremented version (2.6.11) and a small additional compilation warning fix. 
* Fixed compilation warnings on MAC
* Fixed linking errors on gcc-ASAN and clang-ASAN
