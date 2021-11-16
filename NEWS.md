# naryn 2.6.1

* Added support for logical tracks. 
* `emr_filter.create` and `emr_vtrack.create` now return silently the name of the filter/vtrack.
* Allow addition of entries with the same id/time/values to `emr_track.addto`. 
* `emr_dist` and `emr_cor` now have a `dataframe` parameter that returns the results in a tidy format. 
* Bug fix: wrong results in `emr_dist` when `right=FALSE` and values were a fraction.
* Added convenience time functions (emr_time, year, months etc). 
* Use the `devtools` ecosystem: `roxygen` documentation and `testthat` for tests.
* Added a `NEWS.md` file to track changes to the package.
