# naryn 2.6.3

* Added support for operator based filtering, see 'operator' argument in `emr_filter.create`
* Added support for track attributes to logical tracks. 
* Bug fix: dbs order was changed to lexicographical order on emr_db.connect.
* Bug fix: stray logical tracks on non-global dbs were shown.
* Make sure logical tracks cannot be created on non-global db. 
* Removed cpp11 package dependency


# naryn 2.6.2

* Added support for multiple databases. 
* New functions: `emr_filter.name` and `emr_filter.create_from_name` allow creating filters 
with standard names. 
* Bug fix: allow calling `emr_track.logical.create` when values are a list of length 1. 
* Bug fix: wrong results when creating virtual track where the source is a logical track 
without parameters. 

# naryn 2.6.1

* Added support for logical tracks. 
* `emr_filter.create` and `emr_vtrack.create` now return silently the name of the filter/vtrack.
* Allow addition of entries with the same id/time/values to `emr_track.addto`. 
* `emr_dist` and `emr_cor` now have a `dataframe` parameter that returns the results in a tidy format. 
* Bug fix: wrong results in `emr_dist` when `right=FALSE` and values were a fraction.
* Added convenience time functions (emr_time, year, months etc). 
* Support periodic iterators: `emr_monthly_iterator`, `emr_yearly_iterator`
* Use the `devtools` ecosystem: `roxygen` documentation and `testthat` for tests.
* Added a `NEWS.md` file to track changes to the package.
