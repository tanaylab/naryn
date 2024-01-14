# naryn 2.6.26 

* Fixed compilation warnings on M1 Mac.

# naryn 2.6.25

* Fixed clang warnings.
* Fix: filter name to include operator when name is automatically generated

# naryn 2.6.24

* Allow filters names with a dot when creating an automatic virtual track name.
* Fixed an error when multiple processes are calling `emr_track.var.set` on the same track.

# naryn 2.6.23 

* Fixed memory misalignment errors.

# naryn 2.6.22

* Added `emr_filters.info` that returns filters information given a filter expression (e.g. `f1 | f2`).
* Bug fix: vtrack on logical track to categorical track with no parameters returned an wrong results.
* Fixed clang-UBSAN error when creating an empty track.
* Added "modification_time" to `emr_track.info`.

# naryn 2.6.21

* Removed C++11 specification from Makevars.

# naryn 2.6.20

* fixed some CRAN warnings.

# naryn 2.6.19

* fix: `emr_vtrack.create` with `func="exists"` returned NA instead of 0 when the track didn't have any value at the iterator interval.


# naryn 2.6.18 

* Fix: deal with complex filters in `emr_vtrack.name` and `emr_vtrack.create_from_name`.

# naryn 2.6.17

* New functions: `emr_vtrack.name` and `emr_vtrack.create_from_name` allow creating virtual tracks 
with standard names. 

# naryn 2.6.16

* Added some examples to the documentation.
* Fix: crash when calling `emr_track.create` with `expr=''`

# naryn 2.6.15

* Added entries - a key/value store for storing simple strings and numbers in a `naryn` database. See `emr_entries.ls`, `emr_entries.get`, `emr_entries.set`, `emr_entries.rm` and `emr_entries.reload`.
* Added `string_to_var` and `var_to_string` functions to generate valid variable names from strings.
* `emr_track.rm` can now accept multiple tracks as input. 
* Do not allow importing patients that do not appear at `patients.dob`.
* Fixed issue #83
* Fix: `emr_track.rm` returned an error when given `character(0)`.

# naryn 2.6.14 

* Added <cstdint> include in order to compile with gcc13

# naryn 2.6.13 

* Fixed c++ clang-UBSAN and gcc-UBSAN errors.  
* Fixed `rchk` warnings.

# naryn 2.6.12

* Fixed: hang when all iterator intervals were outside of `stime` and `etime`. 

# naryn 2.6.11

* Fixed an additional clang warning.

# naryn 2.6.10

* removed `emr_traceback` function
* Fixed C++ compilation warnings and possible memory leaks.

# naryn 2.6.9 

* Removed global variables from the package - EMR_ROOTS, EMR_GROOT, EMR_UROOT, EMR_FILTERS and EMR_VTRACKS are now a part of a dedicated environment called `.naryn`. You can access them using `.naryn::EMR_ROOTS` etc. 

# naryn 2.6.8 

* Optimized the performance of the `emr_track.exists` function.

# naryn 2.6.7

* Check that attributes are in ASCII format.

# naryn 2.6.6

* Allow duplicated tracks when running `emr_track.attr.set` in batch mode.

# naryn 2.6.5

* Added batch mode for `emr_track.attr.set` and `emr_track.attr.rm`.
* Added `include_missing` parameter `emr_track.attr.export` allowing missing tracks and attributes.

# naryn 2.6.4 

* Added support for filters which are based on virtual tracks. 

# naryn 2.6.3

* Added support for operator based filtering, see 'operator' argument in `emr_filter.create`
* Added support for track attributes to logical tracks. 
* Bug fix: dbs order was changed to lexicographical order on emr_db.connect.
* Bug fix: stray logical tracks on non-global dbs were shown.
* Bug fix: `emr_time` functions failed when there was an `NA` in an integer vector.
* Bug fix: `emr_track.attr.get` returned attribute name instead of value.
* `emr_track.var.get` now returns `NULL` when variable doesn't exist.
* Make sure logical tracks cannot be created on non-global db. 
* Added `db_id` parameter to `emr_track.ls`
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

# naryn 2.6.0

*  New concept: track attributes
*  New functions: emr_track.attr.get, emr_track.attr.set, emr_track.attr.rm, emr_track.attr.export.
*  Support filtering by track attributes in: emr_track.ls, emr_track.global.ls, emr_track.user.ls.

# naryn 2.5.10

*  Fixed an occasional crash in functions receiving a track expression.
*  Redirect all messages and progress to stderr instead of stdout.

# naryn 2.5.9

*  Fixed a resource leak effecting nearly all functions but manifesting mainly in emr_ids_coverage and emr_ids_vals_coverage by "protection stack overflow" error.

# naryn 2.5.8

*  Bug fix in emr_track.mv: track variables are not moved.

# naryn 2.5.7

*  Bug fix in emr_track.mv: error when moving a file between different file systems.

# naryn 2.5.6

*  New function: emr_track.mv

# naryn 2.5.5

*  Bug fix: occasional memory corruption in track expressions using OR operator in the filter.

# naryn 2.5.4

*  Bug fix: occasional crash in emr_track.rm.
*  Bug fix: Ctrl+C doesn't stop emr_track.import.

# naryn 2.5.3

*  Bug fix: sub-second creation/deletion/modification of tracks in concurrent sessions might corrupt the database.

# naryn 2.5.2

*  emr_track.import, emr_track.addto: run-time and memory consumption improvements.

# naryn 2.5.1

*  Allow creation of empty tracks.

# naryn 2.5.0

*  Support concurrent sessions adding / deleting / modifying tracks. As long as the tracks files are not manipulated outside of naryn, sessions should always resync the changes at the beginning of a new transaction and hence access up-to-date version of tracks and up-to-date list of existing tracks. Manipulating track files outside of naryn requires emr_db.reload.
*  **emr_db.init**: split load.on.demand parameter to global.load.on.demand and user.load.on.demand.
*  **emr_db.init**: new 'do.reload' parameter.
*  Run-time improvement in **emr_db.init** when load.on.demand==FALSE: previously - rebuild list of tracks. Currently - read track list from '.tracks'.
*  Major run-time and memory consumption improvement in **emr_db.reload**: previously - rebuild list of tracks and reload <u>all</u> tracks into memory. Currently - rebuild list of tracks and reload <u>outdated</u> tracks (if load.on.demand==FALSE) or kick them out of memory (if load.on.demand==TRUE).
*  **emr_db.reload**: while rebuilding the list DB gets locked, concurrent transactions will need to wait till the task completes.
*  Currently - each transaction starts with fetching a track list from '.tracks', if '.tracks' had been modified between the transactions. Outdated tracks are unloaded from the memory (if load.on.demand==TRUE) or reloaded (if load.on.demand==FALSE).  
    Previously - new transaction continues using old track list and outdated tracks up until emr_db.reload is called.
*  Bug fix: '.ids' file is reloaded in each transaction even if the updated version of it is already in the memory.
*  emr_time2hour, emr_time2dayofmonth, emr_time2month, emr_time2year, emr_date2time do not require anymore to open a database.

# naryn 2.4.4

*  Removed MINID/MAXID constants from global environment.
*  IDs for Beat iterator and Time Interval take their ids from 'dob' track.
*  Bug fix in emr_track.import, emr_track.addto: crash when source data frame has 'ref' but no 'value' column.
*  Bug fix in Time Intervals iterator: in some cases ids outside of subset might be produced.
*  DB cache file format has changed. The file is now called '.tracks'. The first call to emr_db.init will recreate this file after a full DB reload.

# naryn 2.4.3

*  Removed MINTIME/MAXTIME constants from global environment.
*  All functions receiving stime/etime parameters and an iterator: defaults can no longer be used for stime/etime when the iterator type is Ids, Beat or Extended Beat.
*  DB cache file format has changed, the first call to emr_db.init will recreate this file after a full DB reload.

# naryn 2.4.2

*  Bug fix in emr_track.info: returned path is NULL.

# naryn 2.4.1

*  More robust handling of .naryn-cache file when several users simultaneously add / delete tracks. Might resolve various glitches when emr_db.reload() is needed.

# naryn 2.4.0

*  emr_vtrack.create / emr_vtrack.attr.src: allow 'src' to be a data frame. Syntax: src=list(data.frame, T/F), where T/F indicates whether the data is treated as categorical (otherwise: quantitative).
*  Bug fix in emr_filter.attr.src: error when 'src' is a data frame

# naryn 2.3.12

*  Bug fix: various functions may suddenly return an empty data frame

# naryn 2.3.11

*  Bug fix: crash if the period of beat / extended beat iterator is set to 0

# naryn 2.3.10

*  Bug fix: crash in certain scenarios while using 'filter' parameter. For example: filter="(track1 & track2) & (track3 & track4)"

# naryn 2.3.9

*  Bug fix in emr_filter.attr.src: error if 'src' is ID-Time table

# naryn 2.3.8

*  Bug fix in emr_filter.create: named filters become inconsistent if src of the existing filter is changed from global track to user track or vice versa
*  Bug fix in emr_vtrack.create: virtual tracks become inconsistent if src of the existing filter is changed from global track to user track or vice versa
*  Bug fix in emr_filter.ls: if filters exist with src based on user space tracks, only these filters are returned
*  Bug fix in emr_vtrack.ls: if virtual tracks exist with src based on user space tracks, only these virtual tracks are returned
*  Run time optimizations in emr_filter.create and emr_vtrack.create

# naryn 2.3.7

*  Bug fix in emr_db.subset: error if src==NULL
*  Bug fix in virtual track, func="stddev": crash when stddev is computed over a single value

# naryn 2.3.6

*  Bug fix in emr_track.percentile: lower bound is returned even if lower=FALSE

# naryn 2.3.5

*  Bug fix: MINTIME/MAXTIME may be wrongly set. As a result performance might be compromised and invalid results might be produced when defaults are used for stime,etime. Please call emr_db.reload() to set the correct values!

# naryn 2.3.4

*  Replaced "categorial" with "categorical" in all the parameters and documentation

# naryn 2.3.3

*  Bug fix: "child process ended unexpectedly" errors, crashes and hang ups whilst multitasking when running out of memory

# naryn 2.3.2

*  New iterator type: "Id-Time" Intervals (dataframe with 'id', 'stime', 'etime' columns)
*  New filter type: "Id-Time" Intervals

# naryn 2.3.1

*  Set/retrieve a single attribute of a virtual track via: emr_vtrack.attr.src, emr_vtrack.attr.func, emr_vtrack.attr.params, emr_vtrack.attr.keepref, emr_vtrack.attr.time.shift, emr_vtrack.attr.id.map, emr_vtrack.attr.filter
*  Set/retrieve a single attribute of a named filter via: emr_filter.attr.src, emr_filter.attr.keepref, emr_filter.attr.time.shift, emr_filter.attr.val, emr_filter.attr.expiration

# naryn 2.3.0

*  Filters can be applied now to the virtual tracks
*  emr_vtrack.create: new 'filter' parameter

# naryn 2.2.11

*  emr_vtrack.create: new "sample.time" function

# naryn 2.2.10

*  Switched from custom random seed control (options(grnd.seed=...)) to R's standard (set.seed)
*  emr_db.subset: 'seed' parameter removed
*  New function: emr_db.subset.info

# naryn 2.2.9

*  Issue a warning if beat / extended beat iterator is used without a filter and it produces more than options(emr_warning.itr.no.filter.size = ...) points (default: 100000). To suppress this type of warning use options(emr_warning.itr.no.filter.size = -1)

# naryn 2.2.8

*  emr_dist, emr_cor: new "right" parameter (similar to "cut" function)

# naryn 2.2.7

*  emr_vtrack.create: new "sample" function

# naryn 2.2.6

*  Bug in multitasking: buffer overflow and possible memory corruption when the child process generates an error

# naryn 2.2.5

*  emr_vtrack.create: time.shift column can now be used within 'id.map' data frame uniquely remapping times and not only ids

# naryn 2.2.4

*  Multitasking stability issues

# naryn 2.2.3

*  Bug fix in emr_screen: in multitasking mode returns wrong results
*  Bug fix: multitasking + debug mode might lead to hanging

# naryn 2.2.2

*  Bug fix in track expression evaluation engine: virtual track with func="size" returns NaN instead of 0 when no values fall into the iterator interval

# naryn 2.2.1

*  New function: emr_annotate

# naryn 2.2.0

*  New functions: emr_track.var.get, emr_track.var.set, emr_track.var.rm, emr_track.var.ls
*  emr_track.import, emr_track.create: introduce restrictions for track naming

# naryn 2.1.11

*  Bug fix in track expression evaluation engine: if two or more virtual tracks differ only by values (supplied in params) and are used in one track expression, the value of the first one is used for the rest

# naryn 2.1.10

*  Bug fix: in certain circumstances an error "Database was not loaded. Please call emr_db.init." is printed

# naryn 2.1.9

*  Bug fix in emr_filter.create: warning is issued when ID-Time table is used as source
*  Bug fix in emr_ids_coverage: when src is an ID-Time table and the filter is used "..emr_tmp_filter does not exist (9)" error is generated
*  Bug fix in emr_ids_vals_coverage: when src is an ID-Time table "..emr_tmp_filter does not exist (9)" error is generated

# naryn 2.1.8

*  New function: emr_track.percentile
*  emr_summary: renamed two members of the result member from "Total intervals"/"NaN intervals" to "Total values"/"NaN values"

# naryn 2.1.7

*  New functions: emr_track.exists, emr_vtrack.exists, emr_filter.exists
*  emr_filter.create, emr_vtrack.create - run-time optimizations

# naryn 2.1.6

*  New function: emr_track.ids
*  emr_ids_coverage - new additional parameters: stime, etime, filter
*  emr_ids_vals_coverage - run-time optimizations

# naryn 2.1.5

*  emr_filter.create: support vector of values for "val" parameter

# naryn 2.1.4

*  Redesigned emr_track.import function - supports data frame, does not allow anymore to add records to the existing track
*  New function: emr_track.addto
*  New "space" argument to emr_track.create function

# naryn 2.1.3

*  New filter on values for all virtual track functions applied to categorical tracks. The filter is given via "param" in emr_vtrack.create
*  Bug fix: virtual track function "exists" might not work on non-integer values where double(val) != float(val)

# naryn 2.1.2

*  Bug fix: various functions might hang when emr_debug and emr_multitasking options are both TRUE

# naryn 2.1.1

*  New virtual track functions for simple linear regression model: lm.slope, lm.intercept

# naryn 2.1.0

*  Bug fix: occasional defunc processes AND/OR hanging in multitasking mode

# naryn 2.0.6

*  Mild run-time optimizations due to switching to C++11

# naryn 2.0.5

*  Bug fix: MAXTIME and MAXID might be incorrectly set which might have grave effect on iterators, multitasking, etc. Please call emr_db.reload() on all your trackdbs
*  New option: emr_debug=T/F

# naryn 2.0.4

*  Bug fix in multitasking mode: child process does not die after generating an error

# naryn 2.0.3

*  Run-time optimization in a filter when NOT is applied to a list of time intervals

# naryn 2.0.2

*  Run-time optimization in a filter when NOT is applied to a list of ids

# naryn 2.0.1

*  emr_db.init: new "load.on.demand" parameter
*  emr_db.init: very fast run-time when "load.on.demand" parameter is used

# naryn 2.0.0

*  Multitasking for all functions that use track expression evaluation engine
*  emr_extract and emr_screen: new "sort" parameter
*  New R options controlling multitasking behavior: emr_multitasking, emr_min.processes, emr_max.processes
*  Run-time optimizations in track expression evaluation engine in single-task mode
*  Bug-fix: wrong iteration on dense tracks when subsets are used

# naryn 1.4.7

*  Bug-fix: in certain cases incorrect results when NOT is used in the filter
*  Run-time optimization when NOT is used in the filter

# naryn 1.4.6

*  emr_extract: receive track expressions as a vector
*  emr_extract: new "tidy" parameter
*  emr_extract: "colnames" parameter renamed to "names"
*  Run-time optimizations

# naryn 1.4.5

*  Run-time optimization on long filters
*  Run-time optimization when NOT operator is used in a filter

# naryn 1.4.4

*  Bug fix in filters: incorrect result when OR and NOT are used together

# naryn 1.4.3

*  Bug fix in filters: expiration and val produce incorrect results when used together
*  Bug fix in emr_db.init: occasional crash due to too many files open

# naryn 1.4.2

*  New functions: emr_ids_coverage and emr_ids_vals_coverage
*  Bug fix: MINTIME, MAXTIME, MINID, MAXID constants are not updated after emr_track.create, emr_track.import and emr_track.rm are called

# naryn 1.4.1

*  New function: emr_cor. Returns correlation statistics for each pair of a track expression vector

# naryn 1.4.0

*  New concept: subsets. Use subsets to define train / test sets. Please refer user manual for further details
*  New functions: emr_db.subset, emr_db.subset.ids
*  New option: emr_rnd.seed. Currently used by emr_quantiles

# naryn 1.3.3

*  New global EMR_TIME variable that stores the times of the current iterator
*  Return an empty data frame instead of NULL in emr_extract and emr_screen when result set is empty
*  Added user manual (still incomplete but covers already tracks, virtual tracks, track expressions and iterators).

# naryn 1.3.2

*  Run-time optimizations

# naryn 1.3.1

*  New iterator type: beat iterator aligned to the time that appears in a data frame or a track (aka birthday). Syntax:  
    iterator=list(period, data.frame(id=..., time=...))  
    iterator=list(period, trackname)  

    Note: each id must appear only once in the data frame / track.

# naryn 1.3.0

*  New concept: global tracks and user tracks. emr_track.import creates tracks in global tracks directory, emr_track.create does it in user tracks directory.
*  New functions: emr_track.create, emr_track.rm, emr_track.global.ls, emr_track.user.ls
*  emr_db.init: new user.dir parameter
*  emr_db.init: renamed root parameter to global.dir
*  emr_track.info: added file path to the output.
*  emr_track.import: removed reload_db parameter. Imported track will automatically be efficiently reloaded.

# naryn 1.2.1

*  emr_filter.create: new "val" parameter (slicing)
*  emr_filter.create: new "expiration" parameter. Please refer reference manual for usage.
*  Run-time optimizations

# naryn 1.2.0

*  New concept: named filters. No more parameters (time shift, keepref) are allowed in track expression filters. Please use named filters instead.
*  New functions: emr_filter.create, emr_filter.rm, emr_filter.ls, emr_filter.info
*  New virtual track functions: earliest.time, latest.time, closest.earlier.time, closest.later.time
*  emr_time2...: return NaN if the time is NaN
*  emr_time2...: return numeric instead of integer
*  4+5 = ability to use emr_time2... in track expressions
*  Bug fix in emr_track.import: do not allow creation of a track if identically named virtual track already exists

# naryn 1.1.8

*  emr_vtrack.create: new id.map parameter
*  emr_vtrack.create: renamed time_shift parameter to time.shift

# naryn 1.1.7

*  emr_dist: allow implicit breaks (use breaks=NULL) if the track expression constitutes of a categorical track or a virtual track that is based on a categorical track

# naryn 1.1.6

*  Run-time optimizations when track expression equals to a track or a virtual track

# naryn 1.1.5

*  New logical parameter in emr_track.import: "categorical"
*  Track format had been changed
*  New function: emr_track.unique - returns sorted unique values of a track. Hint: use this functions to set breaks when categorical track is used
*  emr_vtrack.create: "val" parameter had been removed
*  emr_vtrack.create: new functions "value", "size", "frequent", "exists"
*  emr_vtrack.create: functions "earliest", "latest" and "closest" return -1 in case of ambiguity for categorical tracks
*  Interpret data frame with id, time columns as patient-time even if ref column is missing
*  Bug fix: emr_db.init continues failing after it failed once (for example: due to invalid track format)

# naryn 1.1.4

*  New functions: emr_track.info, emr_summary
*  Enable patient ids to be used as iterators
*  Enable patient ids  to be used in filters
*  Patient ids format for 2 & 3: data frame with the first column named "id"
*  Enable time intervals to be used as iterators
*  Enable time intervals to be used in filters
*  Time intervals format for 5 & 6: data frame with the first columns named "stime" and "etime"
*  Run-time optimization when OR is used in iterator filter
*  Report error id-time list used for iterator / filter contains two records that differ only by reference when keepref == FALSE
10.  Bug fix in filter when time after shift precedes the epoch
11.  Bug fix in beat iterator: invalid progress report

# naryn 1.1.3

*  Enable beat iterator (iterator=integer) produce references if keepref=T
*  Support filters when beat iterator is used
*  1+2 means that one can now run complex iterators. For example iterator=track1 | track2, keepref = R can be simulated with help of a beat iterator and a filter as: iterator=1, keepref=R, filter="track1(R) | track2(R)"
*  Support filters when patient-time list is used as an iterator
*  Enable patient-time lists to be used in the filter. The syntax is identical to tracks, e.g.: filter="mylist1(F, c(10,20)) | track & !mylist2"
*  Prefetch track files into shared memory: initial emr_db.init is expected to be slower, however subsequent access to the tracks is faster

# naryn 1.1.2

*  Use shared memory to store tracks in RAM

# naryn 1.1.1

*  Added regression tests
*  Bug fix in emr_date2time: crash if year is out of range
*  Bug fix in emr_vtrack.create: functions percentile.upper.min, percentile.lower.min, percentile.upper.max, percentile.lower.max generate an error

# naryn 1.1.0

*  Added track filters. Examples of valid filters:  
    "track1", "track1(T)", "track1(F, 2)", "track1(F, c(10, 20))", "track1 & track2", "track1 | track2", "!track1", "!track1 & (track2(F, c(10, 20)) | !track3(T))"
*  Bug fix: track format changes depending on the OS.

# naryn 1.0.5

*  New function: emr_dist

# naryn 1.0.4

*  Increase the maximal number of simultaneously used virtual tracks to 10K

# naryn 1.0.3

*  Add "val" parameter to emr_vtrack.create

# naryn 1.0.2

*  Allow addition of data into the existing tracks emr_track.import (use "add" parameter)

# naryn 1.0.1

*  New functions: emr_quantiles, emr_track.import
*  Support new functions in virtual tracks: percentile.upper, percentile.lower, percentile.max.upper, percentile.max.lower, percentile.min.upper, percentile.min.lower
*  Track format changed, old track format is not supported anymore
*  New keepref parameter in emr_vtrack.create
*  Changes in vtrack evaluation

# naryn 1.0.0

*  First version
