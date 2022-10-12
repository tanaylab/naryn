#' Download example database
#'
#' @description Download an example database which was simulated to include an example of a typical EMR database.
#'
#'
#' @param dir Directory to save the database to. Default: current working directory.
#' @param temp_dir Directory to save the temporary downloaded file to.
#' Change if your system has a small `/tmp`` directory
#'
#' @return None. The database is saved under the name `sample_db` in the specified directory.
#'
#' @examples
#' \donttest{
#' emr_download_example_data()
#' }
#'
#' \dontshow{
#' unlink("sample_db", recursive = TRUE)
#' }
#'
#' @export
emr_download_example_data <- function(dir = getwd(), temp_dir = tempdir()) {
    temp_file <- tempfile(tmpdir = temp_dir)

    # Download the data
    withr::with_options(list(timeout = 1e4), utils::download.file("https://naryn.s3.eu-west-1.amazonaws.com/naryn_example_db.tar.gz", temp_file))

    message("Extracting data...")
    # untar the data
    utils::untar(temp_file, exdir = dir)

    stopifnot(file.exists(file.path(dir, "sample_db")))

    message(glue::glue("Downloaded example database to {dir}/sample_db"))
}
