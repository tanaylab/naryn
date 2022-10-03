library(testthat)
library(naryn)

if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    test_check("naryn")
}
