load_test_db()


test_that("emr_track.create works", {
    emr_track.rm("test_track1", TRUE)
    emr_track.create("test_track1", "user", FALSE, "track0", keepref = TRUE)
    expect_true(emr_track.exists("test_track1"))
    withr::defer(emr_track.rm("test_track1", TRUE))

    e0 <- emr_extract("track0", keepref = TRUE)
    e1 <- emr_extract(c("test_track1", "track0"), iterator = "test_track1", keepref = TRUE)

    expect_equal(nrow(e1), 100000)
    expect_identical(e1$test_track1, e1$track0)
    expect_equal(e1$track0, e1$test_track1)
    expect_equal(e0$track0, e1$test_track1)
    expect_equal(e0$id, e1$id)
    expect_equal(e0$time, e1$time)
    expect_equal(e0$ref, e1$ref)

    e2 <- emr_extract(c("test_track1", "track0"), iterator = "track0", keepref = TRUE)
    expect_identical(e1, e2)

    track.info <- emr_track.info("test_track1")
    track.info$path <- NULL
    track.info$modification_time <- NULL
    expect_equal(
        track.info,
        list(
            type = "dense", data.type = "float", categorical = FALSE,
            num.vals = 100000L, num.unique.vals = 1000L, min.val = 0,
            max.val = 999, min.id = 0L, max.id = 999L, min.time = 0L,
            max.time = 9999L
        )
    )
    track.info$path <- NULL
})

test_that("create and remove categorical", {
    emr_track.rm("test_track1", TRUE)
    r_extract <- emr_extract("track0+2", keepref = TRUE, names = "test_track1")
    emr_track.create("test_track1", "user", FALSE, "track0+2", keepref = TRUE)
    expect_true(emr_track.exists("test_track1"))
    r_create <- emr_extract("test_track1", keepref = TRUE)
    expect_equal(r_extract, r_create)
    emr_track.rm("test_track1", TRUE)
    expect_false(emr_track.exists("test_track1"))
})

test_that("create and remove categorical multiple tracks", {
    emr_track.rm("test_track1", TRUE)
    emr_track.rm("test_track2", TRUE)
    r_extract <- emr_extract("track0+2", keepref = TRUE, names = "test_track1")
    emr_track.create("test_track1", "user", FALSE, "track0+2", keepref = TRUE)
    emr_track.create("test_track2", "user", FALSE, "track0+2", keepref = TRUE)
    expect_true(emr_track.exists("test_track1"))
    expect_true(emr_track.exists("test_track2"))
    r_create <- emr_extract("test_track1", keepref = TRUE)
    expect_equal(r_extract, r_create)
    emr_track.rm(c("test_track1", "test_track2"), TRUE)
    expect_false(emr_track.exists("test_track1"))
    expect_false(emr_track.exists("test_track2"))
})

test_that("create categorical keepref=FALSE", {
    emr_track.rm("test_track1", TRUE)
    r_extract <- emr_extract("track0+2", keepref = FALSE, names = "test_track1")
    emr_track.create("test_track1", "user", FALSE, "track0+2", keepref = FALSE)
    expect_true(emr_track.exists("test_track1"))
    withr::defer(emr_track.rm("test_track1", TRUE))

    r_create <- emr_extract("test_track1", keepref = TRUE)
    expect_equal(nrow(r_create), 99508)
    expect_equal(r_extract, r_create, tolerance = 1e-7)
})

test_that("create categorical with filter", {
    emr_track.rm("test_track1", TRUE)
    emr_track.create("test_track1", "user", FALSE, "track0", filter = "!track0")
    expect_true(emr_track.exists("test_track1"))
    withr::defer(emr_track.rm("test_track1", TRUE))

    r_create <- emr_extract("test_track1", keepref = TRUE)
    expect_equal(nrow(r_create), 0)
})

test_that("emr_track.mv works", {
    emr_track.rm("test_track1", TRUE)
    emr_track.create("test_track1", "user", FALSE, "track0+2", keepref = FALSE)
    emr_track.mv("test_track1", "test_track2")
    expect_false(emr_track.exists("test_track1"))
    expect_true(emr_track.exists("test_track2"))
    withr::defer(emr_track.rm("test_track2", TRUE))
})

test_that("emr_track.mv works with different values", {
    emr_track.rm("test_track1", TRUE)
    emr_track.create("test_track1", "global", FALSE, "track0+2", keepref = FALSE)
    emr_track.mv("test_track1", "test_track2", "user")
    expect_false(emr_track.exists("test_track1"))
    expect_true(emr_track.exists("test_track2"))
    withr::defer(emr_track.rm("test_track2", TRUE))
})

test_that("emr_track.mv moves track attribues as well", {
    emr_track.rm("test_track1", TRUE)
    emr_track.create("test_track1", "user", FALSE, "track0+2", keepref = FALSE)
    emr_track.attr.set("test_track1", "test_attr", "value")
    emr_track.mv("test_track1", "test_track2")
    expect_false(emr_track.exists("test_track1"))
    expect_true(emr_track.exists("test_track2"))
    expect_equal(emr_track.attr.get("test_track2", "test_attr"), "value")
    withr::defer(emr_track.rm("test_track2", TRUE))
})

test_that("emr_track.mv moves track vars as well", {
    emr_track.rm("test_track1", TRUE)
    emr_track.create("test_track1", "user", FALSE, "track0+2", keepref = FALSE)
    emr_track.var.set("test_track1", "test_var", 1:10)
    emr_track.mv("test_track1", "test_track2")
    expect_false(emr_track.exists("test_track1"))
    expect_true(emr_track.exists("test_track2"))
    expect_equal(emr_track.var.get("test_track2", "test_var"), 1:10)
    withr::defer(emr_track.rm("test_track2", TRUE))
})

test_that("emr_track.rm doesn't fail when given character(0)", {
    emr_track.rm(character(0))
    expect_true(TRUE)
})

test_that("emr_track.create errors instead of segfaulting on an empty expression", {
    # R_ParseVector returns PARSE_OK for input that parses to no expression at all, so before this
    # guard the C++ side read element 0 of a zero-length list and killed the R process outright
    # (exit 139, "caught segfault / address (nil), cause 'memory not mapped'"). An uncatchable
    # crash in a long-lived worker process is much worse than a failed call.
    # iterator= is essential to these being regression tests at all. Without it,
    # create_expr_iterator() cannot infer a policy from an expression that contributes no track
    # vars, so it errors *before* the guard under test is reached - and the assertions then pass
    # against unpatched naryn, testing nothing.
    emr_track.rm("test_track1", TRUE)
    for (bad in list(
        "",                 # zero-length parse
        "   ",              # ditto, whitespace
        "\t\n",            # ditto
        "# comment only",   # ditto, parses to nothing
        "NULL",             # length-1 parse whose only element IS R_NilValue
        " NULL ",
        ";",                # parse() throws; the error is swallowed by R_tryEval
        ";;",
        ",",
        "()",
        "dense_track +",    # a plausible user typo
        "'unterminated",
        "foo("
    )) {
        expect_error(
            emr_track.create("test_track1", "user", FALSE, bad, iterator = "dense_track"),
            info = sprintf("expression %s must error, not segfault", deparse(bad))
        )
    }
    expect_false(emr_track.exists("test_track1"))
})
