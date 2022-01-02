load_minimock_db()

test_that("filters with value", {
    withr::local_options(list(emr_max.data.size = 1e9))
    abnormal_hemoglobin <- emr_screen("lab.103 < 12", keepref = TRUE)
    a <- emr_extract("lab.103", iterator = abnormal_hemoglobin)

    emr_filter.create("f", "lab.103", val = 12, operator = "<")
    b <- emr_extract("lab.103", filter = "f")
    expect_equal(a, b)
})

test_that("filters on vtracks #1", {
    emr_filter.clear()
    emr_vtrack.clear()

    withr::local_options(list(emr_max.data.size = 1e9))
    emr_filter.create("female", "patients.female", time.shift = c(-years(120), 0))
    emr_filter.create("male", "patients.male", time.shift = c(-years(120), 0))
    hgb_female <- emr_screen("lab.103 < 12", filter = "female", keepref = TRUE)
    hgb_male <- emr_screen("lab.103 < 14", filter = "male", keepref = TRUE)
    emr_filter.create("abnormal_hgb_female", hgb_female %>% dplyr::distinct(id, time))
    emr_filter.create("abnormal_hgb_male", hgb_male %>% dplyr::distinct(id, time))
    emr_track.create("anemia",
        categorical = FALSE, expr = "lab.103",
        filter = "abnormal_hgb_female | abnormal_hgb_male"
    )
    withr::defer(emr_track.rm("anemia", force = TRUE))

    a <- emr_extract("anemia", names = "v")

    emr_filter.create("female", "patients.female", time.shift = c(-years(120), 0))
    emr_filter.create("male", "patients.male", time.shift = c(-years(120), 0))
    emr_filter.create("abnormal_hgb_female", src = "lab.103", val = 12, operator = "<")
    emr_filter.create("abnormal_hgb_male", src = "lab.103", val = 14, operator = "<")
    emr_track.create("anemia1",
        categorical = FALSE, expr = "lab.103",
        filter = "(female & abnormal_hgb_female) | (male & abnormal_hgb_male)"
    )
    withr::defer(emr_track.rm("anemia1", force = TRUE))
    b <- emr_extract("anemia1", names = "v")
    expect_equal(a, b)
})

test_that("filters on vtracks #2", {
    emr_filter.clear()
    emr_vtrack.clear()

    withr::local_options(list(emr_max.data.size = 1e9))
    emr_filter.create("female", "patients.female", time.shift = c(-years(120), 0))
    emr_filter.create("male", "patients.male", time.shift = c(-years(120), 0))
    hct_female_48 <- emr_screen("lab.104 > 48", filter = "female", keepref = TRUE)
    hct_male_49 <- emr_screen("lab.104 > 49", filter = "male", keepref = TRUE)
    emr_filter.create("abnormal_hct_female_past", hct_female_48 %>% dplyr::distinct(id, time), time.shift = c(-years(3), -1))
    emr_filter.create("abnormal_hct_male_past", hct_male_49 %>% dplyr::distinct(id, time), time.shift = c(-years(3), -1))
    emr_filter.create("abnormal_hct_female_current", hct_female_48 %>% dplyr::distinct(id, time))
    emr_filter.create("abnormal_hct_male_current", hct_male_49 %>% dplyr::distinct(id, time))
    emr_track.create("abnormal_hct_second_time",
        categorical = FALSE, expr = "lab.104",
        filter = "(abnormal_hct_female_past & abnormal_hct_female_current) | (abnormal_hct_male_past & abnormal_hct_male_current)"
    )
    withr::defer(emr_track.rm("abnormal_hct_second_time", force = TRUE))

    a <- emr_extract("abnormal_hct_second_time", names = "v")

    emr_filter.clear()
    emr_filter.create("female", "patients.female", time.shift = c(-years(120), 0))
    emr_filter.create("male", "patients.male", time.shift = c(-years(120), 0))
    emr_filter.create("abnormal_hct_female", src = "lab.104", val = 48, operator = ">")
    emr_filter.create("abnormal_hct_male", src = "lab.104", val = 49, operator = ">")
    emr_vtrack.create("abnormal_hct_past", src = "lab.104", filter = "(female & abnormal_hct_female) | (male & abnormal_hct_male)", time.shift = c(-years(3), -1))
    emr_track.create("abnormal_hct_second_time1",
        categorical = FALSE, expr = "lab.104",
        filter = "abnormal_hct_past & ((female & abnormal_hct_female) | (male & abnormal_hct_male))", iterator = "lab.104"
    )
    withr::defer(emr_track.rm("abnormal_hct_second_time1", force = TRUE))

    b <- emr_extract("abnormal_hct_second_time1", names = "v")
    expect_equal(a, b)

    emr_filter.clear()
    emr_filter.create("female", "patients.female", time.shift = c(-years(120), 0))
    emr_filter.create("male", "patients.male", time.shift = c(-years(120), 0))
    emr_filter.create("abnormal_hct_female", src = "lab.104", val = 48, operator = ">")
    emr_filter.create("abnormal_hct_male", src = "lab.104", val = 49, operator = ">")
    emr_vtrack.create("num_abnormal_hct", src = "lab.104", func = "size", filter = "(female & abnormal_hct_female) | (male & abnormal_hct_male)", time.shift = c(-years(3), 0))
    emr_filter.create("abnormal_hct_twice", src = "num_abnormal_hct", val = 2, operator = ">=")
    emr_track.create("abnormal_hct_second_time2", categorical = FALSE, expr = "lab.104", filter = "abnormal_hct_twice & ((female & abnormal_hct_female) | (male & abnormal_hct_male))", iterator = "lab.104")
    withr::defer(emr_track.rm("abnormal_hct_second_time2", force = TRUE))

    d <- emr_extract("abnormal_hct_second_time2", names = "v")
    expect_equal(a, d)
})
