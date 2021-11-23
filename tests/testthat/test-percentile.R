
test_that("emr_track.percentile works", {
    expect_equal(
        emr_track.percentile("track1", emr_extract("track2", iterator = "track3")$track2, lower = T),
        c(
            NaN, NaN, 0.462709993124008, NaN, 0.811730027198792, NaN, 0.428662002086639,
            NaN, NaN, NaN, NaN, 0.522267997264862, NaN, NaN, NaN, NaN, 0.65066397190094,
            NaN, NaN, NaN, NaN, 0.800718009471893, 0.552169978618622, 0.353909999132156,
            NaN, NaN, NaN, NaN, 0.712692022323608, 0.472631990909576, NaN,
            NaN, NaN, NaN, 0.356002002954483, NaN, NaN, NaN, 0.0451259985566139,
            NaN, NaN, NaN, 0.038162000477314, NaN, NaN, NaN, NaN, NaN, NaN,
            NaN, NaN, NaN, NaN, NaN, 0.689742028713226, NaN, NaN, NaN, NaN,
            0.0180840007960796, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN,
            0.622633993625641, NaN, NaN, NaN, 0.0762720033526421, 0.833428025245667,
            NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 0.424607992172241,
            NaN, NaN, NaN, 0.889331996440887, NaN, NaN, NaN, NaN, 0.215777993202209,
            NaN, 0.826496005058289, NaN, 0.0281680002808571, NaN, NaN
        )
    )
})

test_that("emr_track.percentile works lower=F", {
    expect_equal(
        emr_track.percentile("track1", emr_extract("track2", iterator = "track3")$track2, lower = F),
        c(
            NaN, NaN, 0.463672012090683, NaN, 0.812665998935699, NaN, 0.429715991020203,
            NaN, NaN, NaN, NaN, 0.523307979106903, NaN, NaN, NaN, NaN, 0.651678025722504,
            NaN, NaN, NaN, NaN, 0.801657974720001, 0.553193986415863, 0.354934006929398,
            NaN, NaN, NaN, NaN, 0.713649988174438, 0.473630011081696, NaN,
            NaN, NaN, NaN, 0.357012003660202, NaN, NaN, NaN, 0.0461059994995594,
            NaN, NaN, NaN, 0.0391619987785816, NaN, NaN, NaN, NaN, NaN, NaN,
            NaN, NaN, NaN, NaN, NaN, 0.690774023532867, NaN, NaN, NaN, NaN,
            0.0190999992191792, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN,
            0.623579978942871, NaN, NaN, NaN, 0.0772219970822334, 0.834415972232819,
            NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, NaN, 0.425666004419327,
            NaN, NaN, NaN, 0.890339970588684, NaN, NaN, NaN, NaN, 0.216744005680084,
            NaN, 0.827467978000641, NaN, 0.029101999476552, NaN, NaN
        )
    )
})

test_that("emr_track.percentile fails on categorical tracks", {
    expect_error(emr_track.percentile("ph1", 15))
})
