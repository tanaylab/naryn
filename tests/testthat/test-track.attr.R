test_that("emr_track.attr.set fails when track doesn't exist", {
    expect_error(emr_track.attr.set('trackaaaa', 'var1', 'val1'))
})

test_that("emr_track.attr.export returns correct output", {
    if (nrow(emr_track.attr.export()))
        apply(emr_track.attr.export(), 1, function(x) { emr_track.attr.rm(x[1], x[2]) })
    emr_track.attr.set('track1', 'var1', 'val1')
    expect_equal(emr_track.attr.export(), structure(list(track = "track1", attr = "var1", value = "val1"), row.names = 1L, class = "data.frame"))
})

test_that("emr_track.attr.get returns correct output", {
    if (nrow(emr_track.attr.export())){
        apply(emr_track.attr.export(), 1, function(x) { emr_track.attr.rm(x[1], x[2]) })
    }
    emr_track.attr.set('track1', 'var1', 'val1')
    expect_equal(emr_track.attr.get('track1', 'var1'), "var1")
})