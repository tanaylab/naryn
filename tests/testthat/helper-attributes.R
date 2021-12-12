clean_attributes <- function() {
    if (nrow(emr_track.attr.export())) {
        apply(emr_track.attr.export(), 1, function(x) {
            emr_track.attr.rm(x[1], x[2])
        })
    }
}
