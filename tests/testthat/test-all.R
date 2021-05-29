
# This is a dump from Misha's regression test. They need to be edited to testthat form,
# see test-track.attr.R for examples.



# test_that("test", emr_track.readonly("track0"))

# test_that("test", {
#     if (emr_track.exists("test_track1_ro")) {
#         emr_track.readonly("test_track1_ro", F)
#         emr_track.rm("test_track1_ro", T)
#     }
#     emr_track.create("test_track1_ro", "user", F, "track0+2", keepref=F)
#     emr_track.readonly("test_track1_ro", T)
#     r<-emr_track.readonly("test_track1_ro")
#     tryCatch({
#         emr_track.rm("test_track1_ro", T)
#         r<<-append(r, "oops - deleted R/O track")
#     },
#     error=function(cond) {
#         r<<-append(r, list(cond))
#     })
#     emr_track.readonly("test_track1_ro", F)
#     emr_track.rm("test_track1_ro", T)
#     r
# })

# test_that("test", {
#     emr_track.rm("test_track1", T)
#     emr_track.create("test_track1", "user", F, "track0+2", keepref=F)
#     r<-emr_track.ls()
#     emr_track.rm("test_track1", T)
#     list(emr_track.ls(), r)
# })

# test_that("test", emr_track.exists("track2"))

# test_that("test", emr_track.exists("sdaf"))

# test_that("test", emr_track.info("blabla"))

# test_that("test", {
#     track.info <- emr_track.info("track4")
#     track.info$path <- NULL
#     track.info
# })

# test_that("test", {
#     track.info <- emr_track.info("track1_sparse")
#     track.info$path <- NULL
#     track.info
# })

# test_that("test", emr_track.var.get("aaa", "blablablabla"))

# test_that("test", emr_track.var.get("track1", "blablablabla"))

# test_that("test", {
#     emr_track.var.set("track1", "v1", c(1,4))
#     r <- emr_track.var.get("track1", "v1")
#     emr_track.var.rm("track1", "v1")
#     r
# })

# test_that("test", {
#     emr_track.var.set("track10", "v1", c(1,4))
#     r <- emr_track.var.get("track10", "v1")
#     emr_track.var.rm("track10", "v1")
#     r
# })

# test_that("test", emr_track.var.ls("track1"))

# test_that("test", {
#     emr_track.var.set("track1", "v1", c(1,4))
#     emr_track.var.set("track1", "v2", 5)
#     r <- emr_track.var.ls("track1")
#     emr_track.var.rm("track1", "v1")
#     emr_track.var.rm("track1", "v2")
#     r
# })

# test_that("test", {
#     emr_track.var.set("track10", "v1", c(1,4))
#     emr_track.var.set("track10", "v2", 5)
#     r <- emr_track.var.ls("track10")
#     emr_track.var.rm("track10", "v1")
#     emr_track.var.rm("track10", "v2")
#     r
# })

# test_that("test", emr_track.var.rm("blabla", "v"))

# test_that("test", emr_track.var.rm("track1", "v"))

# test_that("test", {
#     emr_track.var.set("track1", "v1", c(1,4))
#     emr_track.var.set("track1", "v2", 5)
#     emr_track.var.rm("track1", "v1")
#     r <- list()
#     r[[1]] <- emr_track.var.ls("track1")
#     emr_track.var.rm("track1", "v2")
#     r[[2]] <- emr_track.var.ls("track1")
#     r
# })

# test_that("test", {
#     emr_track.var.set("track10", "v1", c(1,4))
#     emr_track.var.set("track10", "v2", 5)
#     emr_track.var.rm("track10", "v1")
#     r <- list()
#     r[[1]] <- emr_track.var.ls("track10")
#     emr_track.var.rm("track10", "v2")
#     r[[2]] <- emr_track.var.ls("track10")
#     r
# })

# test_that("test", emr_track.var.set("aaa", "blablablabla", 5))

# test_that("test", emr_track.percentile("track1", emr_extract("track2", iterator="track3")$track2, lower=T))

# test_that("test", emr_track.percentile("track1", emr_extract("track2", iterator="track3")$track2, lower=F))

# test_that("test", emr_track.unique("blabla"))

# test_that("test", emr_track.unique("track4"))

# test_that("test", emr_track.unique("track5"))

# test_that("test", emr_track.unique("track6"))

# test_that("test", {
#     r1<-emr_extract("track1", stime = 10, etime = 10000, keepref=T)
#     r2<-emr_extract("track2", stime = 10, etime = 10000, keepref=T)
#     emr_annotate(r1,r2)
# })

# test_that("test", {
#     r1<-emr_extract("track1", stime = 10, etime = 100, keepref=T)
#     r2<-emr_extract("track2", stime = 10, etime = 100, keepref=T)
#     r2$blabla <- paste("bla", 1:nrow(r2))
#     emr_annotate(r1,r2)
# })

# test_that("test", {
#     r1<-emr_extract("track1", stime = 10, etime = 100, keepref=T)
#     r2<-emr_extract("track2", stime = 10, etime = 100, keepref=T)
#     r2$blabla <- as.factor(paste("bla", 1:nrow(r2)))
#     emr_annotate(r1,r2)
# })

# test_that("test", {
#     r1<-emr_extract("track1", stime = 10, etime = 10000, keepref=T)
#     r2<-emr_extract("track2", stime = 10, etime = 10000, keepref=T)
#     emr_annotate(r2,r1)
# })

# test_that("test", {
#     r1<-emr_extract("track1", stime = 10, etime = 10000, keepref=F)
#     r2<-emr_extract("track2", stime = 10, etime = 10000, keepref=T)
#     emr_annotate(r1,r2)
# })

# test_that("test", {
#     r1<-emr_extract("track1", stime = 10, etime = 10000, keepref=F)
#     r2<-emr_extract("track2", stime = 10, etime = 10000, keepref=T)
#     emr_annotate(r2,r1)
# })

# test_that("test", {
#     r1<-emr_extract("track1", stime = 10, etime = 10000, keepref=T)
#     r2<-emr_extract("track2", stime = 10, etime = 10000, keepref=F)
#     emr_annotate(r1,r2)
# })

# test_that("test", {
#     r1<-emr_extract("track1", stime = 10, etime = 10000, keepref=T)
#     r2<-emr_extract("track2", stime = 10, etime = 10000, keepref=F)
#     emr_annotate(r2,r1)
# })

# test_that("test", {
#     r1<-emr_extract("track1", stime = 10, etime = 10000, keepref=F)
#     r2<-emr_extract("track2", stime = 10, etime = 10000, keepref=F)
#     emr_annotate(r1,r2)
# })

# test_that("test", {
#     r1<-emr_extract("track1", stime = 10, etime = 10000, keepref=F)
#     r2<-emr_extract("track2", stime = 10, etime = 10000, keepref=F)
#     emr_annotate(r2,r1)
# })

# test_that("test", {
#     r1<-emr_extract("track1", stime = 10, etime = 10000, keepref=T)
#     r1<-r1[sample(1:nrow(r1)),]
#     r2<-emr_extract("track2", stime = 10, etime = 10000, keepref=T)
#     emr_annotate(r1,r2)
# })

# test_that("test", {
#     r1<-emr_extract("track1", stime = 10, etime = 10000, keepref=T)
#     r2<-emr_extract("track2", stime = 10, etime = 10000, keepref=T)
#     r2<-r2[sample(1:nrow(r2)),]
#     emr_annotate(r1,r2)
# })

# test_that("test", {
#     r1<-emr_extract("track1", stime = 10, etime = 10000, keepref=T)
#     r1$ref<-NULL
#     r1<-r1[!duplicated(r1[,1:2]),]
#     r2<-emr_extract("track2", stime = 10, etime = 10000, keepref=T)
#     emr_annotate(r1,r2)
# })

# test_that("test", {
#     r1<-emr_extract("track1", stime = 10, etime = 10000, keepref=T)
#     r1$ref<-NULL
#     r1<-r1[!duplicated(r1[,1:2]),]
#     r2<-emr_extract("track2", stime = 10, etime = 10000, keepref=T)
#     emr_annotate(r2,r1)
# })

# test_that("test", emr_extract("bla"))

# test_that("test", emr_extract("track2", stime = 1000, etime = 10))

# test_that("test", emr_extract("track2", stime = 10, etime = 1000, keepref=F))

# test_that("test", emr_extract(c("track1", "track2"), stime = 10, etime = 1000))

# test_that("test", emr_extract(c("track1", "track2"), stime = 10, etime = 1000, iterator = 100))

# test_that("test", emr_extract(c("track1", "track2"), iterator="track0", tidy=T, keepref=T))

# test_that("test", emr_extract(c("track1", "track2"), iterator="track0", tidy=T, keepref=T, names=c("t1", "t2")))

# test_that("test", emr_extract(c("track1", "track2"), iterator="track0", keepref=T, names=c("t1", "t2")))

# test_that("test", {
#     i <- emr_screen("track1>990", stime = 500, etime = 2000)
#     emr_extract("track2", stime = 10, etime = 1000, keepref=F, iterator=i)
# })

# test_that("test", {
#     i <- emr_screen("track1>990", stime = 500, etime = 2000)
#     i$ref <- NULL
#     emr_extract("track2", stime = 10, etime = 1000, keepref=F, iterator=i)
# })

# test_that("test", {
#     i <- emr_screen("track1>990", stime = 500, etime = 2000)
#     emr_extract("track2", stime = 10, etime = 1000, keepref=T, iterator=i)
# })

# test_that("test", {
#     i <- emr_screen("track1>990", stime = 5000, etime = 20000)
#     emr_extract("track2", stime = 10, etime = 1000, keepref=F, iterator=i)
# })

# test_that("test", {
#     i <- emr_screen("track1>990", stime = 500, etime = 2000)
#     i <- data.frame(id = i$id)
#     emr_extract("track2", stime = 10, etime = 1000, keepref=T, iterator=i)
# })

# test_that("test", {
#     i <- emr_screen("track1>990", stime = 500, etime = 2000)
#     i <- data.frame(id = unique(i$id))
#     emr_extract("track2", stime = 10, etime = 11, keepref=T, iterator=i)
# })

# test_that("test", {
#     i <- emr_screen("track1>990", stime = 500, etime = 2000)
#     i <- data.frame(id = unique(i$id))
#     emr_extract("track2", stime = 10, etime = 20, keepref=F, iterator=i)
# })

# test_that("test", {
#     t <- data.frame(stime=c(5, 50), etime=c(10, 52))
#     emr_extract("track2", stime = 10, etime = 20, keepref=F, iterator=t)
# })

# test_that("test", {
#     t <- data.frame(stime=c(5, 50), etime=c(10, 52))
#     emr_extract("track2", stime = 10, etime = 20, keepref=T, iterator=t)
# })

# test_that("test", emr_extract("track2", stime = 10, etime = 1000, keepref=T))

# test_that("test", emr_extract("track2_sparse", stime = 10, etime = 1000, keepref=F))

# test_that("test", emr_extract("track2_sparse", stime = 10, etime = 1000, keepref=T))

# test_that("test", emr_extract("2 * track2", stime = 10, etime = 1000, keepref=T, filter="track1"))

# test_that("test", emr_extract("track2_sparse", stime = 10, etime = 1000, keepref=T, filter="track1_sparse"))

# test_that("test", {
#     n <- 10000
#     df <- data.frame(id=sample(c(0:1005), n, replace=T), stime=sample(c(0:11000), n, replace=T),etime=0)
#     df$etime <- df$stime+sample(c(0:1000), n, replace=T)
#     emr_extract("1", iterator=df)
# })

# test_that("test", {
#     n <- 10000
#     df <- data.frame(id=sample(c(0:1005), n, replace=T), stime=sample(c(0:11000), n, replace=T),etime=0)
#     df$etime <- df$stime+sample(c(0:1000), n, replace=T)
#     idx1 <- sample(1:n, n/2)
#     idx2 <- setdiff(1:n, idx1)
#     df1 <- df[idx1,]
#     df2 <- df[idx2,]
#     emr_extract("1", iterator=1, filter="df1|df2")
# })

# test_that("test", {
#     n <- 10000
#     df <- data.frame(id=sample(c(0:999), n, replace=T), stime=sample(c(0:11000), n, replace=T),etime=0)
#     df$etime <- df$stime+sample(c(0:1000), n, replace=T)
#     idx1 <- sample(1:n, n/2)
#     idx2 <- setdiff(1:n, idx1)
#     df1 <- df[idx1,]
#     df2 <- df[idx2,]
#     emr_extract("1", iterator=1, stime=100, etime=5000, filter="df1|df2")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track1", keepref=T)
#     emr_extract("track2", stime = 10, etime = 1000, keepref=T, filter="f1")
# })

# test_that("test", emr_filter.create("f1", "track1", keepref=T, time.shift=10))

# test_that("test", emr_filter.create("f1", "track1", val=c(3,5)))

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track8", val=c(3,5), time.shift=c(-10,10))
#     emr_extract("track7", keepref=T, filter="f1")
# })

# test_that("test", emr_filter.create("f1", "track8", expiration=-10))

# test_that("test", emr_filter.create("f1", "track8", keepref=T, expiration=100))

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track8", expiration=100)
#     emr_extract("track7", keepref=T, filter="f1")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track8", expiration=100, val=c(3,5))
#     emr_extract("track7", keepref=T, filter="f1")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track1", keepref=F, time.shift=10)
#     emr_extract("track2", stime = 10, etime = 1000, keepref=T, filter="f1")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track1", keepref=F, time.shift=c(10,20))
#     emr_extract("track2", stime = 10, etime = 1000, keepref=T, filter="f1")
# })

# test_that("test", emr_extract("track2", stime = 10, etime = 1000, keepref=T, filter="!track1"))

# test_that("test", emr_extract("track2", stime = 10, etime = 1000, keepref=T, filter="!track0 | !track1"))

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track1", keepref=T)
#     emr_extract("track2", stime = 10, etime = 1000, keepref=T, filter="!f1")
# })

# test_that("test", emr_extract("track2", stime = 10, etime = 1000, keepref=T, filter="track0 & track1"))

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track0", keepref=T)
#     emr_extract("track2", stime = 10, etime = 1000, keepref=T, filter="f1 & track1")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track0", keepref=T)
#     emr_extract("track2", stime = 10, etime = 1000, keepref=F, filter="(f1 | track1) & !track5")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track0", keepref=T)
#     emr_extract("track2", stime = 10, etime = 1000, keepref=T, filter="(f1 | track1) & !track5")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track0_sparse", keepref=T)
#     emr_extract("track2_sparse", stime = 10, etime = 1000, keepref=T, filter="(f1 | track1) & !track5_sparse")
# })

# test_that("test", {
#     i1 <- emr_extract("track1", keepref=T)
#     emr_extract("2 * track2", stime = 10, etime = 1000, keepref=T, filter="i1")
# })

# test_that("test", {
#     i1 <- emr_extract("track1", keepref=F)
#     emr_extract("2 * track2", stime = 10, etime = 1000, keepref=T, filter="i1")
# })

# test_that("test", {
#     i1 <- emr_extract("track1_sparse", keepref=F)
#     emr_extract("track2_sparse", stime = 10, etime = 1000, keepref=T, filter="i1")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     i1 <- emr_extract("track1", keepref=T)
#     emr_filter.create("f1", i1, keepref=T)
#     emr_extract("track2", stime = 10, etime = 1000, keepref=T, filter="f1")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     i1 <- emr_extract("track1", keepref=F)
#     emr_filter.create("f1", i1, keepref=T)
#     emr_extract("track2", stime = 10, etime = 1000, keepref=T, filter="f1")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     i1 <- emr_extract("track1", keepref=T)
#     emr_filter.create("f1", i1, keepref=T, time.shift=10)
#     emr_extract("track2", stime = 10, etime = 1000, keepref=T, filter="f1")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     i1 <- emr_extract("track1", keepref=F)
#     emr_filter.create("f1", i1, keepref=F, time.shift=10)
#     emr_extract("track2", stime = 10, etime = 1000, keepref=T, filter="f1")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     i1 <- emr_extract("track1", keepref=F)
#     emr_filter.create("f1", i1, keepref=F, time.shift=c(10,20))
#     emr_extract("track2", stime = 10, etime = 1000, keepref=T, filter="f1")
# })

# test_that("test", {
#     i1 <- emr_extract("track1", keepref=F)
#     emr_extract("track2", stime = 10, etime = 1000, keepref=T, filter="!i1")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     i1 <- emr_extract("track1", keepref=T)
#     emr_filter.create("f1", i1, keepref=T)
#     emr_extract("track2", stime = 10, etime = 1000, keepref=T, filter="!f1")
# })

# test_that("test", {
#     i0 <- emr_extract("track0", keepref=F)
#     i1 <- emr_extract("track1", keepref=F)
#     emr_extract("track2", stime = 10, etime = 1000, keepref=T, filter="i0 & i1")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     i0 <- emr_extract("track0", keepref=T)
#     i1 <- emr_extract("track1", keepref=F)
#     emr_filter.create("f1", i0, keepref=T)
#     emr_extract("track2", stime = 10, etime = 1000, keepref=T, filter="f1 & i1")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     i0 <- emr_extract("track0", keepref=T)
#     i1 <- emr_extract("track1", keepref=F)
#     emr_filter.create("f1", i0, keepref=T)
#     emr_extract("track2", stime = 10, etime = 1000, keepref=T, filter="f1 | i1")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     i0 <- emr_extract("track0", keepref=T)
#     i1 <- emr_extract("track1", keepref=F)
#     emr_filter.create("f1", i0, keepref=T)
#     emr_extract("track2", stime = 10, etime = 1000, keepref=F, filter="(f1 | i1) & !track5")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     i0 <- emr_extract("track0", keepref=T)
#     i1 <- emr_extract("track1", keepref=F)
#     emr_filter.create("f1", i0, keepref=T)
#     emr_extract("track2", stime = 10, etime = 1000, keepref=T, filter="(f1 | i1) & !track5")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     i1 <- emr_extract("track1", keepref=F)
#     emr_filter.create("f1", "track0_sparse", keepref=T)
#     emr_extract("track2_sparse", stime = 10, etime = 1000, keepref=T, filter="(f1 | i1) & !track5_sparse")
# })

# test_that("test", {
#     i1 <- emr_screen("track1>990", stime = 500, etime = 2000)
#     i1 <- data.frame(id = unique(i1$id))
#     emr_extract("2 * track2", stime = 10, etime = 1000, keepref=T, filter="i1")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     i1 <- emr_screen("track1>990", stime = 500, etime = 2000)
#     i1 <- data.frame(id = unique(i1$id))
#     emr_filter.create("f1", i1, keepref=T)
# })

# test_that("test", {
#     i1 <- emr_screen("track1>990", stime = 500, etime = 2000)
#     i1 <- data.frame(id = unique(i1$id))
#     emr_extract("2 * track2", stime = 10, etime = 1000, keepref=T, filter="i1")
# })

# test_that("test", {
#     i1 <- emr_screen("track1>990", stime = 500, etime = 2000)
#     i1 <- data.frame(id = unique(i1$id))
#     emr_extract("track2", stime = 10, etime = 1000, keepref=T, filter="!i1")
# })

# test_that("test", {
#     i0 <- emr_screen("track0>990", stime = 500, etime = 2000)
#     i0 <- data.frame(id = unique(i0$id))
#     i1 <- emr_screen("track1>990", stime = 500, etime = 2000)
#     i1 <- data.frame(id = unique(i1$id))
#     emr_extract("track2", stime = 10, etime = 1000, keepref=T, filter="i0 & i1")
# })

# test_that("test", {
#     i0 <- emr_screen("track0>990", stime = 500, etime = 2000)
#     i0 <- data.frame(id = unique(i0$id))
#     i1 <- emr_screen("track1>990", stime = 500, etime = 2000)
#     i1 <- data.frame(id = unique(i1$id))
#     emr_extract("track2", stime = 10, etime = 1000, keepref=T, filter="i0 | i1")
# })

# test_that("test", {
#     i0 <- emr_screen("track0>990", stime = 500, etime = 2000)
#     i0 <- data.frame(id = unique(i0$id))
#     i1 <- emr_screen("track1>990", stime = 500, etime = 2000)
#     i1 <- data.frame(id = unique(i1$id))
#     emr_extract("track2", stime = 10, etime = 1000, keepref=F, filter="(i0 | i1) & !track5")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     i0 <- emr_extract("track0", keepref=T)
#     i1 <- emr_screen("track1>990", stime = 500, etime = 2000)
#     i1 <- data.frame(id = unique(i1$id))
#     emr_filter.create("f1", i0, keepref=T)
#     emr_extract("track2", stime = 10, etime = 1000, keepref=T, filter="(f1 | i1) & !track5")
# })

# test_that("test", {
#     i0 <- emr_screen("track0>700", keepref=F)
#     i1 <- emr_screen("track0<300", keepref=F)
#     num.rows <- min(nrow(i0),nrow(i1))
#     t<-data.frame(stime=i0$time[1:num.rows], etime=i1$time[1:num.rows])
#     emr_extract("track2", stime = 10, etime = 20, keepref=T, filter="t")
# })

# test_that("test", {
#     i0 <- emr_screen("track5>990", keepref=F)
#     i1 <- emr_screen("track5<10", keepref=F)
#     num.rows <- min(nrow(i0),nrow(i1))
#     t<-data.frame(stime=pmin(i0$time[1:num.rows],i1$time[1:num.rows]), etime=pmax(i0$time[1:num.rows],i1$time[1:num.rows]))
#     emr_extract("track2", stime = 10, etime = 2000, keepref=T, filter="t")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     i0 <- emr_screen("track5>990", keepref=F)
#     i1 <- emr_screen("track5<10", keepref=F)
#     num.rows <- min(nrow(i0),nrow(i1))
#     t<-data.frame(stime=pmin(i0$time[1:num.rows],i1$time[1:num.rows]), etime=pmax(i0$time[1:num.rows],i1$time[1:num.rows]))
#     emr_filter.create("f1", t, keepref=F, time.shift=c(1000, 3000))
# })

# test_that("test", emr_extract("track1", iterator=1, keepref=F, stime=10, etime=15))

# test_that("test", emr_extract("track1", iterator=1, keepref=T, stime=10, etime=15))

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track0", keepref=F, time.shift=c(10, 50))
#     emr_filter.create("f2", "track3", keepref=T)
#     emr_extract("track1", iterator=1, keepref=T, filter="f1 & f2")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track0", keepref=F, time.shift=c(10, 50))
#     emr_filter.create("f2", "track3", keepref=T)
#     emr_extract("track1", iterator=1, stime=20, etime=5000, keepref=T, filter="f1 & f2")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track0", keepref=F, time.shift=c(10, 50))
#     emr_filter.create("f2", "track3", keepref=F)
#     emr_extract("track1", iterator=1, stime=20, etime=5000, keepref=T, filter="f1 & f2")
# })

# test_that("test", emr_extract("track1", iterator=2, keepref=F, stime=10, etime=15))

# test_that("test", emr_extract("track1", iterator=2, keepref=T, stime=10, etime=15))

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track0", keepref=F, time.shift=c(10, 50))
#     emr_filter.create("f2", "track3", keepref=T)
#     emr_extract("track1", iterator=2, stime=20, etime=5000, keepref=T, filter="f1 & f2")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track0", keepref=F, time.shift=c(10, 50))
#     emr_filter.create("f2", "track3", keepref=F)
#     emr_extract("track1", iterator=2, stime=20, etime=5000, keepref=T, filter="f1 & f2")
# })

# test_that("test", {
#     set.seed(0)
#     emr_extract("track1", iterator=list(2, data.frame(id=sample(1:1000, 500), time=sample(1:100, 500, replace=T))), stime=10, etime=15)
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_track.rm("test_track1", T)
#     emr_filter.create("f1", "track6", expiration=100000)
#     emr_track.create("test_track1", "user", T, "track6", keepref=T, filter="f1")
#     r<-emr_extract("track1", iterator=list(7, "test_track1"), stime=40, etime=190)
#     emr_track.rm("test_track1", T)
#     r
# })

# test_that("test", emr_cor("track0", c(0,10,500,1000), cor.exprs=c("track0", "track1", "track2", "track3"), iterator=1, stime=20, etime=5000, keepref=F))

# test_that("test", emr_dist("track2", c(100, 300, 500, 900, 2000, 3000)))

# test_that("test", emr_dist("track2", c(100, 300, 500, 900, 2000, 3000), keepref=T))

# test_that("test", emr_dist("track1", c(100, 300, 500, 900, 2000, 3000), "track2", c(50, 60, 80, 90)))

# test_that("test", emr_dist("track1", c(100, 300, 500, 900, 2000, 3000), "track2", c(50, 60, 80, 90), iterator="track1"))

# test_that("test", emr_dist("track1", NULL, "track2", c(50, 60, 80, 90), iterator="track1"))

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track7", func="dt1.earliest", time.shift=c(-10, 10))
#     emr_dist("v1", NULL, "track2", c(50, 60, 80, 90), iterator="track1")
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track7", func="frequent", time.shift=c(-10, 10))
#     emr_dist("v1", NULL, "track2", c(50, 60, 80, 90), iterator="track1")
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track7", func="sample", time.shift=c(-10, 10))
#     emr_dist("v1", NULL, "track2", c(50, 60, 80, 90), iterator="track1")
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track7", func="sample.time", time.shift=c(-10, 10))
#     emr_dist("v1", NULL, "track2", c(50, 60, 80, 90), iterator="track1")
# })

# test_that("test", emr_ids_coverage(data.frame(id=0:200), c("track7", "track6")))

# test_that("test", emr_ids_coverage(data.frame(id=0:200), c("track7", "track6"), filter="track2"))

# test_that("test", {
#     r <- emr_extract("track4")
#     emr_ids_coverage(r, c("track7", "track6"))
# })

# test_that("test", {
#     r <- emr_extract("track4")
#     emr_ids_coverage(r, c("track7", "track6"), filter="track2")
# })

# test_that("test", emr_ids_coverage("track5", c("track7", "track6"), filter="track1"))

# test_that("test", emr_ids_vals_coverage(data.frame(id=0:200), c("track7", "track6")))

# test_that("test", emr_ids_vals_coverage(data.frame(id=0:200), c("track7", "track6"), filter="track2"))

# test_that("test", {
#     r <- emr_extract("track4")
#     emr_ids_vals_coverage(r, c("track7", "track6"))
# })

# test_that("test", {
#     r <- emr_extract("track4")
#     emr_ids_vals_coverage(r, c("track7", "track6"), filter="track2")
# })

# test_that("test", emr_ids_vals_coverage("track5", c("track7", "track6"), filter="track1"))

# test_that("test", emr_quantiles("track1", c(0.1, 0.2, 0.5, 0.9)))

# test_that("test", emr_quantiles("track1 + track2", c(0.1, 0.2, 0.5, 0.9)))

# test_that("test", emr_quantiles("track1 + track2", c(0.1, 0.2, 0.5, 0.9), iterator="track2"))

# test_that("test", emr_screen("track1"))

# test_that("test", emr_screen("track1 < 0"))

# test_that("test", emr_screen("track1 > 990"))

# test_that("test", emr_summary("track1"))

# test_that("test", emr_summary("track4"))

# test_that("test", emr_date2time(0, 0, 0, 0))

# test_that("test", emr_date2time(1, 1, 1000, 0))

# test_that("test", emr_date2time(1, 1, 300000, 0))

# test_that("test", emr_date2time(1, 3, 1867, 0))

# test_that("test", emr_date2time(29, 2, 1868, 10))

# test_that("test", emr_date2time(28, 2, 1868, 10))

# test_that("test", emr_date2time(29, 2, 1869, 10))

# test_that("test", emr_date2time(15, c(2,5,6), 1869, 10))

# test_that("test", emr_date2time(15, c(2,5,6), 1869, c(10,20)))

# test_that("test", {
#     r <- emr_extract("track2_sparse", stime = 10, etime = 1000, keepref=F)
#     emr_time2dayofmonth(r$time)
# })

# test_that("test", {
#     r <- emr_extract("track2_sparse", stime = 10, etime = 1000, keepref=F)
#     emr_time2hour(r$time)
# })

# test_that("test", {
#     r <- emr_extract("track2_sparse", stime = 10, etime = 1000, keepref=F)
#     emr_time2month(r$time)
# })

# test_that("test", {
#     r <- emr_extract("track2_sparse", stime = 10, etime = 1000, keepref=F)
#     emr_time2year(r$time)
# })

# test_that("test", emr_vtrack.create("v1", "blabla"))

# test_that("test", emr_vtrack.create("v1", "track1", func="min", params=2))

# test_that("test", emr_vtrack.create("v1", "track1", func="min", keepref=T))

# test_that("test", emr_vtrack.create("v1", "track1", func="value"))

# test_that("test", emr_vtrack.create("v1", "track6", func="min"))

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track6", func="value")
#     emr_extract("v1")
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track7", func="value")
#     emr_extract("v1")
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track6", func="exists")
#     emr_extract("v1")
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track7", func="exists", params=c(1:8), time.shift=c(-20, 30))
#     emr_extract("v1")
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track7", func="frequent", time.shift=c(-100, 200))
#     emr_extract("v1")
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track7", func="frequent", params=c(2:5), time.shift=c(-100, 200))
#     emr_extract("v1")
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track7", func="size", time.shift=c(-100, 200))
#     emr_extract("v1")
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track7", func="size", params=c(2:5), time.shift=c(-100, 200))
#     emr_extract("v1")
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track0", func="size", time.shift=c(-100, 200))
#     emr_extract("v1")
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track0", func="size", params=c(2:5), time.shift=c(-100, 200))
#     emr_extract("v1")
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track8", func="earliest", time.shift=c(-100, 200))
#     emr_extract("v1", stime=100, etime=500)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track8", func="earliest", params=c(2:5), time.shift=c(-100, 200))
#     emr_extract("v1", stime=100, etime=500)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track8", func="latest", time.shift=c(-100, 200))
#     emr_extract("v1", stime=100, etime=500)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track8", func="latest", params=c(2:5), time.shift=c(-100, 200))
#     emr_extract("v1", stime=100, etime=500)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track8", func="closest", time.shift=c(-100, 200))
#     emr_extract("v1", stime=100, etime=500)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track8", func="closest", params=c(2:5), time.shift=c(-100, 200))
#     emr_extract("v1", stime=100, etime=500)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track0", func="stddev")
#     emr_extract("v1", stime=100, etime=500)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track0", func="stddev", time.shift=c(-100, 200))
#     emr_extract("v1", stime=100, etime=500)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track1")
#     emr_extract("v1")
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track1", keepref=T)
#     emr_extract("v1")
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track1", keepref=T)
#     emr_extract("v1", keepref=T)
# })

# test_that("test", emr_vtrack.create("v1", "track1", func="max", keepref=T))

# test_that("test", emr_vtrack.create("v1", "track1", func="avg", keepref=T, time.shift=2))

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track1", func="percentile.upper", keepref=T)
#     emr_extract("v1", keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track1", func="percentile.lower", keepref=T)
#     emr_extract("v1", keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track2", func="avg", keepref=F, time.shift=2)
#     emr_extract("v1", keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track2", func="avg", keepref=F, time.shift=c(100000, 200000))
#     emr_extract("v1", stime=10, etime=500, keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track2", func="avg", keepref=F, time.shift=c(-10, 20))
#     emr_extract("v1", stime=10, etime=500, keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track2", func="min", keepref=F, time.shift=c(-10, 20))
#     emr_extract("v1", stime=10, etime=500, keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track2", func="max", keepref=F, time.shift=c(-10, 20))
#     emr_extract("v1", stime=10, etime=500, keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track2", func="earliest", keepref=F, time.shift=c(-10, 20))
#     emr_extract("v1", stime=10, etime=500, keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track2", func="latest", keepref=F, time.shift=c(-10, 20))
#     emr_extract("v1", stime=10, etime=500, keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track2", func="closest", keepref=F, time.shift=c(-10, 20))
#     emr_extract("v1", stime=10, etime=500, keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track2", func="earliest.time", keepref=F, time.shift=c(-10, 20))
#     emr_extract("v1", stime=10, etime=500, keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track2", func="latest.time", keepref=F, time.shift=c(-10, 20))
#     emr_extract("v1", stime=10, etime=500, keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track2", func="closest.earlier.time", keepref=F, time.shift=c(-10, 20))
#     emr_extract("v1", stime=10, etime=500, keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track2", func="closest.later.time", keepref=F, time.shift=c(-10, 20))
#     emr_extract("v1", stime=10, etime=500, keepref=T)
# })

# test_that("test", emr_vtrack.create("v1", "track2", func="quantile", keepref=F, time.shift=c(-10, 20)))

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track2", func="quantile", keepref=F, time.shift=c(-10, 20), params=0.5)
#     emr_extract("v1", stime=10, etime=500, keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track2", func="sum", keepref=F, time.shift=c(-10, 20))
#     emr_extract("v1", stime=10, etime=500, keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track2", func="lm.intercept", keepref=F, time.shift=c(-10, 20))
#     emr_extract("v1", stime=10, etime=500, keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track2", func="lm.slope", keepref=F, time.shift=c(-10, 20))
#     emr_extract("v1", stime=10, etime=500, keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track2", func="percentile.upper", keepref=F, time.shift=c(-10, 20))
#     emr_extract("v1", stime=10, etime=500, keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track2", func="percentile.upper.min", keepref=F, time.shift=c(-10, 20))
#     emr_extract("v1", stime=10, etime=500, keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track2", func="percentile.upper.max", keepref=F, time.shift=c(-10, 20))
#     emr_extract("v1", stime=10, etime=500, keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track2", func="percentile.lower", keepref=F, time.shift=c(-10, 20))
#     emr_extract("v1", stime=10, etime=500, keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track2", func="percentile.lower.min", keepref=F, time.shift=c(-10, 20))
#     emr_extract("v1", stime=10, etime=500, keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track2", func="percentile.lower.max", keepref=F, time.shift=c(-10, 20))
#     emr_extract("v1", stime=10, etime=500, keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track2", func="dt1.earliest", keepref=F, time.shift=c(-10, 20))
#     emr_extract("v1", stime=10, etime=500, keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track2", func="dt1.latest", keepref=F, time.shift=c(-10, 20))
#     emr_extract("v1", stime=10, etime=500, keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track2", func="dt2.earliest", keepref=F, time.shift=c(-10, 20))
#     emr_extract("v1", stime=10, etime=500, keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track2", func="dt2.latest", keepref=F, time.shift=c(-10, 20))
#     emr_extract("v1", stime=10, etime=500, keepref=T)
# })

# test_that("test", {
#     set.seed(0)
#     id2id <- data.frame(id1=sample(0:999, 500), id2=sample(0:999, 500))
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track1", func="avg", id.map=id2id)
#     emr_extract("v1", iterator="track2")
# })

# test_that("test", {
#     set.seed(0)
#     id2id <- data.frame(id1=sample(0:999, 500), id2=sample(0:999, 500), time.shift=sample(-100:200, 500, replace=T))
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track1", func="avg", id.map=id2id)
#     emr_extract("v1", iterator="track2")
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track1", filter="!track0")
#     emr_extract("v1")
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track1", filter="track0")
#     emr_extract("v1")
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track1", filter="!track0", func="percentile.upper")
#     emr_extract("v1")
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track1", filter="track0", func="percentile.upper")
#     emr_extract("v1")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track0", keepref=T)
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track1", filter="!f1", keepref=T)
#     emr_extract("v1", keepref=T)
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track0", keepref=T)
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track1", filter="f1", keepref=T)
#     emr_extract("v1", keepref=T)
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track0", keepref=T)
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track1", filter="!f1", keepref=T, func="percentile.upper")
#     emr_extract("v1", keepref=T)
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track0", keepref=T)
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track1", filter="f1", keepref=T, func="percentile.upper")
#     emr_extract("v1", keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     r <- emr_extract("track0", keepref=T, names="value")
#     emr_vtrack.create("v1", list(r,T), func="max", time.shift=c(-10,20))
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     r <- emr_extract("track0", keepref=T, names="value")
#     emr_vtrack.create("v1", list(r,F), func="max", time.shift=c(-10,20))
#     emr_extract("v1", keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     r <- emr_extract("track0", keepref=T, names="value")
#     emr_vtrack.create("v1", list(r,T), func="closest", time.shift=c(-10,20))
#     emr_extract("v1", keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     r <- emr_extract("track0", keepref=F, names="value")
#     r$ref <- NULL
#     emr_vtrack.create("v1", list(r,F), func="max", time.shift=c(-10,20))
#     emr_extract("v1", keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     r <- emr_extract("track0", keepref=F, names="value")
#     r$ref <- NULL
#     emr_vtrack.create("v1", list(r,T), func="closest", time.shift=c(-10,20))
#     emr_extract("v1", keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     r <- emr_extract("track0", keepref=F, names="value")
#     emr_vtrack.create("v1", list(r,F), func="percentile.upper", time.shift=c(-10,20))
#     emr_extract("v1", keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     r <- emr_extract("track0", keepref=F, names="value")
#     emr_vtrack.create("v1", list(r,F), func="percentile.upper", time.shift=c(-10,20), filter="track1")
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     r <- emr_extract("track0", keepref=F, names="value")
#     emr_vtrack.create("v1", list(r,F), func="percentile.upper", time.shift=c(-10,20))
#     emr_extract(c("v1", "track1"), keepref=T)
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track1")
#     emr_vtrack.attr.src("v1", "track2")
#     emr_vtrack.attr.src("v1")
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track1")
#     emr_vtrack.attr.src("v1", "track10")
#     emr_vtrack.attr.src("v1")
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track10")
#     r <- emr_extract("track0", keepref=F, names="value")
#     emr_vtrack.attr.src("v1", list(head(r), F))
#     emr_vtrack.attr.src("v1")
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track1")
#     emr_vtrack.attr.func("v1", "value")
#     emr_vtrack.attr.func("v1")
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track1")
#     emr_vtrack.attr.params("v1", 26)
#     emr_vtrack.attr.params("v1")
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track1")
#     emr_vtrack.attr.keepref("v1", T)
#     emr_vtrack.attr.keepref("v1")
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track1")
#     emr_vtrack.attr.time.shift("v1", c(-10,20))
#     emr_vtrack.attr.time.shift("v1")
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track1")
#     emr_vtrack.attr.id.map("v1", data.frame(id1=10,id2=20))
#     emr_vtrack.attr.id.map("v1")
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track1")
#     emr_vtrack.attr.filter("v1", "track2 & track3")
#     emr_vtrack.attr.filter("v1")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track1", time.shift=c(-10,20))
#     emr_filter.attr.src("f1", "track2")
#     emr_filter.attr.src("f1")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track1", time.shift=c(-10,20))
#     emr_filter.attr.src("f1", "track10")
#     emr_filter.attr.src("f1")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track10", time.shift=c(-10,20))
#     emr_filter.attr.src("f1", data.frame(id=c(1,3), time=c(10,30)))
#     emr_filter.attr.src("f1")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track10", time.shift=c(-10,20))
#     emr_filter.attr.src("f1", data.frame(bla=2))
#     emr_filter.attr.src("f1")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track1", time.shift=c(-10,20))
#     emr_filter.attr.keepref("f1", T)
#     emr_filter.attr.keepref("f1")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track1", time.shift=c(-10,20))
#     emr_filter.attr.time.shift("f1", c(-17,30))
#     emr_filter.attr.time.shift("f1")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track1", time.shift=c(-10,20))
#     emr_filter.attr.val("f1", 500)
#     emr_filter.attr.val("f1")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track1", time.shift=c(-10,20))
#     emr_filter.attr.expiration("f1", 300)
#     emr_filter.attr.expiration("f1")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track2", keepref=F, time.shift=c(-10, 20))
#     emr_filter.create("f2", "track2", keepref=F, time.shift=c(-10, 30))
#     emr_filter.exists("f1")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track2", keepref=F, time.shift=c(-10, 20))
#     emr_filter.create("f2", "track2", keepref=F, time.shift=c(-10, 30))
#     emr_filter.exists("sdaf")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track2", keepref=F, time.shift=c(-10, 20))
#     emr_filter.info("f1")
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track2", keepref=F, time.shift=c(-10, 20))
#     emr_filter.create("f2", "track2", keepref=F, time.shift=c(-10, 30))
#     emr_filter.ls()
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track10", keepref=F, time.shift=c(-10, 20))
#     emr_filter.create("f2", "track1", keepref=F, time.shift=c(-10, 30))
#     emr_filter.ls()
# })

# test_that("test", {
#     EMR_FILTERS <<- list()
#     emr_filter.create("f1", "track2", keepref=F, time.shift=c(-10, 20))
#     emr_filter.create("f2", "track2", keepref=F, time.shift=c(-10, 30))
#     emr_filter.rm("f1")
#     emr_filter.ls()
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track2", func="dt2.latest", keepref=F, time.shift=c(-10, 20))
#     emr_vtrack.create("v2", "track2", func="dt2.latest", keepref=F, time.shift=c(-10, 20))
#     emr_vtrack.exists("v1")
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track2", func="dt2.latest", keepref=F, time.shift=c(-10, 20))
#     emr_vtrack.create("v2", "track2", func="dt2.latest", keepref=F, time.shift=c(-10, 20))
#     emr_vtrack.exists("sdaf")
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track2", func="dt2.latest", keepref=F, time.shift=c(-10, 20))
#     emr_vtrack.info("v1")
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track2", func="dt2.latest", keepref=F, time.shift=c(-10, 20))
#     emr_vtrack.create("v2", "track2", func="dt2.latest", keepref=F, time.shift=c(-10, 20))
#     emr_vtrack.ls()
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track10")
#     emr_vtrack.create("v2", "track1")
#     emr_vtrack.ls()
# })

# test_that("test", {
#     EMR_VTRACKS <<- list()
#     emr_vtrack.create("v1", "track2", func="dt2.latest", keepref=F, time.shift=c(-10, 20))
#     emr_vtrack.create("v2", "track2", func="dt2.latest", keepref=F, time.shift=c(-10, 20))
#     emr_vtrack.rm("v1")
#     emr_vtrack.ls()
# })

# test_that("test", {
#     options(emr_multitasking=T)
#     r<-emr_summary("track2", iterator=4, stime=20, etime=9000, keepref=T, filter="track0|track1|track3|track4|track5|track6|track7|track8")
#     options(emr_multitasking=F)
#     r
# })

# test_that("test", {
#     options(emr_multitasking=T)
#     options(emr_max.data.size=200000000)
#     r<-emr_extract("track2", sort=T, iterator=4, stime=20, etime=9000, keepref=T, filter="track0|track1|track3|track4|track5|track6|track7|track8")
#     options(emr_multitasking=F)
#     options(emr_max.data.size=10000000)
#     r
# })
