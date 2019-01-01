\name{emr_track.percentile}
\alias{emr_track.percentile}
\title{
Returns track percentile of the values
}
\description{
Returns track percentile of the values.
}
\usage{
emr_track.percentile(track, val, lower = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{track}{track name}
  \item{val}{vector of values}
  \item{lower}{how to calculate percentiles}
}
\details{
This function returns the percentiles of the values given in 'val' based on track data.

If 'lower' is 'TRUE' percentile indicates the relative number of track values lower than 'val'.
If 'lower' is 'FALSE' percentile reflects the relative number of track values lower or equal than 'val'.
}
\value{
A vector of percentile values
}

\seealso{
\code{\link{emr_track.unique}}
}
\examples{
emr_db.init_examples()

# percentiles of 30, 50
emr_track.percentile("dense_track", c(30, 50))

# calculate percentiles of track's earliest values in time window
emr_vtrack.create("v1", src = "dense_track", func = "earliest",
                  time.shift = c(-5, 5))
emr_extract(c("dense_track",
              "emr_track.percentile(\"dense_track\", v1, F)"),
              keepref = T, names =  c("col1", "col2"))
}
\keyword{ ~track }
\keyword{ ~percentile }
