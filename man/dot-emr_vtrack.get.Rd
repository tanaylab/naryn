% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vtrack.R
\name{.emr_vtrack.get}
\alias{.emr_vtrack.get}
\title{Get virtual track parameters given a string}
\usage{
.emr_vtrack.get(vtrackstr, adjust_logical = TRUE)
}
\arguments{
\item{vtrackstr}{name of the virtual track}

\item{adjust_logical}{when the source is logical track - adjust the parameters to imitate a physical track}
}
\value{
a list with the parameters of the virtual track
}
\description{
Get virtual track parameters given a string
}
\examples{
emr_db.init_examples()

emr_vtrack.create("vtrack1", "dense_track",
    time.shift = 1,
    func = "max"
)
.emr_vtrack.get("vtrack1")

}
\keyword{internal}
