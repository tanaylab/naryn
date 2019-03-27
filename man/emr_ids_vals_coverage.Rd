\name{emr_ids_vals_coverage}
\alias{emr_ids_vals_coverage}
\title{
Returns ids coverage per value track
}
\description{
Returns ids coverage per value track. 
}
\usage{
emr_ids_vals_coverage(ids, tracks, stime = NULL, etime = NULL,
                      filter = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ids}{track name or Ids Table}
  \item{tracks}{a vector of track names}
  \item{stime}{start time scope}
  \item{etime}{end time scope}
  \item{filter}{iterator filter}
}
\details{
This function accepts a set of ids and a vector of categorical tracks. For each track value it calculates how many ids share this value. Each id is counted only once. A data frame with 3 columns 'track', 'val' and 'count' is returned.

Ids can originate from a track or be provided within Ids Table.

Note: The internal iterator that runs over each track is defined with 'keepref=T'.
}
\value{
A data frame containing the number of ids for each track value.
}

\seealso{
\code{\link{emr_ids_coverage}}, \code{\link{emr_track.ids}}, \code{\link{emr_dist}}
}
\examples{
emr_db.init_examples()
emr_ids_vals_coverage(data.frame(id=c(15,24,27)), "categorical_track")
}
\keyword{ ~coverage }
