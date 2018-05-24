\name{emr_ids_coverage}
\alias{emr_ids_coverage}
\title{
Returns ids coverage per track
}
\description{
Returns ids coverage per track. 
}
\usage{
emr_ids_coverage(ids, tracks, stime = MINTIME, etime = MAXTIME,
                 filter = NULL))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ids}{track name, Ids table or Id-Time Points table}
  \item{tracks}{a vector of track names}
  \item{stime}{start time scope}
  \item{etime}{end time scope}
  \item{filter}{iterator filter}
}
\details{
This function accepts a set of ids and a vector of categorial tracks. For each track it calculates how many ids appear in the track. Each id is counted only once.

Ids can come from various source such as a track or an Ids Table.

Note: The internal iterator that runs over each track is defined with 'keepref=T'.
}
\value{
A vector containing the ids count for each track.
}

\seealso{
\code{\link{emr_ids_vals_coverage}}, \code{\link{emr_track.ids}}, \code{\link{emr_dist}}
}
\examples{
emr_db.init_examples()
emr_ids_coverage(data.frame(id=c(24,27)), "categorial_track")
}
\keyword{ ~coverage }
