\name{emr_vtrack.attr.src}
\alias{emr_vtrack.attr.src}
\alias{emr_vtrack.attr.func}
\alias{emr_vtrack.attr.params}
\alias{emr_vtrack.attr.keepref}
\alias{emr_vtrack.attr.time.shift}
\alias{emr_vtrack.attr.id.map}
\alias{emr_vtrack.attr.filter}
\title{
Get or set attributes of a virtual track
}
\description{
Get or set attributes of a virtual track.
}
\usage{
emr_vtrack.attr.src(vtrack, src)
emr_vtrack.attr.func(vtrack, func)
emr_vtrack.attr.params(vtrack, params)
emr_vtrack.attr.keepref(vtrack, keepref)
emr_vtrack.attr.time.shift(vtrack, time.shift)
emr_vtrack.attr.id.map(vtrack, id.map)
emr_vtrack.attr.filter(vtrack, filter)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vtrack}{virtual track name.}
  \item{src, func, params, keepref, time.shift, id.map, filter}{virtual track attributes.}
}
\details{
When only 'vtrack' argument is used in the call, the functions return the corresponding attribute of the virtual track. Otherwise a new attribute value is set.

Note: since inter-dependency exists between certain attributes, the  correctness of the attributes as a whole can only be verified when the virtual track is used in a track expression.

For more information about the valid attribute values please refer to the documentation of 'emr_vtrack.create'.
}
\value{
None.
}

\seealso{
\code{\link{emr_vtrack.create}}
}
\examples{
emr_db.init_examples()
emr_vtrack.create("vtrack1", "dense_track")
emr_vtrack.attr.src("vtrack1")
emr_vtrack.attr.src("vtrack1", "sparse_track")
emr_vtrack.attr.src("vtrack1")
}
\keyword{ ~virtual }
