\name{emr_filter.attr.src}
\alias{emr_filter.attr.src}
\alias{emr_filter.attr.keepref}
\alias{emr_filter.attr.time.shift}
\alias{emr_filter.attr.val}
\alias{emr_filter.attr.expiration}
\title{
Get or set attributes of a named filter
}
\description{
Get or set attributes of a named filter.
}
\usage{
emr_filter.attr.src(filter, src)
emr_filter.attr.keepref(filter, keepref)
emr_filter.attr.time.shift(filter, time.shift)
emr_filter.attr.val(filter, val)
emr_filter.attr.expiration(filter, expiration)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filter}{filter name.}
  \item{src, keepref, time.shift, val, expiration}{filter attributes.}
}
\details{
When only 'filter' argument is used in the call, the functions return the corresponding attribute of the named filter. Otherwise a new attribute value is set.

Note: since inter-dependency exists between certain attributes, the  correctness of the attributes as a whole can only be verified when the named filter is applied to a track expression.

For more information about the valid attribute values please refer to the documentation of 'emr_filter.create'.
}
\value{
None.
}

\seealso{
\code{\link{emr_filter.create}}
}
\examples{
emr_db.init_examples()
emr_filter.create("f1", "dense_track", time.shift = c(2,4))
emr_filter.attr.src("f1")
emr_filter.attr.src("f1", "sparse_track")
emr_filter.attr.src("f1")
}
\keyword{ ~filter }
