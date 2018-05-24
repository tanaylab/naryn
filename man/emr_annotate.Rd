\name{emr_annotate}
\alias{emr_annotate}
\title{
Annotates id-time points table
}
\description{
Annotates id-time points table by the values given in the second table.
}
\usage{
emr_annotate(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{sorted id-time points table that is expanded}
  \item{y}{sorted id-time points table that is used for annotations}
}
\details{
This function merges two sorted id-time points tables 'x' and 'y' by matching 'id', 'time' and 'ref' columns. The result is a new id-time points table that has all the additional columns of 'x' and 'y'.

Two rows match if 'id' AND 'time' match AND either 'ref' matches OR one of the 'ref' is '-1'.

If a row RX from 'x' matches N rows RY1, ..., RYn from 'y', N rows are added to the result: [RX RY1], ..., [RX RYn].

If a row RX from 'x' does not match any rows from 'y', a row of [RX NA] form is added to the result (i.e. all the values of columns borrowed from 'y' are set to 'NA').

A missing 'ref' column is interpreted as if reference equals '-1'.

Both of 'x' and 'y' must be sorted by 'id', 'time' and 'ref' (in this order!). Note however that all the package functions (such as 'emr_extract', ...) return id-time point tables always properly sorted.
}
\value{
A data frame with all the columns from 'x' and additional columns from 'y'.
}

\seealso{
\code{\link{emr_extract}}
}
\examples{
emr_db.init_examples()

r1<-emr_extract("sparse_track", keepref=T)
r2<-emr_extract("dense_track", keepref=T)
r2$dense_track <- r2$dense_track + 1000
emr_annotate(r1, r2)
}
\keyword{ ~annotate }

