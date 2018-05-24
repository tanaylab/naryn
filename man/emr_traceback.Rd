\name{emr_traceback}
\alias{emr_traceback}
\title{
Prints call stack of the last uncaught error
}
\description{
Prints call stack of the last uncaught error in a friendly way.
}
\usage{
emr_traceback(x = NULL, max.lines = getOption("deparse.max.lines"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{see 'traceback'}
  \item{max.lines}{see 'traceback'}
}
\details{
Similarly to 'traceback' this function prints the call stack of the last uncaught error. Yet 'emr_traceback' does it in a more friendly way by omitting the calls that occurred inside the library.
}
\value{
See 'traceback'.
}

\seealso{
\code{\link{traceback}}
}
\examples{
emr_db.init_examples()
f <- function() { emr_screen("blablabla") }
f()
emr_traceback()
}
\keyword{ ~trace }
\keyword{ ~error }
\keyword{ ~exception }
