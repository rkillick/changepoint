\name{test.stat}
\alias{test.stat}
\title{
Generic Function - test.stat
}
\description{
Generic function
}
\usage{
test.stat(object)
}
\arguments{
  \item{object}{
	Depending on the class of \code{object} depends on the method used (and if one exists)
}
}
\details{
	Generic Function
}
\value{
	Depends on the class of \code{object}, see individual methods
}
\author{
Rebecca Killick
}


\seealso{
\code{\link{test.stat-methods}}
}
\examples{
x=new("cpt") # new cpt object
test.stat(x) # retrieves the test.stat slot from x
}

\keyword{methods}
\keyword{cpt}
\keyword{internal}