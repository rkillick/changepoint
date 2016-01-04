\name{cpts.full}
\alias{cpts.full}
\title{
Generic Function - cpts.full
}
\description{
Generic function
}
\usage{
cpts.full(object)
}
\arguments{
  \item{object}{
	Depending on the class of \code{object} depends on the method used (and if one exists)
}
}
\details{
	Generic function.
}
\value{
	Depends on the class of \code{object}, see individual methods
}
\author{
Rebecca Killick
}


\seealso{
\code{\link{cpts.full-methods}}
}
\examples{
x=new("cpt.range") # new cpt.range object
cpts.full(x) # retrieves the cpts.full slot from x
}

\keyword{methods}
\keyword{cpt}
\keyword{internal} 