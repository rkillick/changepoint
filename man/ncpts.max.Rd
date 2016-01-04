\name{ncpts.max}
\alias{ncpts.max}
\title{
Generic Function - ncpts.max
}
\description{
Generic function
}
\usage{
ncpts.max(object)
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
\code{\link{ncpts.max-methods}}
}
\examples{
x=new("cpt") # new cpt object
ncpts.max(x) # retrieves the ncpts.max slot from x
}

\keyword{methods}
\keyword{cpt}
\keyword{internal}