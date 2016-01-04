\name{data.set.ts}
\alias{data.set.ts}
\title{
Generic Function - data.set.ts
}
\description{
Generic function
}
\usage{
data.set.ts(object)
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
\code{\link{data.set.ts-methods}}
}
\examples{
x=new("cpt") # new cpt object
data.set.ts(x) # retrieves the data.set slot from x.  This is a ts object.
}

\keyword{methods}
\keyword{cpt}
\keyword{internal} 