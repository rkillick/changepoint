\name{pen.type}
\alias{pen.type}
\title{
Generic Function - pen.type
}
\description{
Generic function
}
\usage{
pen.type(object)
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
\code{\link{pen.type-methods}}
}
\examples{
x=new("cpt") # new cpt object
pen.type(x) # retrieves the pen.type slot from x
}

\keyword{methods}
\keyword{cpt}
\keyword{internal}