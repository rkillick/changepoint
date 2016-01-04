\name{cpts}
\alias{cpts}
\title{
Generic Function - cpts
}
\description{
Generic function
}
\usage{
cpts(object)
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
\code{\link{cpts-methods}}
}
\examples{
x=new("cpt") # new cpt object
cpts(x) # retrieves the cpts slot from x
}

\keyword{methods}
\keyword{cpt}
\keyword{internal}