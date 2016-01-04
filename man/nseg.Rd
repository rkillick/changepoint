\name{nseg}
\alias{nseg}
\title{
Generic Function - nseg
}
\description{
Generic function
}
\usage{
nseg(object)
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
\code{\link{nseg-methods}}
}
\examples{
x=new("cpt") # new cpt object
nseg(x) # returns the number of segments (i.e. length of the cpts slot)
}

\keyword{methods}
\keyword{cpt}
