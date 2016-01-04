\name{param.est}
\alias{param.est}
\title{
Generic Function - param.est
}
\description{
Generic function
}
\usage{
param.est(object)
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
\code{\link{param.est-methods}}
}
\examples{
x=new("cpt") # new cpt object
param.est(x) # retrieves the param.est slot from x
}

\keyword{methods}
\keyword{cpt}
\keyword{internal}