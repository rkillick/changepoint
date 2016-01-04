\name{cpttype}
\alias{cpttype}
\title{
Generic Function - cpttype
}
\description{
Generic function
}
\usage{
cpttype(object)
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
\code{\link{cpttype-methods}}
}
\examples{
x=new("cpt") # new cpt object
cpttype(x) # retrieves the cpttype slot from x
}

\keyword{methods}
\keyword{cpt}
\keyword{internal} 