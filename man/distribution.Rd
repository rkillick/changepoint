\name{distribution}
\alias{distribution}
\title{
Generic Function - distribution
}
\description{
Generic function
}
\usage{
distribution(object)
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
\code{\link{distribution-methods}}
}
\examples{
x=new("cpt") # new cpt object
distribution(x) # retrieves the distribution slot from x
}

\keyword{methods}
\keyword{cpt}
\keyword{internal}