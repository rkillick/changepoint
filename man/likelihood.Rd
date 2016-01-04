\name{likelihood}
\alias{likelihood}
\title{
Generic Function - likelihood
}
\description{
Generic function to calculate the likelihood
}
\usage{
likelihood(object)
}
\arguments{
  \item{object}{
	Depending on the class of \code{object} depends on the method used to calculate the likelihood (and if one exists)
}
}
\details{
	Generic Function to calculate the likelihood.
}
\value{
	Depends on the class of \code{object}, see individual methods
}
\author{
Rebecca Killick
}


\seealso{
\code{\link{likelihood-methods}}, \code{\link{cpt.mean}},\code{\link{cpt.var}},\code{\link{cpt.meanvar}}
}
\examples{
set.seed(1)
x=c(rnorm(50,0,1),rnorm(50,0,10),rnorm(50,0,5),rnorm(50,0,1))
out=cpt.var(x,penalty="Manual",pen.value="2*log(n)",method="BinSeg",Q=5)
likelihood(out) # returns the raw scaled negative likelihood (925.8085) and the scaled negative
#likelihood + penalty (957.5984)
}

\keyword{methods}
\keyword{cpt}
\keyword{internal}