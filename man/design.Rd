\name{design}
\alias{design}
\title{
Generates Design matrix
}
\description{
This function generates a user an (n-1)x(p+2) design matrix for their data and own choice of AR(p) model. This can be used directly with cpt.reg() but is unnecessary for cpt.ar() as it will be generated internally. 
}
\usage{
design(data,p=1)
}
\arguments{
  \item{data}{
	A vector, ts object or matrix containing the data within which you wish to find a changepoint.  If data is a matrix, each row is considered a separate dataset.
}
  \item{p}{
	User specifies the order of the AR(p) model they specifically wish to plug in to \code{cpt.reg}. 
}
}
\author{
Andrew Connell
}


\seealso{
\code{\link{cpt.reg}}
}
\examples{
data <- c(rnorm(100,1,1),rnorm(100,50,1))
dataAR1 <- design(data,1)
dataAR2 <- design(data,2)
}

\keyword{methods}
\keyword{univar}
\keyword{models}
\keyword{ts}
