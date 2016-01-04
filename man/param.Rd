\name{param}
\alias{param}
\title{
Generic Function - param
}
\description{
Generic function that returns parameter estimates.
}
\usage{
param(object,...)
}
\arguments{
  \item{object}{
	Depending on the class of \code{object} depends on the method used to find the parameter estimates (and if one exists)
}
  \item{...}{
	Other variables that may be required depending on the class of \code{object}, see individual methods.
}
}
\details{
	Generic Function that returns parameter estimates
}
\value{
	Depends on the class of \code{object}, see individual methods
}
\author{
Rebecca Killick
}


\seealso{
\code{\link{param-methods}},\code{\link{cpt.mean}},\code{\link{cpt.var}},\code{\link{cpt.meanvar}}
}
\examples{
set.seed(1)
x=c(rnorm(100,0,1),rnorm(100,0,10))
ans=cpt.var(x,penalty="Asymptotic",pen.value=0.01,method="AMOC",param.estimates=FALSE)
# if estimates were not requested in the analysis then they can be created at a later stage if
#required
ans=param(ans) # fills the param.est slot with the parameter estimes.
param.est(ans) # variance is 0.8067621
}

\keyword{methods}
\keyword{cpt}
\keyword{internal}