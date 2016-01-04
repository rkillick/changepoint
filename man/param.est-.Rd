\name{param.est<-}
\alias{param.est<-}
\title{
Generic Function - param.est<-
}
\description{
Generic function
}
\usage{
param.est(object)<-value
}
\arguments{
  \item{object}{
	Depending on the class of \code{object} depends on the method used (and if one exists)
}
  \item{value}{
	Replacement value
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
\code{\link{param.est<--methods}}
}
\examples{
x=new("cpt") # new cpt object
param.est(x)<-list(mean=0) # replaces the current param.est list in x with list(mean=0)
}

\keyword{methods}
\keyword{cpt}
\keyword{internal} 