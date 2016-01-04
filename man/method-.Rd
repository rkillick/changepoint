\name{method<-}
\alias{method<-}
\title{
Generic Function - method<-
}
\description{
Generic function
}
\usage{
method(object)<-value
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
\code{\link{method<--methods}}
}
\examples{
x=new("cpt") # new cpt object
method(x)<-"mean" # replaces the existing method slot in x with "mean"
}

\keyword{methods}
\keyword{cpt}
\keyword{internal} 