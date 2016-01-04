\name{pen.value<-}
\alias{pen.value<-}
\title{
Generic Function - pen.value
}
\description{
Generic function
}
\usage{
pen.value(object)<-value
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
\code{\link{pen.value<--methods}}
}
\examples{
x=new("cpt") # new cpt object
pen.value(x)<-5 # replaces the existing pen.value slot in x with 5
}

\keyword{methods}
\keyword{cpt}
\keyword{internal} 