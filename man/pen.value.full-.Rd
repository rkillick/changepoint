\name{pen.value.full<-}
\alias{pen.value.full<-}
\title{
Generic Function - pen.value.full<-
}
\description{
Generic function
}
\usage{
pen.value.full(object)<-value
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
\code{\link{pen.value.full<--methods}}
}
\examples{
x=new("cpt.range") # new cpt.range object
pen.value.full(x)<-5 # replaces the existing pen.value.full slot in x with 5
}

\keyword{methods}
\keyword{cpt}
\keyword{internal} 