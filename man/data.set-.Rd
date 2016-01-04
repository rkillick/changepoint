\name{data.set<-}
\alias{data.set<-}
\title{
Generic Function - data.set<-
}
\description{
Generic function
}
\usage{
data.set(object)<-value
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
\code{\link{data.set<--methods}}
}
\examples{
x=new("cpt") # new cpt object
data.set(x)<-c(1,2,3,4,5) # replaces the existing data.set slot in x with c(1,2,3,4,5)
}

\keyword{methods}
\keyword{cpt}
\keyword{internal} 