\name{distribution<-}
\alias{distribution<-}
\title{
Generic Function - distribution<-
}
\description{
Generic function
}
\usage{
distribution(object)<-value
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
\code{\link{distribution<--methods}}
}
\examples{
x=new("cpt") # new cpt object
distribution(x)<-"normal" # replaces the current distribution slot of x with "normal"
}

\keyword{methods}
\keyword{cpt}
\keyword{internal} 