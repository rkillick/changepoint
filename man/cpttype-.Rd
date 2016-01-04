\name{cpttype<-}
\alias{cpttype<-}
\title{
Generic Function - cpttype
}
\description{
Generic function
}
\usage{
cpttype(object)<-value
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
\code{\link{cpttype<--methods}}
}
\examples{
x=new("cpt") # new cpt object
cpttype(x)<-"mean" # replaces the existing cpttype in object x with "mean"
}

\keyword{methods}
\keyword{cpt}
\keyword{internal}