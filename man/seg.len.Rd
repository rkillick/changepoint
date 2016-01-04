\name{seg.len}
\alias{seg.len}
\title{
Generic Function - seg.len
}
\description{
Generic function
}
\usage{
seg.len(object)
}
\arguments{
  \item{object}{
	Depending on the class of \code{object} depends on the method used (and if one exists)
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
\code{\link{seg.len-methods}}
}
\examples{
x=new("cpt") # new cpt object
seg.len(x) # returns the length of each segment in the data (i.e. no. of obs between changepoints)
}

\keyword{methods}
\keyword{cpt}
