\name{data.set}
\alias{data.set}
\title{
Generic Function - data.set
}
\description{
Generic function
}
\usage{
data.set(object)
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
\code{\link{data.set-methods}}
}
\examples{
x=new("cpt") # new cpt object
data.set(x) # retrieves the data.set slot from x
}

\keyword{methods}
\keyword{cpt}
\keyword{internal}