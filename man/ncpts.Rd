\name{ncpts}
\alias{ncpts}
\title{
Generic Function - ncpts
}
\description{
Generic function
}
\usage{
ncpts(object)
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
\code{\link{ncpts-methods}}
}
\examples{
x=new("cpt") # new cpt object
ncpts(x) # returns the number of changepoints (i.e. length of the cpts slot in x minus 1)
}

\keyword{methods}
\keyword{cpt}