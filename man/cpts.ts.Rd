\name{cpts.ts}
\alias{cpts.ts}
\title{
Generic Function - cpts.ts
}
\description{
Generic function
}
\usage{
cpts.ts(object)
}
\arguments{
  \item{object}{
	Depending on the class of \code{object} depends on the method used (and if one exists)
}
}
\details{
	Generic function.
}
\value{
	Depends on the class of \code{object}, see individual methods
}
\author{
Rebecca Killick
}


\seealso{
\code{\link{cpts.ts-methods}}
}
\examples{
x=new("cpt") # new cpt object
cpts.ts(x) # retrieves the cpts slot from x but unlike cpts, displays the "date" index rather than
#the position within the vector
}

\keyword{methods}
\keyword{cpt}
\keyword{internal}