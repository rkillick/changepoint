\name{nseg-methods}
\docType{methods}
\alias{nseg-methods}
\alias{nseg,cpt-method}
\alias{nseg,cpt.range-method}
\alias{nseg,cpt.reg-method}
\title{ ~~ Methods for Function nseg  ~~}
\description{
 ~~ Methods for function \code{nseg}  ~~
}
\section{Methods}{
\describe{

\item{\code{signature(object = "cpt")}}{
	Returns the number of segments (i.e. length of the cpts slot) from an object of class cpt
}
\item{\code{signature(object = "cpt.range",ncpts=NA)}}{
	Returns the number of segments from an object of class cpt.range.  If ncpts is not specified, this the length of the cpts slot of the cpt.range object.  If ncpts is specified, this is (trivially) ncpts+1.
}
\item{\code{signature(object = "cpt.reg")}}{
	Returns the number of segments (i.e. length of the cpts slot) from an object of class cpt.reg
}
}}
\keyword{methods}
\keyword{cpt}
\keyword{internal}
