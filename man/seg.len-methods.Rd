\name{seg.len-methods}
\docType{methods}
\alias{seg.len-methods}
\alias{seg.len,cpt-method}
\alias{seg.len,cpt.range-method}
\alias{seg.len,cpt.reg-method}
\title{ ~~ Methods for Function seg.len  ~~}
\description{
 ~~ Methods for function \code{seg.len}  ~~
}
\section{Methods}{
\describe{

\item{\code{signature(object = "cpt")}}{
	Returns the length of segments from an object of class cpt, i.e. the number of observations between changepoints
}
\item{\code{signature(object = "cpt.range",ncpts=NA)}}{
	Returns the length of segments from an object of class cpt.range, i.e. the number of observations between changepoints.  If ncpts is not specified, this is called on the cpts slot of the cpt.range object.  If ncpts is specified, this is called on the cpts.full slot and calculated this for the specific row that gives ncpts.
}

\item{\code{signature(object = "cpt.reg")}}{
	Returns the length of segments from an object of class cpt.reg, i.e. the number of observations between changepoints
}
}}
\keyword{methods}
\keyword{cpt}
\keyword{internal}
