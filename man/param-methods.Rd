\name{param-methods}
\docType{methods}
\alias{param-methods}
\alias{param,cpt-method}
\alias{param,cpt.range-method}
\alias{param,cpt.reg-method}
\title{ ~~ Methods for Function param  ~~}
\description{
 ~~ Methods for function \code{param}  ~~
}
\section{Methods}{
\describe{

\item{\code{signature(object = "cpt")}}{
	Replaces the \code{param.value} slot in object with the parameter estimates that are appropriate for the changepoint type (\code{cpttype} slot).  If the Gamma test statistic is used then the shape parameter is required as a variable.
}
\item{\code{signature(object = "cpt.range")}}{
	Returns a new (blank) cpt.range object where the \code{param.value} slot contains the parameter estimates for a segmentation with the \code{ncpts} specified number of changepoints.  This is also tailored to the changepoint type (\code{cpttype} slot) of the original object.  If the Gamma test statistic is used then the shape parameter is required as a variable.  An example of use is given in the \link{cpt.mean} help file.
}
\item{\code{signature(object = "cpt.reg")}}{
	Replaces the \code{param.value} slot in object with the parameter estimates that are appropriate for the changepoint test statistic (\code{test.stat} slot).
}
}}
\keyword{methods}
\keyword{cpt}
\keyword{internal} 