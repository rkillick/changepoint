\name{fitted-methods}
\docType{methods}
\alias{fitted-methods}
\alias{fitted,ANY-method}
\alias{fitted,cpt-method}
\alias{fitted,cpt.range-method}
\alias{fitted,cpt.reg-method}
\title{ ~~ Methods for Function fitted in Package `stats' ~~}
\description{
 ~~ Methods for function \code{fitted} in Package `stats' ~~
}
\section{Methods}{
\describe{

\item{\code{signature(x = "ANY")}}{
	Generic fitted function, see stats package description using ?fitted
}

\item{\code{signature(x = "cpt")}}{
	Returns the fitted values for the model fit to x.  Adapts to the specific model type.
}
\item{\code{signature(x = "cpt.range")}}{
	As for the \code{cpt} objects except for one optional arguments, \code{ncpts}.  The \code{ncpts} option allows you to specify the fitted values for \code{ncpts} changepoints in, i.e. the optimal may be specified as 10 changes but you want to get the fitted values for the segmentation with 5 changes (provided a segmentation with 5 changes is listed in \code{cpts.full(x)}.
}
\item{\code{signature(x = "cpt.reg")}}{
	Returns the fitted values for the model fit to x.  Adapts to the specific model type.
}

}}
\keyword{methods}
\keyword{fitted}
\keyword{cpt}
\keyword{internal}
