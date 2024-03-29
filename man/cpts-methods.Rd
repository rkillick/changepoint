\name{cpts-methods}
\docType{methods}
\alias{cpts-methods}
\alias{cpts,cpt-method}
\alias{cpts,cpt.reg-method}
\alias{cpts,cpt.range-method}
\title{ ~~ Methods for Function cpts  ~~}
\description{
 ~~ Methods for function \code{cpts}  ~~
}
\section{Methods}{
\describe{

\item{\code{signature(object = "cpt")}}{
	Retrieves cpts slot from an object of class cpt, from version 1.0 this no longer prints the length of the dataset.
}

\item{\code{signature(object = "cpt.reg")}}{
	Retrieves cpts slot from an object of class cpt.reg, from version 1.0 this no longer prints the length of the dataset.
}

\item{\code{signature(object = "cpt.range",ncpts=NA)}}{
	Retrieves the row in the cpts.full slot from an object of class cpt.range that has length ncpts, from version 1.0 this no longer prints the length of the dataset.
}
}}
\keyword{methods}
\keyword{cpt}
\keyword{internal} 