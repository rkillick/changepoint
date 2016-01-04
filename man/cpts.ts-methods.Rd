\name{cpts.ts-methods}
\docType{methods}
\alias{cpts.ts-methods}
\alias{cpts.ts,cpt-method}
\alias{cpts.ts,cpt.reg-method}
\title{ ~~ Methods for Function cpts.ts  ~~}
\description{
 ~~ Methods for function \code{cpts.ts}  ~~
}
\section{Methods}{
\describe{

\item{\code{signature(object = "cpt")}}{
	Retrieves cpts slot from an object of class cpt, from version 1.0 this no longer prints the length of the dataset.  Contrary to the \code{cpts} function, cpts.ts displays the changepoints using the "date" index according to the start and frequency of the time series data.set(object)
}

\item{\code{signature(object = "cpt.reg")}}{
	Retrieves cpts slot from an object of class cpt.reg, from version 1.0 this no longer prints the length of the dataset.  Contrary to the \code{cpts} function, cpts.ts displays the changepoints using the "date" index according to the start and frequency of the time series data.set(object)
}
}}
\keyword{methods}
\keyword{cpt}
\keyword{internal} 