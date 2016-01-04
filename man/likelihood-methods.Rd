\name{likelihood-methods}
\docType{methods}
\alias{likelihood-methods}
\alias{likelihood,cpt-method}
\title{ ~~ Methods for Function likelihood  ~~}
\description{
 ~~ Methods for function \code{likelihood}  ~~
}
\section{Methods}{
\describe{

\item{\code{signature(object = "cpt")}}{
	Returns the likelihood of the data with the fitted changepoints, two values are returned, the raw scaled negative likelihood and the scaled negative likelihood + penalty.  Only valid for cpttype="mean","variance" or "mean and variance".
}
}}
\keyword{methods}
\keyword{cpt}
\keyword{internal} 