\name{residuals-methods}
\docType{methods}
\alias{residuals-methods}
\alias{residuals,ANY-method}
\alias{residuals,cpt-method}
\alias{residuals,cpt.range-method}
\alias{residuals,cpt.reg-method}
\title{ ~~ Methods for Function residuals in Package `stats' ~~}
\description{
 ~~ Methods for function \code{residuals} in Package `stats' ~~
}
\section{Methods}{
  \describe{

    \item{\code{signature(x = "ANY")}}{
      Generic residuals function, see stats package description using ?residuals
    }

    \item{\code{signature(x = "cpt")}}{
      Returns the residuals values from the model fit to x.  Adapts to the specific model type.
    }
    \item{\code{signature(x = "cpt.range")}}{
      As for the \code{cpt} objects except for one optional arguments, \code{ncpts}.  The \code{ncpts} option allows you to specify the residuals values for \code{ncpts} changepoints in, i.e. the optimal may be specified as 10 changes but you want to get the residuals values for the segmentation with 5 changes (provided a segmentation with 5 changes is listed in \code{cpts.full(x)}.
    }
    \item{\code{signature(x = "cpt.reg")}}{
      Returns the residuals values from the model fit to x.  Adapts to the specific model type.
    }

  }}
\keyword{methods}
\keyword{residuals}
\keyword{cpt}
\keyword{internal}
