\name{plot-methods}
\docType{methods}
\alias{plot-methods}
\alias{plot,ANY-method}
\alias{plot,cpt-method}
\alias{plot,cpt.range-method}
\alias{plot,cpt.reg-method}
\title{ ~~ Methods for Function plot in Package `graphics' ~~}
\description{
 ~~ Methods for function \code{plot} in Package `graphics' ~~
}
\section{Methods}{
\describe{

\item{\code{signature(x = "ANY")}}{
	Generic plot function, see graphics package description using ?plot
}

\item{\code{signature(x = "cpt")}}{
	Plots the data and identifies the changepoints using vertical lines (change in variance), horizontal lines (change in mean).  Optional arguments to control the lines: \code{cpt.col} equivilent to \code{col} to change the colour of the changepoint line; \code{cpt.width} equivilent to \code{lwd} to change the width of the changepoint line; \code{cpt.style} equivilent to \code{lty} to change the style of the line.
}
\item{\code{signature(x = "cpt.range")}}{
	As for the \code{cpt} objects except for two optional arguments, \code{ncpts} and \code{diagnostic}.  The \code{ncpts} option allows you to specify a plot of the segmentation with \code{ncpts} changepoints in, i.e. the optimal may be specified as 10 changes but you want to plot the segmentation with 5 changes (provided a segmentation with 5 changes is listed in \code{cpts.full(x)}.  The \code{diagnostic} option when set to \code{TRUE} plots the number of changepoints in each segmentation against the change in test statistic when adding that change.  This can aide the decision on the number of changepoints as when a true changepoint is added the cost increases/decreases rapidly, but when a changepoint due to noise is added the change is small.  This is akin to a scree plot in principal component analysis.  The idea is that someone may choose to create a plot using \code{diagnostic=TRUE}, identify the appropriate number of changes and then replot using \code{ncpts} to visualize that segmentation.
}
\item{\code{signature(x = "cpt.reg")}}{
	Plotting is only valid for one regressor.  Plots the regressor against the response and identifies the changepoints using horizontal lines.  Optional arguments to control the lines: \code{cpt.col} equivilent to \code{col} to change the colour of the changepoint line; \code{cpt.width} equivilent to \code{lwd} to change the width of the changepoint line; \code{cpt.style} equivilent to \code{lty} to change the style of the line.
}

}}
\keyword{methods}
\keyword{plot}
\keyword{cpt}
\keyword{internal} 