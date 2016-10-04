\name{BINSEG}
\alias{BINSEG}
\title{
Binary Segmentation - Only intended for developer use.
}
\description{
Implements the Binary Segmentation method for identifying changepoints in a given set of summary statistics for a specified cost function and penalty.

This function is called by \code{cpt.mean}, \code{cpt.var} and \code{cpt.meanvar} when \code{method="BinSeg"}.  This is not intended for use by regular users of the package.  It is exported for developers to call directly for speed increases or to fit alternative cost functions.

WARNING: No checks on arguments are performed!
}
\usage{
BINSEG(sumstat, pen = 0, cost_func = "norm.mean", shape = 1, minseglen = 2,  Q=5)
}
\arguments{
  \item{sumstat}{
	A matrix containing the summary statistics of data within which you wish to find a changepoint.  Currently assumes 3 columns and uses the number of rows as the length of the data +1 (initial value of 0).
}
  \item{pen}{
  Default choice is 0, this should be evaluated elsewhere and a numerical value entered.  This should be positive - this isn't checked but results are meaningless if it isn't.
}
  \item{cost_func}{
  The friendly name of the cost function to be called in C.  If using your own cost function, this must be the name of the C function to use.
}
  \item{shape}{
	Only required for cost_func="Gamma",default is 1.  Must be a positive value, this isn't checked.
}
  \item{minseglen}{
  Positive integer giving the minimum segment length (no. of observations between changes), default is 2.  No checks are performed on the input value so it could be larger than feasible to have changes in the data.
  }
  \item{Q}{
	The maximum number of changepoints to search for (positive integer). No checks are performed and so a number larger than allowed can be input.
}
}
\details{
	This function is used as a wrapper function to implement the Binary Segmentation algorithm in C.  It simply creates the necessary worker vectors, ensures all inputs are the correct type, and passes everything to the C function.
	
	This function is exported for developer use only.  It does not perform any checks on inputs (other than type coersion) and is simply a wrapper function for the C code.
}
\value{
	A list is returned with elements:
	\item{cps}{2xQ Matrix containing the changepoint positions on the first row and the test statistic on the second row in the order identified.}
	\item{cpts}{Ordered list of optimal number of changepoints ending with n.}
	\item{op.cpts}{The optimal number changepoint locations for the penalty supplied.}
	\item{pen}{Penalty used to find the optimal number of changepoints.}
}
\references{
Binary Segmentation: Scott, A. J. and Knott, M. (1974) A Cluster Analysis Method for Grouping Means in the Analysis of Variance, \emph{Biometrics} \bold{30(3)}, 507--512
}
\author{
Rebecca Killick
}


\seealso{
\code{\link{cpt.mean}},\code{\link{cpt.meanvar}},\code{\link{plot-methods}},\code{\linkS4class{cpt}}
}
\examples{
#This function should only be used by developers, see its use in cpt.mean, cpt.var and cpt.meanvar.
}

\keyword{methods}
\keyword{univar}
\keyword{models}
\keyword{ts}
