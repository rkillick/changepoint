\name{class_input}
\alias{class_input}
\title{
Input all required arguments into cpt classes - Only intended for developer use.
}
\description{
This function helps to input all the necessary information into the correct format for \code{cpt} and \code{cpt.range} classes.

This function is called by \code{cpt.mean}, \code{cpt.var} and \code{cpt.meanvar} when \code{class=TRUE}.  This is not intended for use by regular users of the package.  It is exported for developers to call directly for speed and convenience.

WARNING: No checks on arguments are performed!
}
\usage{
class_input(data, cpttype, method, test.stat, penalty, pen.value, minseglen, 
param.estimates, out=list(), Q=NA, shape=NA)
}
\arguments{
  \item{data}{
  Data used in changepoint analysis, see \code{\link{cpt.mean}} for further details.
}
  \item{cpttype}{
  Type of changepoint analysis performed as a text string, e.g. "Mean", "Mean and Variance".
}
  \item{method}{
	Method used as a text string, see \code{\link{cpt.mean}} for further details.
}
  \item{test.stat}{
	The assumed test statistic / distribution of the data as a text string.  , see \code{\link{cpt.mean}}, \code{\link{cpt.meanvar}} or \code{\link{cpt.var}} for further details.
}
  \item{penalty}{
	Penalty used as a text string, see \code{\link{cpt.mean}} for further details.
}
  \item{pen.value}{
	Numerical penalty value used in the analysis (positive).
}
  \item{minseglen}{
  Minimum segment length used in the analysis (positive integer).
}
  \item{param.estimates}{
	Logical.  If TRUE then parameter estimates are calculated. If FALSE no parameter estimates are calculated and the slot is blank in the returned object.
}
  \item{out}{
  List of output from \code{\link{BINSEG}}, \code{\link{PELT}} or other \code{method} used.  Function assumes that \code{method} and format of \code{out} match.
}
  \item{Q}{
	The value of \code{Q} used in the \code{BinSeg} or \code{SegNeigh} methods.
}
  \item{shape}{
	Value of the assumed known shape parameter required when test.stat="Gamma".
}
}
\details{
	This function takes all the input required for the \code{cpt} or \code{cpt.range} classes and enters it into the object.
	
		This function is exported for developer use only.  It does not perform any checks on inputs and is simply a convenience function for converting the output of the worker functions into a nice format for the \code{cpt} and \code{cpt.range} classes.
}
\value{
  An object of class \code{cpt} or \code{cpt.range} as appropriate filled with the given attributes.
}
\author{
Rebecca Killick
}
\seealso{
\code{\link{cpt.var}},\code{\link{cpt.mean}},\code{\link{plot-methods}},\code{\linkS4class{cpt}}
}
\examples{
#This function should only be used by developers, see its use in cpt.mean, cpt.var and cpt.meanvar.
}

\keyword{methods}
\keyword{univar}
\keyword{models}
\keyword{ts}
