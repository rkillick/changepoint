\name{decision}
\alias{decision}
\title{
Decision Function - Only intended for developer use.
}
\description{
Uses the function parameters to decide if a proposed changepoint is a true changepoint or due to random variability.  Test is conducted using the user specified penalty.

This function is called by \code{cpt.mean}, \code{cpt.var} and \code{cpt.meanvar} when \code{method="AMOC"}.  This is not intended for use by regular users of the package.  It is exported for developers to call directly for speed increases or to fit alternative cost functions.

WARNING: No checks on arguments are performed!
}
\usage{
decision(tau,null,alt=NA,penalty="MBIC",n=0,diffparam=1,pen.value=0)
}
\arguments{
  \item{tau}{
	A numeric value or vector specifying the proposed changepoint location(s).
}
  \item{null}{
	The value of the null test statistic.  If tau is a vector, so is null.  If the test statistic is already known (i.e. doesn't have null and alternative components), replace the null argument with the test statistic.
}
  \item{alt}{
	The value of the alternative test statistic (at tau).  If tau is a vector, so is alt.  If the test statistic is already known, then it is used in replacement of the null argument and the alternative should not be specified (default NA to account for this)
}
  \item{penalty}{
	Choice of "None", "SIC", "BIC", "MBIC", AIC", "Hannan-Quinn", "Asymptotic" and "Manual" penalties.  If Manual is specified, the manual penalty is contained in the pen.value parameter. If Asymptotic is specified, the theoretical type I error is contained in the pen.value parameter. The predefined penalties listed DO count the changepoint as a parameter, postfix a 0 e.g."SIC0" to NOT count the changepoint as a parameter.
}
  \item{n}{
	The length of the original data, required to give sensible "no changepoint" output.
}
  \item{diffparam}{
	The difference in the number of parameters in the null and alternative hypotheses, required for the SIC, BIC, AIC, Hanna-Quinn and possibly Manual penalties.
}
  \item{pen.value}{
	The theoretical type I error e.g.0.05 when using the Asymptotic penalty.  The value of the penalty when using the Manual penalty option - this can be a numeric value or text giving the formula to use.  Available variables are, n=length of original data, null=null likelihood, alt=alternative likelihood, tau=proposed changepoint, diffparam=difference in number of alternatve and null parameters.  
}
}
\details{
	This function is used to test whether tau is a true changepoint or not.  This test uses the null-alternative as the test statistic and performs the test where the null hypothesis is no change point and the alternative hypothesis is a single changepoint at tau.  The test is (null-alt)>=penalty, if TRUE then the changepoint is deemed a true changepoint, if FALSE then n (length of data) is returned.

If the test statistic is already known then it replaces the null value and the alternative is not required (default NA).  In this case the test is null>=penalty, if TRUE then the changepoint is deemed a true changepoint, if FALSE then n (length of data) is returned.

This function is exported for developer use only.  It does not perform any checks on inputs and is included for convenience and speed for those who are developing their own cost functions.
}
\value{
	A list is returned with two elements, cpt and pen.
	\item{cpt}{ If tau is a single value then a single value is returned: Either the value of the true changepoint location or n (length of data) if no changepoint is found.
	
	If tau is a vector of length m then a vector of length m is returned:Each element is either the value of the true changepoint location or n (length of data) if no changepoint is found.  The first element is for the first value of tau and the final element is for the final value of tau.
	}
	\item{pen}{The numeric value of the penalty used for the test(s).}
}
\references{
SIC/BIC: Schwarz, G. (1978) Estimating the Dimension of a Model, \emph{The Annals of Statistics} \bold{6(2)}, 461--464

MBIC: Zhang, N. R. and Siegmund, D. O. (2007) A Modified Bayes Information Criterion with Applications to the Analysis of Comparative Genomic Hybridization Data. \emph{Biometrics} \bold{63}, 22-32.

AIC: Akaike, H. (1974) A new look at the statistical model identification, \emph{Automatic Control, IEEE Transactions on} \bold{19(6)}, 716--723

Hannan-Quinn: Hannan, E. J. and B. G. Quinn (1979) The Determination of the Order of an Autoregression, \emph{Journal of the Royal Statistical Society, B} \bold{41}, 190--195
}
\author{
Rebecca Killick
}


\seealso{
\code{\link{cpt.mean}},\code{\link{cpt.var}},\code{\link{cpt.meanvar}}
}
\examples{
# Example of finding a change
out=c(100,765.1905,435.6529) # tau, null, alt
decision(out[1],out[2],out[3],penalty="SIC",n=200,diffparam=1) # returns 100 as a true changepoint

# Example of no change found
out=c(53,-22.47768,-24.39894) # tau, null, alt
decision(out[1],out[2],out[3],penalty="Manual",n=200,diffparam=1,pen.value="2*log(n)")
}

\keyword{methods}
\keyword{univar}
\keyword{models}
\keyword{ts}
