\name{penalty_decision}
\alias{penalty_decision}
\title{
Penalty Decision Function - Only intended for developer use.
}
\description{
Evaluates the arguments to give a numeric value for the penalty.

This function is called by \code{cpt.mean}, \code{cpt.var} and \code{cpt.meanvar}.  This is not intended for use by regular users of the package.  It is exported for developers to call directly for speed increases or to fit alternative cost functions.

WARNING: No checks on arguments are performed!
}
\usage{
penalty_decision(penalty, pen.value, n, diffparam, asymcheck, method)
}
\arguments{
  \item{penalty}{
	Choice of "None", "SIC", "BIC", "MBIC", AIC", "Hannan-Quinn", "Asymptotic" and "Manual" penalties.  If Manual is specified, the manual penalty is contained in the pen.value parameter. If Asymptotic is specified, the theoretical type I error is contained in the pen.value parameter. The predefined penalties listed DO count the changepoint as a parameter, postfix a 0 e.g."SIC0" to NOT count the changepoint as a parameter.
}
  \item{pen.value}{
	The theoretical type I error e.g.0.05 when using the Asymptotic penalty.  The value of the penalty when using the Manual penalty option - this can be a numeric value or text giving the formula to use.  Available variables are, n=length of original data, null=null likelihood, alt=alternative likelihood, tau=proposed changepoint, diffparam=difference in number of alternatve and null parameters.  
}
  \item{n}{
	The length of the original data, required to give sensible "no changepoint" output.
}
  \item{diffparam}{
	The difference in the number of parameters (degrees of freedom) when a change is added, required for the SIC, BIC, AIC, Hanna-Quinn and possibly Manual penalties.  Do NOT include the changepoint when calculating this number as this is automatically added.
}
  \item{asymcheck}{
  A text string which translates to the asymptotic formula for a specific cost function.  Currently implemented values are: \code{mean.norm}, \code{var.norm}, \code{meanvar.norm}, \code{reg.norm}, \code{var.css}, \code{mean.cusum}, \code{meanvar.gamma}, \code{meanvar.exp}, \code{meanvar.poisson}.
}
  \item{method}{
	Method used as a text string, see \code{\link{cpt.mean}} for further details.
}
}
\details{
This function takes the text string input and converts it to a numerical value for the specific length of data specified by n.

This function is exported for developer use only.  It does not perform any checks on inputs and is included for convenience and speed for those who are developing their own cost functions.
}
\value{
	The numeric value of the penalty.
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
