\name{cpt.reg}
\alias{cpt.reg}
\title{
  Identifying Changes in Regression
}
\description{
  Calculates the optimal position and (potentially) number of changepoints in regression structure for data using the user specified method.
}
\usage{
  cpt.reg(data, penalty="MBIC", pen.value=0, method="PELT", test.stat="Normal",
    class=TRUE, param.estimates=TRUE, shape=0, minseglen=3, tol=1e-07)
}
\arguments{
  \item{data}{
    A matrix/array or ts object containing the data to fit the models to.
    Col1: the dependent variable, Col2+: regressors. A check is performed
    validate (or include if not) that an intercept regressor is included.
  }
  \item{penalty}{
    Choice of penalty, see \code{\link[changepoint]{penalty_decision}}.
  }
  \item{pen.value}{
    Additional values to be used in evaluating the penalty.
  }
  \item{method}{
    Choice changepoint algorithm. Either "AMOC" (at least one changepoint) or
    "PELT" (pruned exact linear time) method. Default is "PELT".
  }
  \item{test.stat}{
    Test statistic used for regression fit. Currently only "Normal" is supported which assumes a Normal distribution for the errors and fits using Residual Sum of Squares.
  }
  \item{class}{
    Logical. If \code{TRUE} then an oblect of class 'cpt.reg' is returned.
  }
  \item{param.estimates}{
    Logical. If \code{TRUE} and \code{class=TRUE} then parameter estimates are
    returned.
  }
  \item{shape}{
    Additional parameters used in the cost function. If \code{dist="Normal"}, then
    \code{shape} is a single numeric variable that define the cost function to be:

    * \code{shape} < 0 : the residual sum of squares (i.e. quadratic cost).

    * \code{shape} = 0 : -2 * logLik (i.e. -2 * maximum likelihood value). Default.

    * \code{shape} > 0 : -2 * maximum likelihood value with variance=\code{shape}.
  }
  \item{minseglen}{
    Positive integer giving the minimum segment length (no. of observations
    between changes). Default is set at 3, however checks (and adjustements
    where applicable) are performed to ensure this is not smaller than the
    number of regressors.
  }
  \item{tol}{
    Tolerance for the 'qr' decomposition. Default is 1e-7.
    See \code{\link[stats]{lm.fit}}
  }
}
\details{
  This function is used to find change in linear regression structure for
  \code{data}. The changes are found using the method supplied wihich can be
  single changepoint (AMOC) or multiple changepoints (PELT). A changepoint is
  denoted as the last observation of the segment / regime.
}
\value{
  If \code{class=TRUE} then an object of S4 class \code{"cpt.reg"} is returned.
  The slot \code{cpts} contains the changepoints that are returned. For
  \code{class=FALSE} the changepoint positions are returned, along with
  supplementary information about the fit detailed below. (This info is mainly
  used for bug fixing.)

  If \code{data} is a matrix, then a vector/list/cpt.reg is returned depending
  on the of \code{method}. If \code{data} is a 3D array (multiple data sets,
  with total number of data sets = dim1 and each data set of the same size)
  then a list is returned where each element is either a vector/list/cpt.reg
  corresponding to the fit on each data set in the order they appear in the array.

  If \code{method="AMOC"} & \code{dist="Normal"} then a list is returned with:

  cpts: changepoint position.

  pen.value: penalty value.

  If \code{method="PELT"} & \code{dist="Normal"} then a list is returned with:

  cpts: changepoint positions.

  lastchangecpts: index of last changepoint according to optimal sequential fit.

  lastchangelike: cost at last changepont according to optimal sequential fit.

  ncpts: number of changepoints according to optimal squential fit.

}
\references{
PELT Algorithm: Killick R, Fearnhead P, Eckley IA (2012) Optimal detection of changepoints with a linear computational cost, \emph{JASA} \bold{107(500)}, 1590--1598

MBIC: Zhang, N. R. and Siegmund, D. O. (2007) A Modified Bayes Information Criterion with Applications to the Analysis of Comparative Genomic Hybridization Data. \emph{Biometrics} \bold{63}, 22-32.
}
\author{
Rebecca Killick, Simon Taylor
}


\seealso{
  \code{\link[changepoint]{cpt.mean}},
  \code{\link[changepoint]{penalty_decision}},
  \code{\link[changepoint]{plot-methods}},
  \code{\linkS4class{cpt}},
  \code{\link[stats]{lm.fit}}
}
\examples{
## Trend change
set.seed(1)
  x <- 1:200
  beta0 <- rep(c(0,100,50,0),each=50)
  beta1 <- rep(c(1,-1,0,0.25),each=50)
  y <- beta0 + beta1*x + rnorm(200)
  data <- cbind(y,1,x)

  out <- cpt.reg(data, method="PELT", minseglen=5, penalty="MBIC", test.stat="Normal")
  cpts(out)      ##changepoints
  param.est(out) ##parameter estimates (rows: beta estimates per segment)
  plot(out)      ##plot of fit



  ## Seasonal change, period 12
  n=100
indicator=rep(1,n)
trend=1:n
seasonal=cos(2*pi*(1:n -6)/12) # yearly, peak in summer
cpt.s = c(rep(0,floor(n/4)), rep(2, floor(n/4)), rep(1, floor(n/4)),rep(0,n-3*floor(n/4)))
##3 Alternating Cpts
y=0.1*cpt.s*1:n+cos(2*pi*(1:n -6)/12)+rnorm(n)
data=cbind(y,indicator,trend,seasonal)
out=cpt.reg(data, minseglen=12)
plot(out,cpt.width=3)
cpts(out)
param.est(out) ## column order of estimates matches the column order of inputs
}

\keyword{changepoint}
\keyword{regression}
\keyword{models}
\keyword{ts}
