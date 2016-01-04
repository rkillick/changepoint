\name{cpt-class}
\Rdversion{1.1}
\docType{class}
\alias{cpt-class}
\alias{cpt-method,cpts}
\alias{cpt-method,cpttype}
\alias{cpt-method,data.set}
\alias{cpt-method,data.set.ts}
\alias{cpt-method,test.stat}
\alias{cpt-method,ncpts.max}
\alias{cpt-method,method}
\alias{cpt-method,minseglen}
\alias{cpt-method,param.est}
\alias{cpt-method,pen.type}
\alias{cpt-method,pen.value}
\alias{cpt-method,cpts<-}
\alias{cpt-method,cpttype<-}
\alias{cpt-method,data.set<-}
\alias{cpt-method,test.stat<-}
\alias{cpt-method,ncpts.max<-}
\alias{cpt-method,method<-}
\alias{cpt-method,minseglen<-}
\alias{cpt-method,param.est<-}
\alias{cpt-method,pen.type<-}
\alias{cpt-method,pen.value<-}
\alias{cpt-method,print}
\alias{cpt-method,summary}
\alias{cpt-method,plot}
\alias{cpt-method,param}
\alias{cpt-method,logLik}

\title{Class "cpt"}
\description{
A class for changepoint objects.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("cpt", ...)}.
  \describe{
    \item{\code{new("cpt", ...)}:}{creates a new object with class cpt }
  }
}
\section{Slots}{
  \describe{
    \item{\code{data.set}:}{Object of class \code{"ts"}, a coerced time series of the original data. }
    \item{\code{cpttype}:}{Object of class \code{"character"}, the type of changepoint that was identified. }
    \item{\code{method}:}{Object of class \code{"character"}, the method that was used to search for changepoints. }
    \item{\code{test.stat}:}{Object of class \code{"character"}, the test statistic for the analysis of the data. }
    \item{\code{pen.type}:}{Object of class \code{"character"}, the penalty type specified in the analysis.}
    \item{\code{pen.value}:}{Object of class \code{"numeric"}, the value of the penalty used in the analysis.}
    \item{\code{minseglen}:}{Object of class \code{"numeric"}, the minimum segment length (no. of observations between changepoints) used in the analysis.}
    \item{\code{cpts}:}{Object of class \code{"numeric"}, vector of changepoints identified. }
    \item{\code{ncpts.max}:}{Object of class \code{"numeric"}, maximum number of changepoint that can be identified. }
    \item{\code{param.est}:}{Object of class \code{"list"}, list where each element is a vector of parameter estimates, if requested. }
    \item{\code{date}:}{Object of class \code{"character"}, date and time the changepoint analysis was run. }
        \item{\code{version}:}{Object of class \code{"character"}, version number of the package used when the analysis was run.}

  }
}
\section{Methods}{
  \describe{
    \item{cpts}{\code{signature(object = "cpt")}: retrieves cpts slot }
    \item{cpttype}{\code{signature(object = "cpt")}: retrieves cpttype slot }
    \item{data.set}{\code{signature(object = "cpt")}: retrieves vector version of data.set slot }
    \item{data.set.ts}{\code{signature(object = "cpt")}: retrieves time series version of data.set slot }
    \item{test.stat}{\code{signature(object = "cpt")}: retrieves test.stat slot }
    \item{ncpts.max}{\code{signature(object = "cpt")}: retrieves ncpts.max slot }
    \item{method}{\code{signature(object = "cpt")}: retrieves method slot }
    \item{minseglen}{\code{signature(object = "cpt")}: retrieves minseglen slot }
    \item{param.est}{\code{signature(object = "cpt")}: retrieves param.est slot }
    \item{pen.type}{\code{signature(object = "cpt")}: retrieves pen.type slot }
    \item{pen.value}{\code{signature(object = "cpt")}: retrieves pen.value slot }
    \item{cpts<-}{\code{signature(object = "cpt")}: replaces cpts slot }
    \item{cpttype<-}{\code{signature(object = "cpt")}: replaces cpttype slot }
    \item{data.set<-}{\code{signature(object = "cpt")}: replaces data.set slot }
    \item{test.stat<-}{\code{signature(object = "cpt")}: replaces test.stat slot }
    \item{ncpts.max<-}{\code{signature(object = "cpt")}: replaces ncpts.max slot }
    \item{method<-}{\code{signature(object = "cpt")}: replaces method slot }
    \item{minseglen<-}{\code{signature(object = "cpt")}: replaces minseglen slot }
    \item{param.est<-}{\code{signature(object = "cpt")}: replaces param.est slot }
    \item{pen.type<-}{\code{signature(object = "cpt")}: replaces pen.type slot }
    \item{pen.value<-}{\code{signature(object = "cpt")}: replaces pen.value slot }
    \item{print}{\code{signature(object = "cpt")}: prints details of the cpt object including summary}
    \item{summary}{\code{signature(object = "cpt")}: prints a summary of the cpt object }
    \item{plot}{\code{signature(object = "cpt")}: plots the cpt object with changepoints highlighted}
    \item{param}{\code{signature(object = "cpt")}: calculates the parameter estimates for the cpt object}
    \item{logLik}{\code{signature(object = "cpt")}: returns the overall log-likelihood of the cpt object}
  }
}

\author{
Rebecca Killick
}

\seealso{
\code{\link{data.set-methods}},\code{\link{cpts-methods}},\code{\linkS4class{cpt.reg}},\code{\link{cpt.mean}},\code{\link{cpt.var}},\code{\link{cpt.meanvar}}
}
\examples{
showClass("cpt") # shows the structure of the cpt class

x=new("cpt") # creates a new object with the cpt class defaults
cpts(x) # retrieves the cpts slot from x
cpts(x)<-c(10,50,100) # replaces the cpts slot from x with c(10,50,100)

# Example of a change in variance at 100 in simulated normal data
set.seed(1)
x=c(rnorm(100,0,1),rnorm(100,0,10))
ans=cpt.var(x) 
print(ans) # prints details of the analysis including a summary
summary(ans)
plot(ans) # plots the data with change (vertical line) at 100
logLik(ans) # raw likelihood of the data with changepoints, second value is likelihood + penalty
}
\keyword{classes}
\keyword{cpt}
\keyword{internal} 