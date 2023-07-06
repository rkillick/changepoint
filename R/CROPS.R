CROPS <- function(data, penalty="CROPS", pen.value, method="PELT", test.stat="Normal", class=TRUE, param.est=TRUE, minseglen, shape, func, size){
  if(method != "PELT"){stop('CROPS is a valid penalty choice only if method="PELT", please change your method or your penalty.')}
  mu <- mean(data)

  switch(test.stat,
    "Normal" = {stat = "norm"},
    "Exponential" = {stat = "exp"},
    "Gamma" = {stat = "gamma"},
    "Poisson" = {stat = "poisson"},
    "Binomial" = {stat = "binomial"},
    {stop("Only Normal, Exponential, Gamma, Poisson and Binomial are valid test statistics")}
  )
  costfunc = paste0(func, ".", stat)
  
  sumstat = SumStats(test.stat, data, size)
  
  out = range_of_penalties(sumstat, cost=costfunc, min_pen=pen.value[1], max_pen=pen.value[2], minseglen=minseglen)
  
  if(func=="var"){
    cpttype="variance"
  }else if(func=="meanvar"){
    cpttype="mean and variance"
  }else{
    cpttype="mean"
  }
  
  if(class==TRUE){
      ans = class_input(data=data,cpttype=cpttype, method="PELT", test.stat=test.stat, penalty=penalty, pen.value=pen.value, minseglen=minseglen, param.estimates=param.est, out=out,shape=shape,size=size)
      if(func=="var"){
        param.est(ans)=c(param.est(ans),mean=mu)
      }
    return(ans)
  }else{return(out)}
}
