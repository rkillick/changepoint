cpt.mean=function(data,penalty="MBIC",pen.value=0,method="AMOC",Q=5,test.stat="Normal",class=TRUE,param.estimates=TRUE,minseglen=1){
  checkData(data)
  if(method=="SegNeigh" & minseglen>1){stop("minseglen not yet implemented for SegNeigh method, use PELT instead.")}
  if(minseglen<1){minseglen=1;warning('Minimum segment length for a change in mean is 1, automatically changed to be 1.')}
  if(!((test.stat=="Normal")||(test.stat=="CUSUM"))){ stop("Invalid test statistic, must be Normal or CUSUM") }
  
  if(penalty == "CROPS"){
    # browser()
    if(is.numeric(pen.value)){
      if(length(pen.value) == 2){
        if(pen.value[2] < pen.value[1]){
          pen.value = rev(pen.value)
        }
        #run range of penalties
        return(CROPS(data=data, method=method, pen.value=pen.value, test.stat=test.stat, class=class, param.est=param.estimates, minseglen=minseglen, func="mean"))
      }else{
        stop('The length of pen.value must be 2')
      }
    }else{
      stop('For CROPS, pen.value must be supplied as a numeric vector and must be of length 2')
    }
  }
  
  if(test.stat=="Normal"){
    if(method=="AMOC"){
      return(single.mean.norm(data,penalty,pen.value,class,param.estimates,minseglen))
    }
    else if(method=="PELT" || method=="BinSeg"){
      
      return(multiple.mean.norm(data,mul.method=method,penalty,pen.value,Q,class,param.estimates,minseglen)) 
    }
    else if(method=="SegNeigh"){
      warning("SegNeigh is computationally slow, use PELT instead")
      return(multiple.mean.norm(data,mul.method=method,penalty,pen.value,Q,class,param.estimates,minseglen))
    }	
    else{
      stop("Invalid Method, must be AMOC, PELT, SegNeigh or BinSeg")
    }
  }
  else if(test.stat=="CUSUM"){
    warning('Traditional penalty values are not appropriate for the CUSUM test statistic')
    if(method=="AMOC"){
      return(single.mean.cusum(data,penalty,pen.value,class,param.estimates,minseglen))
    }
    else if(method=="SegNeigh" || method=="BinSeg"){
      return(multiple.mean.cusum(data,mul.method=method,penalty,pen.value,Q,class,param.estimates,minseglen))
    }
    else{
      stop("Invalid Method, must be AMOC, SegNeigh or BinSeg")
    }
  }
}

#cpt.reg=function(data,penalty="MBIC",pen.value=0,method="AMOC",Q=5,test.stat="Normal",class=TRUE,param.estimates=TRUE){
#	if(test.stat !="Normal"){ stop("Invalid test statistic, must be Normal") }
#	if(method=="AMOC"){
#		return(single.reg.norm(data,penalty,pen.value,class,param.estimates))
#	}
#	else if(method=="PELT" || method=="BinSeg"){
#		return(multiple.reg.norm(data,mul.method=method,penalty,pen.value,Q,class,param.estimates))
#	}
#	else if(method=="SegNeigh"){
#		warning("SegNeigh is computationally slow, use PELT instead")
#		return(multiple.reg.norm(data,mul.method=method,penalty,pen.value,Q,class,param.estimates))
#	}
#	else{
#		stop("Invalid Method, must be AMOC, PELT, SegNeigh or BinSeg")
#	}
#}

cpt.var=function(data,penalty="MBIC",pen.value=0,know.mean=FALSE, mu=NA,method="AMOC",Q=5,test.stat="Normal",class=TRUE,param.estimates=TRUE,minseglen=2){
  checkData(data)
  if(method=="SegNeigh" & minseglen>2){stop("minseglen not yet implemented for SegNeigh method, use PELT instead.")}
  if(minseglen<2){minseglen=2;warning('Minimum segment length for a change in variance is 2, automatically changed to be 2.')}
  if(penalty == "CROPS"){
    # browser()
    if(is.numeric(pen.value)){
      if(length(pen.value) == 2){
        if(pen.value[2] < pen.value[1]){
          pen.value = rev(pen.value)
        }
        #run range of penalties
        return(CROPS(data=data, method=method,pen.value=pen.value, test.stat=test.stat, class=class, param.est=param.estimates, minseglen=minseglen, func="var"))
      }else{
        stop('The length of pen.value must be 2')
      }
    }else{
      stop('For CROPS, pen.value must be supplied as a numeric vector and must be of length 2')
    }
  }
  
  if(test.stat =="Normal"){
    
    if(method=="AMOC"){
      return(single.var.norm(data,penalty,pen.value,know.mean,mu,class,param.estimates,minseglen))
    }
    else if(method=="PELT" || method=="BinSeg"){
      
      return(multiple.var.norm(data,mul.method=method,penalty,pen.value,Q,know.mean,mu,class,param.estimates,minseglen))
    }
    else if(method=="SegNeigh"){
      warning("SegNeigh is computationally slow, use PELT instead")
      return(multiple.var.norm(data,mul.method=method,penalty,pen.value,Q,know.mean,mu,class,param.estimates,minseglen))
    }
    else{
      stop("Invalid Method, must be AMOC, PELT, SegNeigh or BinSeg")
    }
  }
  else if(test.stat=="CSS"){
    warning('Traditional penalty values are not appropriate for the CSS test statistic')
    if(method=="AMOC"){
      return(single.var.css(data,penalty,pen.value,class,param.estimates,minseglen))
    }
    else if(method=="PELT" || method=="SegNeigh" || method=="BinSeg"){
      return(multiple.var.css(data,mul.method=method,penalty,pen.value,Q,class,param.estimates,minseglen))
    }
    else{
      stop("Invalid Method, must be AMOC, SegNeigh or BinSeg")
    }
  }
  else{
    stop("Invalid test statistic, must be Normal or CSS")
  }
}

cpt.meanvar=function(data,penalty="MBIC",pen.value=0,method="AMOC",Q=5,test.stat="Normal",class=TRUE,param.estimates=TRUE,shape=1,minseglen=2){
  checkData(data)
  if(method=="SegNeigh" & minseglen>2){stop("minseglen not yet implemented for SegNeigh method, use PELT instead.")}
  if(minseglen<2){
    if(!(minseglen==1 & (test.stat=="Poisson"|test.stat=="Exponential"))){
      minseglen=2;warning('Minimum segment length for a change in mean and variance is 2, automatically changed to be 2.')}
    }
  if(penalty == "CROPS"){
    if(is.numeric(pen.value)){
      if(length(pen.value) == 2){
        if(pen.value[2] < pen.value[1]){
          pen.value = rev(pen.value)
        }
        #run range of penalties
        return(CROPS(data=data, method=method,pen.value=pen.value, test.stat=test.stat, class=class, param.est=param.estimates, minseglen=minseglen, shape=shape, func="meanvar"))
      }else{
        stop('The length of pen.value must be 2')
      }
    }else{
      stop('For CROPS, pen.value must be supplied as a numeric vector and must be of length 2')
    }
  }
  if(test.stat=="Normal"){
    
    if(method=="AMOC"){
      return(single.meanvar.norm(data,penalty,pen.value,class,param.estimates,minseglen))
    }
    else if(method=="PELT" || method=="BinSeg"){
      
      return(multiple.meanvar.norm(data,mul.method=method,penalty,pen.value,Q,class,param.estimates,minseglen))
    }
    else if(method=="SegNeigh"){
      warning("SegNeigh is computationally slow, use PELT instead")
      return(multiple.meanvar.norm(data,mul.method=method,penalty,pen.value,Q,class,param.estimates,minseglen))
    }
    else{
      stop("Invalid Method, must be AMOC, PELT, SegNeigh or BinSeg")
    }
  }
  else if(test.stat=="Gamma"){
    if(method=="AMOC"){
      return(single.meanvar.gamma(data,shape,penalty,pen.value,class,param.estimates,minseglen))
    }
    else if(method=="PELT" || method=="BinSeg"){
      return(multiple.meanvar.gamma(data,shape,mul.method=method,penalty,pen.value,Q,class,param.estimates,minseglen))
    }
    else if(method=="SegNeigh"){
      warning("SegNeigh is computationally slow, use PELT instead")
      return(multiple.meanvar.gamma(data,shape,mul.method=method,penalty,pen.value,Q,class,param.estimates,minseglen))
    }
    else{
      stop("Invalid Method, must be AMOC, PELT, SegNeigh or BinSeg")
    }
  }
  else if(test.stat=="Exponential"){
    if(method=="AMOC"){
      return(single.meanvar.exp(data,penalty,pen.value,class,param.estimates,minseglen))
    }
    else if(method=="PELT" || method=="BinSeg"){
      
      return(multiple.meanvar.exp(data,mul.method=method,penalty,pen.value,Q,class,param.estimates,minseglen))
    }
    else if(method=="SegNeigh"){
      warning("SegNeigh is computationally slow, use PELT instead")
      return(multiple.meanvar.exp(data,mul.method=method,penalty,pen.value,Q,class,param.estimates,minseglen))
    }
    else{
      stop("Invalid Method, must be AMOC, PELT, SegNeigh or BinSeg")
    }
  }
  else if(test.stat=="Poisson"){
    if(method=="AMOC"){
      return(single.meanvar.poisson(data,penalty,pen.value,class,param.estimates,minseglen))
    }
    else if(method=="PELT" || method=="BinSeg"){
      
      return(multiple.meanvar.poisson(data,mul.method=method,penalty,pen.value,Q,class,param.estimates,minseglen))
    }
    else if(method=="SegNeigh"){
      warning("SegNeigh is computationally slow, use PELT instead")
      return(multiple.meanvar.poisson(data,mul.method=method,penalty,pen.value,Q,class,param.estimates,minseglen))
    }
    else{
      stop("Invalid Method, must be AMOC, PELT, SegNeigh or BinSeg")
    }
  }
  else{
    stop("Invalid test statistic, must be Normal, Gamma, Exponential or Poisson")
  }
}

checkData = function(data){
  if(!is.numeric(data)){
    stop("Only numeric data allowed")
  }  
  if(anyNA(data)){stop("Missing value: NA is not allowed in the data as changepoint methods are only sensible for regularly spaced data.")}
  
}
