cpt.reg <- function(data, penalty="MBIC", pen.value=0, method="PELT", test.stat="Normal",
  class=TRUE, param.estimates=TRUE, shape = 0, minseglen=3, tol = 1e-07){
  checkData(data)
  MBIC=0
  ##Check arguments are valid
  if(!is.array(data) || !is.numeric(data))  ##Further checks applied later
    stop("Argument 'data' must be a numerical matrix/array.")
  if(!is.character(penalty) || length(penalty)>1)
    stop("Argument 'penalty' is invalid.")
    #value of 'penalty' & 'pen.value' checked within changepoint::penalty_decision
  if(!is.character(method) || length(method)>1)
    stop("Argument 'method' is invalid.")
  if(method!="AMOC" && method != "PELT") ##RESTRICTION IN USE
    stop("Invalid method, must be AMOC or PELT.")
  if(!is.character(test.stat) || length(test.stat)>1)
    stop("Argument 'test.stat' is invalid.")
  if(test.stat != "Normal"){  ##RESTRICTION IN USE
    warning(paste0("test.stat = ",test.stat," is not supported. Converted to test.stat='Normal'"))
    test.stat <- "Normal"
  }
  if(!is.logical(class) || length(class)>1)
    stop("Argument 'class' is invalid.")
  if(!is.logical(param.estimates) || length(param.estimates)>1)
    stop("Argument 'param.estimates' is invalid.")
  if(!is.numeric(minseglen) || length(minseglen)>1)
    stop("Argument 'minseglen' is invalid.")
  if(minseglen <= 0 || minseglen%%1 != 0) ##Further checks applied later
    stop("Argument 'minseglen' must be positive integer.")
  if(!is.numeric(tol) || length(tol)!=1)
    stop("Argument 'tol' is invalid.")
  if(tol<0) stop("Argument 'tol' must be positive.")
  ##Argument shape is assessed by the command where it is to be used.
  if(penalty=="CROPS"){
    stop("CROPS has not yet been implemented for cpt.reg")
  }

  #Single data set? convert to multiple data set array.
  if(length(dim(data)) == 2) data <- array(data,dim=c(1,dim(data)))

  ans <- vector("list",dim(data)[1])
  for(i in 1:dim(data)[1]){  ##To each data set.
    #Check format for ith data set
    datai <- check_data(data[i,,])

    #Evaluate penalty value
    pen.value <- penalty_decision(penalty=penalty,
      pen.value=pen.value, n=nrow(datai), diffparam=ncol(datai),
      asymcheck="reg.norm", method=method)
    if(penalty=="MBIC"){MBIC=1}

    #Check value of minseglen
    if(minseglen < (ncol(datai)-1)){
      warning(paste("minseglen is too small, set to:",ncol(datai)))
      minsegleni <- ncol(datai)
    }else if(nrow(datai) < (2*minseglen)){
      stop("Minimum segment length is too large to include a change in this data.")
    }else{
      minsegleni <- minseglen
    }

    CPTS <- ChangepointRegression(data=datai, penalty=penalty,
      penalty.value=pen.value, method=method, dist=test.stat, minseglen=minseglen,
      shape = shape, tol=tol, cpts.only=class, MBIC=MBIC)

    if(class){
      #Convert to cpt.reg object
      ansi <- new("cpt.reg")
      data.set(ansi) <- datai
      cpttype(ansi) <- "regression"
      method(ansi) <- method
      distribution(ansi) <- test.stat
      pen.type(ansi) <- penalty
      pen.value(ansi) <- pen.value
      cpts(ansi) <- CPTS
      if(method=="PELT") ncpts.max(ansi) <- Inf
      if(param.estimates) ansi = param(ansi)
      ans[[i]] <- ansi
    }else{
      #store what is returned from calculation
      ans[[i]] <- CPTS
    }
  }

  ##Return result, only first case if there is only a singe data set
  if(dim(data)[1]==1){ return(ans[[1]]) }else{ return(ans) }
}

####################

check_data <- function(data, minseglen=3){
  if(!is.array(data) || !is.numeric(data))
    stop("Argument 'data' must be a numerical matrix.")
  if(length(dim(data))!=2)
    stop("Argument 'data' must be a numerical matrix.")
  if(!is.numeric(minseglen) || length(minseglen)>1)
    stop("Argument 'minseglen' is invalid.")

  n <- nrow(data)
  p <- ncol(data)-1
  if(p==0) stop("Dimension of data is 1, no regressors found.")
  if(n<p) stop("More regressors than observations.")
  ##Check to see if there is only 1 intercpt regressor
  intercept <- apply(data[,2,drop=FALSE],2,function(a){all(a[1]==a)})
  if(sum(intercept)<1){
    warning("Missing intercept regressor. Appending 1's to right of data.")
    data <- cbind(data,1)
    p <- p+1
  }else if(sum(intercept)>1){
    i <- which(intercept)[-1]+1
    warning("Multiple intercepts found. Keeping only first instance.")
    data <- data[,-i]
  }
  return(data)
}


ChangepointRegression <- function(data, penalty="MBIC", penalty.value=0,
  method="AMOC", dist="Normal", minseglen=3, cpts.only=TRUE, shape=0, MBIC=0, tol=1e-07){
  ##Assume all input arguments are entered correctly
  if(!is.logical(cpts.only) && length(cpts.only)>1)
    stop("Argument 'cpts.only' is invalid.")
  if(method=="AMOC" && dist=="Normal"){
    out <- CptReg_AMOC_Normal(data, penalty, penalty.value, minseglen, shape, MBIC, tol)
  }else if(method=="PELT" && dist=="Normal"){
    out <- CptReg_PELT_Normal(data, penalty.value, minseglen, shape,MBIC, tol)
  }else{
    stop("Changepoint in regression method not recognised.")
  }
  if(cpts.only){
    return(sort(out$cpts))
  }else{
    return(out)
  }
}

CptReg_AMOC_Normal <- function(data, penalty="MBIC", penalty.value=0, minseglen=3,
  shape=0, MBIC=0, tol=1e-07){
  n <- as.integer(nrow(data))
  p <- as.integer(ncol(data)-1)
  if(p<1 || n<p) stop("Invalid data dimensions.")
  #tol <- 1e-07 #Rank tolerance (see lm.fit)
  #shape <- -1 #-1=RSS,0=-2logLik,>0=-2logLik with this fixed variance
  if(!is.numeric(shape) || length(shape)!=1)
    stop("Argument 'shape' is invalid.")

  ##Clean-up on exit
  answer=list()
  answer[[5]]=1
  on.exit(.C("Free_CptReg_Normal_AMOC",answer[[6]],PACKAGE="changepoint"))

  answer <- .C("CptReg_Normal_AMOC", data=as.double(data), n=as.integer(n),
    m=as.integer(p+1), pen=as.double(penalty.value), err=0L,
    shape=as.double(shape), minseglen=as.integer(minseglen), tol=as.double(tol),
    tau=0L, nulllike=vector("double",1), taulike=vector("double",1),
    tmplike=vector("double",n), MBIC=as.integer(MBIC),PACKAGE="changepoint")

  #Check if error has occured
  if(answer$err!=0) stop("C code error:",answer$err,call.=F)
  tmp <- c(answer$tau,answer$nulllike,answer$taulike)
  #Following line in cpt.mean (RSS) but not in old cpt.reg (-2Loglik)
  #if(penalty=="MBIC") tmp[3] = tmp[3] + log(tmp[1]) + log(n-tmp[1]+1)
  out <- decision(tau = tmp[1], null = tmp[2], alt = tmp[3],
    penalty = penalty, n=n, diffparam=p, pen.value = penalty.value)
  names(out) <- c("cpts","pen.value")
  return(out)  #return list of cpts & pen.value
}

CptReg_PELT_Normal <- function(data, penalty.value=0, minseglen=3, shape=0,
  MBIC=0, tol=1e-07){
  n <- as.integer(nrow(data))
  p <- as.integer(ncol(data)-1)
  #tol <- 1e-07 #Rank tolerance (see lm.fit)
  #shape <- -1 #-1=RSS,0=-2logLik,>0=-2logLik with this fixed variance
  if(!is.numeric(shape) || length(shape)!=1)
    stop("Argument 'shape' is invalid.")

  #Check if error has occured
  answer=list()
  answer[[6]]=1
  on.exit(.C("Free_CptReg_Normal_PELT",answer[[6]],PACKAGE="changepoint"))

  answer <- .C("CptReg_Normal_PELT", data=as.double(data), n=n,
    m=as.integer(p+1), pen=as.double(penalty.value), cpt=vector("integer",n),
    err=0L, shape=as.double(shape), minseglen=as.integer(minseglen),
    tol=as.double(tol), lastchangelike=vector("double",n+1),
    lastchangecpts=vector("integer",n+1), numchangecpts=vector("integer",n+1),
    MBIC=as.integer(MBIC), PACKAGE="changepoint")

  if(answer$err!=0){
    stop("C code error:",answer$err,call.=F)
  }
  return(list(lastchangecpts=answer$lastchangecpts,
    cpts=sort(answer$cpt[answer$cpt>0]),
    lastchangelike=answer$lastchangelike,
    ncpts=answer$numchangecpts))
}



########################################################################



