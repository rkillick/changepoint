cpt.meanvar.binom <- function(data,penalty="MBIC",pen.value=0,method="PELT",Q=5,class=TRUE,
                  param.estimates=TRUE,minseglen=2,size=NA){
  ##Checks already performed by cpt.meanvar:
  #   data contains no NAs
  #   method=="SegNeigh" & minseglen>2 ==> stop()
  if(is.na(size)){
    stop("For Binomial cost functions the size argument must be specified.")
  }
  if(method=="AMOC"){
    return(single.meanvar.binomial(data, size, penalty, pen.value, class,
                                   param.estimates,minseglen))
  }else if(method=="PELT" || method=="BinSeg" || method=="SegNeigh"){
    return(multiple.meanvar.binomial(data, size, mul.method=method,penalty,
                                     pen.value,Q,class,param.estimates,minseglen))
  }else{
    stop("Invalid Method, must be AMOC, PELT, SegNeigh or BinSeg")
  }
}

single.meanvar.binomial <- function(data, size, penalty="MBIC",pen.value=0,class=TRUE,
                        param.estimates=TRUE,minseglen){
  ##checks on data & size
  if(any(data<0) || any(size<0)) stop("data and size needs to be positive.")
  if(any(data%%1!=0) || any(size%%1!=0)) stop("data and size needs to be integers.")
  if(length(size)==1){
    SIZE <- data*0 + size ## repeat size to the dimension of data & inherit class
  }else if(length(data)==length(size)){
    if(!is.numeric(size)) stop("Only numeric size allowed")
    if(anyNA(size)){stop("Missing value: NA is not allowed in size.")}
    SIZE <- size
  }else{
    stop("Size needs to be either a single number or have the same dimensions as data.")  
  }
  if(any(data > SIZE)) stop("Elements of data cannot be greater than the corresponding element of size.")
  if(is.null(dim(data))){
    #single dataset
    n <- length(data)
  }else{
    n <- nrow(data)
  }
  if(n<4) stop("Data must have at least 4 observations to fit this changepoint model.")
  if(n<(2*minseglen)) stop("Minimum segment length is too large to include a changepoint in this data")
  
  pen.value = penalty_decision(penalty, pen.value, n, diffparam=1, asymcheck="meanvar.binomial",method="AMOC")
  if(is.null(dim(data))){
    #single dataset
    tmp = single.meanvar.binomial.calc(coredata(data),coredata(SIZE),extrainf=TRUE,minseglen)
    if(penalty=="MBIC"){
      tmp[3] = tmp[3] + log(tmp[1]) + log(n-tmp[1]+1)
    }
    ans = decision(tmp[1],tmp[2],tmp[3],penalty,n,diffparam=1,pen.value)
    if(class){
      return(class_input(data, size=SIZE, method="AMOC",
                               penalty=penalty, pen.value=ans$pen,
                               minseglen=minseglen, param.estimates=param.estimates,
                               out = c(0,ans$cpt)))
    }else{
      return(ans$cpt)
    }
  }else{
    #multiple datasets
    tmp = single.meanvar.binomial.calc(data,SIZE,extrainf=TRUE,minseglen)
    if(penalty=="MBIC"){
      tmp[,3] = tmp[,3] + log(tmp[,1]) + log(n-tmp[,1]+1)
    }
    ans = decision(tmp[,1],tmp[,2],tmp[,3],penalty,n,diffparam=1,pen.value)
    if(class){
      rep = ncol(data)
      out = list()
      for(i in 1:rep){
        if(length(size)==1){ size_out<- size }else{ size_out<-size[,i] } 
        # the above looks repetitive from line 25 but 25 is generic and 71 is specific to multivariate
        out[[i]] <- class_input(data[,i],size=size_out,
                      method="AMOC", penalty=penalty, 
                      pen.value=ans$pen, minseglen=minseglen, 
                      param.estimates=param.estimates, out = c(0,ans$cpt[i]))
      }
      return(out)
    }else{
      return(ans$cpt)
    }
  }
}



single.meanvar.binomial.calc <- function(data,size,extrainf=TRUE,minseglen){
  singledim <- function(data,size,extrainf=TRUE,minseglen){
    n = length(data)
    y = c(0,cumsum(data))
    m = c(0,cumsum(size))
    if(y[n+1] == 0 || y[n+1]==m[n+1]){
      null = Inf
    }else{
      null=-2*(y[n+1]*log(y[n+1])-m[n+1]*log(m[n+1]) + (m[n+1]-y[n+1])*log(m[n+1]-y[n+1]))
    }
    taustar = minseglen:(n-minseglen)
    sx1 = y[taustar+1]
    sm1 = m[taustar+1]
    sx1_sm1 = sm1-sx1
    sx2 = y[n+1]-y[taustar+1]
    sm2 = m[n+1]-m[taustar+1]
    sx2_sm2 = sm2-sx2
    tmp = sx1*log(sx1) - sm1*log(sm1) + sx1_sm1*log(sx1_sm1)
    tmp = tmp + sx2*log(sx2) - sm2*log(sm2) + sx2_sm2*log(sx2_sm2)
    ##NOTE: 
    ## probMLE = c(rep(sx1/sm1,taustar[i]),rep(sx2/sm2,n-taustar[i])) 
    ## sum(dbinom(data,size,probMLE,log=TRUE)) == tmp + sum(lchoose(size,data))
    
    tmp = -2*tmp  #convert to deviance scale
    if(anyNA(tmp)) tmp[is.na(tmp)] = Inf
    tau=which(tmp==min(tmp,na.rm=T))[1]
    taulike = tmp[tau]
    tau = tau + minseglen - 1 ## correcting for the fact that we are starting at minseglen
    if(extrainf){
      out <- c(tau,null,taulike)
      names(out) = c("cpt","null","alt")
      return(out)
    }else{
      return(tau)
    }
  }
  
  if(is.null(dim(data))){
    cpt = singledim(data,size,extrainf,minseglen)
  }else{
    rep = nrow(data) 
    n = ncol(data)   
    if(!extrainf){
      cpt=rep(0,rep)
      for(i in 1:rep){
        cpt[i] <- singledim(data[i,], size[i,], extrainf, minseglen)
      }
    }else{
      cpt = matrix(0,ncol=3,nrow=rep)
      for(i in 1:rep){
        cpt[i,] = singledim(data[i,], size[i,], extrainf, minseglen)
      }
      colnames(cpt) <- c("cpt","null","alt")
    }
    return(cpt)
  }
}

multiple.meanvar.binomial <- function(data, size, mul.method="PELT",penalty="MBIC",
                          pen.value=0,Q=5,class=TRUE,param.estimates=TRUE,minseglen){
  ##checks on data & size
  if(length(size)==1){
    SIZE <- data*0 + size ## repeat size to the dimension of data & inherit class
  }else if(length(data)==length(size)){
    if(!is.numeric(size)) stop("Only numeric size allowed")
    if(anyNA(size)){stop("Missing value: NA is not allowed in the size as changepoint methods are only sensible for regularly spaced data.")}
    SIZE <- size
  }else{
    stop("Size needs to be either a single number or have the same dimensions of data.")  
  }
  if(any(data<0) || any(size<0)) stop("Binomal test statistic requires postive data and size.")
  if(any(data%%1!=0) || any(size%%1!=0)) stop("Binomal test statistic requires integer data and size.")
  if(any(data > SIZE)) stop("Binomal test statistic requires data <= size.")
  
  costfunc = "meanvar.binomial"
  if(penalty == "MBIC"){
    if(mul.method == "SegNeigh"){
      stop("MBIC penalty not implemented for SegNeigh method, please choose an alternative penalty")
    }
    costfunc = paste0(costfunc,".mbic")
  }
  
  diffparam = 1
  if(is.null(dim(data))){
    n = length(data)
  }else{
    n = ncol(data)
  }
  
  if(n<(2*minseglen)) stop("Minimum segment length is too large to include a change in this data")
  pen.value = penalty_decision(penalty, pen.value, n, diffparam = 1, asymcheck = costfunc, method = mul.method)  
  if(is.null(dim(data))){
    out = data_input(data=data,method=mul.method, pen.value=pen.value, costfunc=costfunc,
                     minseglen=minseglen, Q=Q, size=size)
    if(class){
      return(class_input(data=data, size=SIZE, method=mul.method,
                               penalty=penalty, pen.value=pen.value,
                               minseglen=minseglen, param.estimates=param.estimates,
                               out = out, Q=Q))
    }else{
      return(out[[2]])
    }
  }else{
    rep = nrow(data)
    out = list()
    for(i in 1:rep){
      out[[i]] = data_input(data=data[i,],method=mul.method, pen.value=pen.value,
                            costfunc=costfunc, minseglen=minseglen, Q=Q, size=size[i,])
    }
    cpts = lapply(out,'[[',2)
    if(class){
      ans = list()
      for(i in 1:rep){
        if(length(size)==1){ size_out<- size }else{ size_out<-size[i,] }
        ans[[i]] = class_input(data=data[i,],size=size_out,
                                     method=mul.method, penalty=penalty, 
                                     pen.value=pen.value, minseglen=minseglen, 
                                     param.estimates=param.estimates, 
                                     out = out[[i]], Q=Q)
      }
      return(ans)
    }else{
      return(cpts)
    }
  }
}

segneigh.meanvar.binomial <- function(data, size=1, Q=5, pen=0){
  ##nb size is the same dimension of data
  ##checks on data and size should have already been performed
  n <- length(data)
  if(n<4){stop('Data must have atleast 4 observations to fit a changepoint model.')}
  if(Q>((n/2)+1)){stop(paste('Q is larger than the maximum number of segments',(n/2)+1))}
  
  all.seg=matrix(0,ncol=n,nrow=n)
  if(length(size)==1){ SIZE <- data*0 + size }else{SIZE <- size}
  for(i in 1:n){
    sumx=0
    sumsize = 0
    for(j in i:n){
      sumx=sumx+data[j]
      sumsize = sumsize + SIZE[j]
      if(sumx==0 || sumx==sumsize){
        all.seg[i,j]=-Inf
      }
      else{
        all.seg[i,j]=sumx*log(sumx) - sumsize*log(sumsize) + (sumsize-sumx)*log(sumsize-sumx)
      }
    }
  }
  like.Q=matrix(0,ncol=n,nrow=Q)
  like.Q[1,]=all.seg[1,]
  cp=matrix(NA,ncol=n,nrow=Q)
  for(q in 2:Q){
    for(j in q:n){
      like=NULL
      if((j-2-q)<0){v=q}
      else{v=(q):(j-2)}
      like=like.Q[q-1,v]+all.seg[v+1,j]
      
      like.Q[q,j]= max(like,na.rm=TRUE)
      cp[q,j]=which(like==max(like,na.rm=TRUE))[1]+(q-1)
    }
    
  }
  cps.Q=matrix(NA,ncol=Q,nrow=Q)
  for(q in 2:Q){
    cps.Q[q,1]=cp[q,n]
    for(i in 1:(q-1)){
      cps.Q[q,(i+1)]=cp[(q-i),cps.Q[q,i]]
    }
  }
  
  op.cps=NULL
  k=0:(Q-1)
  
  for(i in 1:length(pen)){
    criterion=-2*like.Q[,n]+k*pen[i]
    
    op.cps=c(op.cps,which(criterion==min(criterion,na.rm=T))-1)
  }
  if(op.cps==(Q-1)){warning('The number of segments identified is Q, it is advised to increase Q to make sure changepoints have not been missed.')}
  if(op.cps==0){cpts=n}
  else{cpts=c(sort(cps.Q[op.cps+1,][cps.Q[op.cps+1,]>0]),n)}
  
  return(list(cps=t(apply(cps.Q,1,sort,na.last=TRUE)),cpts=cpts,op.cpts=op.cps,pen=pen,like=criterion[op.cps+1],like.Q=like.Q[,n]))
}
