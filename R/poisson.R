single.meanvar.poisson.calc <-
  function(data,extrainf=TRUE,minseglen){
    singledim=function(data,extrainf=TRUE,minseglen){
      n=length(data)
      y=c(0,cumsum(data))
      if(y[n+1]==0){
        null=Inf
      }
      else{
        null=2*y[n+1]*log(n) - 2*y[n+1]*log(y[n+1])
      }
      taustar=minseglen:(n-minseglen)
      tmp=2*log(taustar)*y[taustar+1] -2*y[taustar+1]*log(y[taustar+1]) + 2*log(n-taustar)*(y[n+1]-y[taustar+1])-2*(y[n+1]-y[taustar+1])*log((y[n+1]-y[taustar+1]))
      if(sum(is.na(tmp))!=0){
        tmp[which(is.na(tmp))]=Inf
      }
      tau=which(tmp==min(tmp,na.rm=T))[1]
      taulike=tmp[tau]
      tau=tau+minseglen-1 # correcting for the fact that we are starting at minseglen
      if(extrainf==TRUE){
        out=c(tau,null,taulike)
        names(out)=c('cpt','null','alt')
        return(out)
      }
      else{
        return(tau)
      }
    }
    
    if(is.null(dim(data))==TRUE){
      # single data set
      cpt=singledim(data,extrainf,minseglen)
      return(cpt)
    }
    else{
      rep=nrow(data)
      n=ncol(data)
      cpt=NULL
      if(extrainf==FALSE){
        for(i in 1:rep){
          cpt[i]=singledim(data[i,],extrainf,minseglen)
        }
      }
      else{
        cpt=matrix(0,ncol=3,nrow=rep)
        for(i in 1:rep){
          cpt[i,]=singledim(data[i,],extrainf,minseglen)
        }
        colnames(cpt)=c('cpt','null','alt')
      }
      return(cpt)
    }
  }


single.meanvar.poisson<-function(data,penalty="MBIC",pen.value=0,class=TRUE,param.estimates=TRUE,minseglen){
  if((sum(data<0)>0)){stop('Poisson test statistic requires positive data')}
  if(sum(as.integer(data)==data)!=length(data)){stop('Poisson test statistic requires integer data')}
  if(is.null(dim(data))==TRUE){
    # single dataset
    n=length(data)
  }
  else{
    n=ncol(data)
  }
  if(n<4){stop('Data must have atleast 4 observations to fit a changepoint model.')}
  if(n<(2*minseglen)){stop('Minimum segment legnth is too large to include a change in this data')}
  
  pen.value = penalty_decision(penalty, pen.value, n, diffparam=1, asymcheck="meanvar.poisson", method="AMOC")   
  if(is.null(dim(data))==TRUE){
    tmp=single.meanvar.poisson.calc(coredata(data),extrainf=TRUE,minseglen)
    if(penalty=="MBIC"){
      tmp[3]=tmp[3]+log(tmp[1])+log(n-tmp[1]+1)
    }
    ans=decision(tmp[1],tmp[2],tmp[3],penalty,n,diffparam=1,pen.value)
    if(class==TRUE){
      return(class_input(data, cpttype="mean and variance", method="AMOC", test.stat="Poisson", penalty=penalty, pen.value=ans$pen, minseglen=minseglen, param.estimates=param.estimates, out=c(0,ans$cpt)))
    }
    else{ return(ans$cpt)}
  }
  else{ 
    tmp=single.meanvar.poisson.calc(data,extrainf=TRUE,minseglen)
    if(penalty=="MBIC"){
      tmp[,3]=tmp[,3]+log(tmp[,1])+log(n-tmp[,1]+1)
    }
    ans=decision(tmp[,1],tmp[,2],tmp[,3],penalty,n,diffparam=1,pen.value)
    if(class==TRUE){
      rep=nrow(data)
      out=list()
      for(i in 1:rep){
        out[[i]]=class_input(data[i,], cpttype="mean and variance", method="AMOC", test.stat="Poisson", penalty=penalty, pen.value=ans$pen, minseglen=minseglen, param.estimates=param.estimates, out=c(0,ans$cpt[i]))
      }
      return(out)
    }
    else{ return(ans$cpt)}
  }
}


multiple.meanvar.poisson=function(data,mul.method="PELT",penalty="MBIC",pen.value=0,Q=5,class=TRUE,param.estimates=TRUE,minseglen){
  if((sum(data<0)>0)){stop('Poisson test statistic requires positive data')}
  if(sum(as.integer(data)==data)!=length(data)){stop('Poisson test statistic requires integer data')}
  if(!((mul.method=="PELT")||(mul.method=="BinSeg"))){
    stop("Multiple Method is not recognised, must be PELT or BinSeg.")
  }
  costfunc = "meanvar.poisson"
  if(penalty=="MBIC"){
    costfunc = "meanvar.poisson.mbic"
  }
  
  diffparam=1
  if(is.null(dim(data))==TRUE){
    # single dataset
    n=length(data)
  }
  else{
    n=ncol(data)
  }
  if(n<(2*minseglen)){stop('Minimum segment legnth is too large to include a change in this data')}
  
  pen.value = penalty_decision(penalty, pen.value, n, diffparam=1, asymcheck=costfunc, method=mul.method)
  if(is.null(dim(data))==TRUE){
    # single dataset
    out = data_input(data=data,method=mul.method,pen.value=pen.value,costfunc=costfunc,minseglen=minseglen,Q=Q)
    
    if(class==TRUE){
      return(class_input(data, cpttype="mean and variance", method=mul.method, test.stat="Poisson", penalty=penalty, pen.value=pen.value, minseglen=minseglen, param.estimates=param.estimates, out=out, Q=Q))
    }
    else{ return(out[[2]])}
  }
  else{
    rep=nrow(data)
    out=list()
    for(i in 1:rep){
      out[[i]]=data_input(data[i,],method=mul.method,pen.value=pen.value,costfunc=costfunc,minseglen=minseglen,Q=Q)  
    }
  
    cpts=lapply(out, '[[', 2)
  
    if(class==TRUE){
      ans=list()
      for(i in 1:rep){
        ans[[i]]=class_input(data[i,], cpttype="mean and variance", method=mul.method, test.stat="Poisson", penalty=penalty, pen.value=pen.value, minseglen=minseglen, param.estimates=param.estimates, out=out[[i]], Q=Q)
      }
      return(ans)
    }
    else{return(cpts)}
  }
}
