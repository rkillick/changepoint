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


segneigh.meanvar.poisson=function(data,Q=5,pen=0){
  if((sum(data<0)>0)){stop('Poisson test statistic requires positive data')}
  if(sum(as.integer(data)==data)!=length(data)){stop('Poisson test statistic requires integer data')}
  n=length(data)
  if(n<4){stop('Data must have atleast 4 observations to fit a changepoint model.')}
  if(Q>((n/2)+1)){stop(paste('Q is larger than the maximum number of segments',(n/2)+1))}
  all.seg=matrix(0,ncol=n,nrow=n)
  for(i in 1:n){
    sumx=0
    for(j in i:n){
      len=j-i+1
      sumx=sumx+data[j]
      if(sumx==0){
        all.seg[i,j]=-Inf
      }
      else{
        all.seg[i,j]=sumx*log(sumx)-sumx*log(len)
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


multiple.meanvar.poisson=function(data,mul.method="PELT",penalty="MBIC",pen.value=0,Q=5,class=TRUE,param.estimates=TRUE,minseglen){
  if((sum(data<0)>0)){stop('Poisson test statistic requires positive data')}
  if(sum(as.integer(data)==data)!=length(data)){stop('Poisson test statistic requires integer data')}
  if(!((mul.method=="PELT")||(mul.method=="BinSeg")||(mul.method=="SegNeigh"))){
    stop("Multiple Method is not recognised")
  }
  costfunc = "meanvar.poisson"
  if(penalty=="MBIC"){
    if(mul.method=="SegNeigh"){
      stop('MBIC penalty not implemented for SegNeigh method, please choose an alternative penalty')
    }
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
