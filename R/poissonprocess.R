single.meanvar.pp.calc <-
  function(data,extrainf=TRUE,minseglen){
    singledim=function(data,extrainf=TRUE,minseglen){
      start=data[1]
      data=data-start # make it all starting from zero
      nevents=length(data)-2
      end=length(data)
      extendend=2*end-2 # -2 because we don't repeat the first and last value as these are no observations
      ntime=data[length(data)]

      null=2*nevents*(1-log(nevents)+log(ntime)) # 2N(1-logN+logT), N=#events, T=length of time observed

      taustar=(minseglen+1):(extendend-minseglen) # minseglen+1 to account for no event at the first value
      tmp=2*floor((taustar-1)/2)*(1-log(floor((taustar-1)/2)) + log(data[floor(taustar/2)+1]))+ # +1 here so that 1 maps to 1 and not 0
        2*(nevents-floor((taustar-1)/2))*(1-log(nevents-floor((taustar-1)/2)) + log(ntime-data[floor(taustar/2)+1]))
      if(sum(is.na(tmp))!=0){
        tmp[which(is.na(tmp))]=Inf
      }
      tau=which(tmp==min(tmp,na.rm=T))[1]
      taulike=tmp[tau]

      if(extrainf==TRUE){
        # correcting for centering to zero
        out=c(floor(taustar[tau]/2 +1)+start,null,taulike,taustar[tau]%%2)# gives tau index on original scale
        names(out)=c('cpt','null','alt','include event at cpt')
        return(out)
      }
      else{
        return(floor(taustar[tau]/2 +1)+start)
      }
    }

    if(is.null(dim(data))==TRUE || length(dim(data)) == 1){
      # single data set
      cpt=singledim(data,extrainf,minseglen)
      return(cpt)
    }
    else{
      rep=nrow(data)
      cpt=NULL
      if(extrainf==FALSE){
        for(i in 1:rep){
          cpt[i]=singledim(data[i,],extrainf,minseglen)
        }
      }
      else{
        cpt=matrix(0,ncol=4,nrow=rep)
        for(i in 1:rep){
          cpt[i,]=singledim(data[i,],extrainf,minseglen)
        }
        colnames(cpt)=c('cpt','null','alt','include event at cpt')
      }
      return(cpt)
    }
  }


single.meanvar.pp<-function(data,penalty="MBIC",pen.value=0,class=TRUE,param.estimates=TRUE,minseglen){
  if(is.null(dim(data))==TRUE || length(dim(data)) == 1){
    # single dataset
    nevents=length(data)-2 # minus 2 because start and end are boundaries of the space and not event times
  }
  else{
    nevents=ncol(data)-2
  }
  if(nevents<2){stop('Data must have atleast 2 events (plus the start and end observation times) to fit a changepoint model.')}
  if(nevents<(2*minseglen-1)){stop('Minimum segment legnth is too large to include a change in this data')} # minus 1 here as the initial point is not repeated

  pen.value = penalty_decision(penalty, pen.value, nevents, diffparam=1, asymcheck="meanvar.pp", method="AMOC")
  if(is.null(dim(data))==TRUE || length(dim(data)) == 1){
    tmp=single.meanvar.poisson.calc(coredata(data),extrainf=TRUE,minseglen)
    if(penalty=="MBIC"){
      tmp[3]=tmp[3]+log(tmp[1])+log(nevents-tmp[1]+1)
    }
    ans=decision(tmp[1],tmp[2],tmp[3],penalty,nevents,diffparam=1,pen.value)
    ans$includecpt=tmp[4]
    if(class==TRUE){
      # RK: need to change class_input for PP and include/not include cpt
      return(class_input(data, cpttype="mean and variance", method="AMOC", test.stat="Poisson Process", penalty=penalty, pen.value=ans$pen, minseglen=minseglen, param.estimates=param.estimates, out=c(coredata(data)[1],ans$cpt)))
    }
    else{ return(ans$cpt)}
  }
  else{
    tmp=single.meanvar.poisson.calc(data,extrainf=TRUE,minseglen)
    if(penalty=="MBIC"){
      tmp[,3]=tmp[,3]+log(tmp[,1])+log(n-2-tmp[,1]+1) # -2 for start and end
      # this may not be correct if each dimension has a different n (but matrix input),
      # need to add a caveat to the documentation to cover this case and suggest lapply instead
    }
    ans=decision(tmp[,1],tmp[,2],tmp[,3],penalty,n-2,diffparam=1,pen.value) # -2 for start and end
    if(class==TRUE){
      rep=nrow(data)
      out=list()
      for(i in 1:rep){
        # RK: need to change class_input for PP and include/not include cpt
        out[[i]]=class_input(data[i,], cpttype="mean and variance", method="AMOC", test.stat="Poisson", penalty=penalty, pen.value=ans$pen, minseglen=minseglen, param.estimates=param.estimates, out=c(0,ans$cpt[i]))
      }
      return(out)
    }
    else{ return(ans$cpt)}
  }
}


segneigh.meanvar.poisson=function(data,Q=5,pen=0){
  n=length(data)
  if(n<4){stop('Data must have atleast 2 events (plus the start and end observation times) to fit a changepoint model.')}
  if(Q>(n-2)){stop(paste('Q is larger than the maximum number of segments',n-2))}
  all.seg=matrix(0,ncol=n,nrow=n)
  for(i in 1:n){
    sumx=0
    for(j in i:n){
      len=j-i+1
      all.seg[i,j]=2*(j-i)*(1-log(j-i) + log(data[j]-data[i]))
      # 2* #events (1-log(#events)+log(time))
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

  k=0:(Q-1)

  criterion=-2*like.Q[,n]+k*pen
  op.cps=which(criterion==min(criterion,na.rm=T))[1]-1

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
  if(is.null(dim(data))==TRUE || length(dim(data)) == 1){
    # single dataset
    n=length(data)
  }
  else{
    n=ncol(data)
  }
  if(n<(2*minseglen)){stop('Minimum segment legnth is too large to include a change in this data')}

  pen.value = penalty_decision(penalty, pen.value, n, diffparam=1, asymcheck=costfunc, method=mul.method)
  if(is.null(dim(data))==TRUE || length(dim(data)) == 1){
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
