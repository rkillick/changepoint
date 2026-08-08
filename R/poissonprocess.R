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
        out=c(floor(taustar[tau]/2 +1),null,taulike,taustar[tau]%%2)
        names(out)=c('cpt','null','alt','include event at cpt')
        return(out)
      } # cpt is data index in the original data, NOT the event time, cpts.ts returns the event time if needed
      else{
        return(floor(taustar[tau]/2 +1))
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
    tmp=single.meanvar.pp.calc(coredata(data),extrainf=TRUE,minseglen)
    if(penalty=="MBIC"){
      tmp[3]=tmp[3]+log(tmp[1])+log(nevents-tmp[1]+1)
    }
    ans=decision(tmp[1],tmp[2],tmp[3],penalty,nevents+2,diffparam=1,pen.value)
    ans$includecpt=tmp[4]
    if(class==TRUE){
      return(class_input(data, cpttype="mean and variance", method="AMOC", test.stat="Poisson Process", penalty=penalty, pen.value=ans$pen, minseglen=minseglen, param.estimates=param.estimates, out=c(coredata(data)[1],ans$cpt),shape=ans$includecpt))
    }
    else{ return(ans$cpt)}
  }
  else{
    tmp=single.meanvar.pp.calc(data,extrainf=TRUE,minseglen)
    if(penalty=="MBIC"){
      tmp[,3]=tmp[,3]+log(tmp[,1])+log(nevents-tmp[,1]+1)
      # this may not be correct if each dimension has a different n (but matrix input),
      # need to add a caveat to the documentation to cover this case and suggest lapply instead
    }
    ans=decision(tmp[,1],tmp[,2],tmp[,3],penalty,nevents+2,diffparam=1,pen.value)
    ans$includecpt=tmp[,4]
    if(class==TRUE){
      rep=nrow(data)
      out=list()
      for(i in 1:rep){
        out[[i]]=class_input(data[i,], cpttype="mean and variance", method="AMOC", test.stat="Poisson Process", penalty=penalty, pen.value=ans$pen, minseglen=minseglen, param.estimates=param.estimates, out=c(coredata(data[i,1]),ans$cpt[i]),shape=ans$includecpt)
      }
      return(out)
    }
    else{ return(ans$cpt)}
  }
}


segneigh.meanvar.pp=function(data,Q=5,pen=0){
  nevents=length(data)-2
  end=length(data)
  extendend=2*end-2 # -2 because we don't repeat the first and last value as these are no observations
  ntime=data[length(data)]

  if(nevents<2){stop('Data must have atleast 2 events (plus the start and end observation times) to fit a changepoint model.')}
  if(Q>nevents){stop(paste('Q is larger than the maximum number of segments',nevents))}
  all.seg=matrix(0,ncol=extendend,nrow=extendend)
  for(i in 1:(extendend-minseglen)){
    sumevent=0
    for(j in (i+minseglen):extendend){
      startind=floor(i/2)+1  # +1 here so that 1 maps to 1 and not 0
      endind=floor(j/2)+1  # +1 here so that 1 maps to 1 and not 0
      neventsseg=floor((j-1)/2)-floor((i-1)/2)
      all.seg[i,j]=2*neventsseg*(1-log(neventsseg) + log(data[endind]-data[startind]))
      # 2* #events (1-log(#events)+log(time))
    }
  }
  like.Q=matrix(0,ncol=extendend,nrow=Q)
  like.Q[1,]=all.seg[1,]
  cp=matrix(NA,ncol=extendend,nrow=Q)
  for(q in 2:Q){
    for(j in (q*minseglen):extendend){
      like=NULL
      if((j-2*minseglen-q)<0){v=q*minseglen}
      else{v=(q*minseglen):(j-2)}
      like=like.Q[q-1,v]+all.seg[v+1,j]

      like.Q[q,j]= max(like,na.rm=TRUE)
      cp[q,j]=which(like==max(like,na.rm=TRUE))[1]+(q*minseglen-1)
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


multiple.meanvar.pp=function(data,mul.method="PELT",penalty="MBIC",pen.value=0,Q=5,class=TRUE,param.estimates=TRUE,minseglen){
  if(!((mul.method=="PELT")||(mul.method=="BinSeg")||(mul.method=="SegNeigh"))){
    stop("Multiple Method is not recognised")
  }
  costfunc = "meanvar.pp"
  if(penalty=="MBIC"){
    if(mul.method=="SegNeigh"){
      stop('MBIC penalty not implemented for SegNeigh method, please choose an alternative penalty')
    }
    costfunc = "meanvar.pp.mbic"
  }

  diffparam=1
  if(is.null(dim(data))==TRUE || length(dim(data)) == 1){
    # single dataset
    nevents=length(data)-2
  }
  else{
    nevents=ncol(data)-2
  }
  if(nevents<2){stop('Data must have atleast 2 events (plus the start and end observation times) to fit a changepoint model.')}
  if(nevents<(2*minseglen)){stop('Minimum segment legnth is too large to include a change in this data')}

  pen.value = penalty_decision(penalty, pen.value, 2*nevents+1, # 2*nevents because n here is the "length" of the data considered,
                               # so each event can have a change just before or at the change, then +1 because we include the final data point
                               diffparam=1, asymcheck=costfunc, method=mul.method)
  if(is.null(dim(data))==TRUE || length(dim(data)) == 1){
    # single dataset
    out = data_input(data=data,method=mul.method,pen.value=pen.value,costfunc=costfunc,minseglen=minseglen,Q=Q)
    # all cpts returned on the longer data length so need transforming back to the original scale
    out[[2]]=floor(out[[2]]/2 +1) # replace the long cpts with the original scale cpts
    out[[2]][length(out[[2]])]=out[[2]][length(out[[2]])]+1 # Need to add 1 to the final value so it is the end point and not the last observation
    inc.event=out[[2]]%%2 # 0 is don't include, 1 is include
    inc.event[length(inc.event)]=0 # as the final value isn't an event

    if(class==TRUE){
      return(out.prep=class_input(data, cpttype="mean and variance", method=mul.method, test.stat="Poisson Process",
            penalty=penalty, pen.value=pen.value, minseglen=minseglen, param.estimates=param.estimates, out=out, Q=Q,
            shape=inc.event))
    }
    else{
      return(list(cpts=out[[2]],include.event=inc.event))
    }
  }
  else{
    rep=nrow(data)
    out=list()
    for(i in 1:rep){
      out[[i]]=data_input(data[i,],method=mul.method,pen.value=pen.value,costfunc=costfunc,minseglen=minseglen,Q=Q)
    }

    cpts=lapply(out, '[[', 2)

    # all cpts returned on the longer data length so need transforming back to the original scale
    inc.event=lapply(cpts,FUN=function(x){x%%2}) # 0 is don't include, 1 is include
    cpts=lapply(cpts,FUN=function(x){floor(x/2 +1)}) # replace the long cpts with the original scale cpts

    if(class==TRUE){
      ans=list()
      for(i in 1:rep){
        ans[[i]]=class_input(data[i,], cpttype="mean and variance", method=mul.method, test.stat="Poisson Process",
              penalty=penalty, pen.value=pen.value, minseglen=minseglen, param.estimates=param.estimates, out=out[[i]],
              Q=Q,shape=inc.event)
      }
      return(ans)
    }
    else{return(cpts)}
  }
}
