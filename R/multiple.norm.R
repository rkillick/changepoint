# PELT.var.norm=function(data,pen=0,know.mean=FALSE,mu=NA,nprune=FALSE){
#   mll.var.EFK=function(x,n){
#     neg=x<=0
#     x[neg==TRUE]=0.00000000001    
#     return( n*(log(2*pi)+log(x/n)+1))
#   }
#   if((know.mean==FALSE)&(is.na(mu))){
# 	mu=mean(data)
#   }
#   n=length(data)
#   y2=c(0,cumsum((data-mu)^2))
# 
#   lastchangecpts=matrix(NA,nrow=n,ncol=2)
#   lastchangelike=matrix(NA,nrow=n,ncol=2)
#   checklist=NULL
#   lastchangelike[1,]=c(mll.var.EFK(y2[2],1),mll.var.EFK(y2[n+1]-y2[2],n-1)+pen)
#   lastchangecpts[1,]=c(0,1)
#   lastchangelike[2,]=c(mll.var.EFK(y2[3],2),mll.var.EFK(y2[n+1]-y2[3],n-2)+pen)
#   lastchangecpts[2,]=c(0,2)
#   lastchangelike[3,]=c(mll.var.EFK(y2[4],3),mll.var.EFK(y2[n+1]-y2[4],n-3)+pen)
#   lastchangecpts[3,]=c(0,3)
#   noprune=NULL
#   for(tstar in 4:n){
#     tmplike=NULL
#     tmpt=c(checklist, tstar-2)
#     tmplike=lastchangelike[tmpt,1]+mll.var.EFK(y2[tstar+1]-y2[tmpt+1],tstar-tmpt)+pen
#     if(tstar==n){
#       lastchangelike[tstar,]=c(min(c(tmplike,mll.var.EFK(y2[tstar+1]-y2[1],tstar)),na.rm=TRUE),0)
#     }
#     else{
#       lastchangelike[tstar,]=c(min(c(tmplike,mll.var.EFK(y2[tstar+1]-y2[1],tstar)),na.rm=TRUE),mll.var.EFK(y2[n+1]-y2[tstar+1],n-tstar)+pen)
#     }
#     if(lastchangelike[tstar,1]==mll.var.EFK(y2[tstar+1]-y2[1],tstar)){
#       lastchangecpts[tstar,]=c(0,tstar)
#     }
#     else{
#       cpt=tmpt[tmplike==lastchangelike[tstar,1]][1]
#       lastchangecpts[tstar,]=c(cpt,tstar)
#     }
#     checklist=tmpt[tmplike<=lastchangelike[tstar,1]+pen]
#     if(nprune==TRUE){
#       noprune=c(noprune,length(checklist))
#     }
#   }
#   if(nprune==TRUE){
#     return(nprune=noprune)
#   }
#   else{
#     fcpt=NULL
#     last=n
#     while(last!=0){
# 	fcpt=c(fcpt,lastchangecpts[last,2])
# 	last=lastchangecpts[last,1]
#     }
#     return(cpt=sort(fcpt))
#   }
# }


# PELT.mean.norm=function(data,pen=0,nprune=FALSE){
#   mll.mean.EFK=function(x2,x,n){
#     return( x2-(x^2)/n)
#   }
#   n=length(data)
#   y2=c(0,cumsum(data^2))
#   y=c(0,cumsum(data))
# 
#   lastchangecpts=matrix(NA,nrow=n,ncol=2)
#   lastchangelike=matrix(NA,nrow=n,ncol=2)
#   checklist=NULL
#   lastchangelike[1,]=c(mll.mean.EFK(y2[2],y[2],1),mll.mean.EFK(y2[n+1]-y2[2],y[n+1]-y[2],n-1)+pen)
#   lastchangecpts[1,]=c(0,1)
#   lastchangelike[2,]=c(mll.mean.EFK(y2[3],y[3],2),mll.mean.EFK(y2[n+1]-y2[3],y[n+1]-y[3],n-2)+pen)
#   lastchangecpts[2,]=c(0,2)
#   lastchangelike[3,]=c(mll.mean.EFK(y2[4],y[4],3),mll.mean.EFK(y2[n+1]-y2[4],y[n+1]-y[4],n-3)+pen)
#   lastchangecpts[3,]=c(0,3)
#   noprune=NULL
#   for(tstar in 4:n){
#     tmplike=NULL
#     tmpt=c(checklist, tstar-2)
#     tmplike=lastchangelike[tmpt,1]+mll.mean.EFK(y2[tstar+1]-y2[tmpt+1],y[tstar+1]-y[tmpt+1],tstar-tmpt)+pen
#     if(tstar==n){
#       lastchangelike[tstar,]=c(min(c(tmplike,mll.mean.EFK(y2[tstar+1],y[tstar+1],tstar)),na.rm=TRUE),0)
#     }
#     else{
#       lastchangelike[tstar,]=c(min(c(tmplike,mll.mean.EFK(y2[tstar+1],y[tstar+1],tstar)),na.rm=TRUE),mll.mean.EFK(y2[n+1]-y2[tstar+1],y[n+1]-y[tstar+1],n-tstar)+pen)
#     }
#     if(lastchangelike[tstar,1]==mll.mean.EFK(y2[tstar+1],y[tstar+1],tstar)){
#       lastchangecpts[tstar,]=c(0,tstar)
#     }
#     else{
#       cpt=tmpt[tmplike==lastchangelike[tstar,1]][1]
#       lastchangecpts[tstar,]=c(cpt,tstar)
#     }
#     checklist=tmpt[tmplike<=lastchangelike[tstar,1]+pen]
#     if(nprune==TRUE){
#       noprune=c(noprune,length(checklist))
#     }
#   }
#   if(nprune==TRUE){
#     return(noprune)
#   }
#   else{
#     fcpt=NULL
#     last=n
#     while(last!=0){
# 	fcpt=c(fcpt,lastchangecpts[last,2])
# 	last=lastchangecpts[last,1]
#     }
#     return(cpt=sort(fcpt))
#   }
# }

# PELT.meanvar.norm=function(data,pen=0,nprune=FALSE){
#   mll.meanvar.EFK=function(x2,x,n){
#     sigmasq=(1/n)*(x2-(x^2)/n)
#     neg=sigmasq<=0
#     sigmasq[neg==TRUE]=0.00000000001
#     return(n*(log(2*pi)+log(sigmasq)+1))
#   }
#   n=length(data)
#   y2=c(0,cumsum(data^2))
#   y=c(0,cumsum(data))
# 
#   lastchangecpts=matrix(NA,nrow=n,ncol=2)
#   lastchangelike=matrix(NA,nrow=n,ncol=2)
#   checklist=NULL
#   lastchangelike[1,]=c(mll.meanvar.EFK(y2[2],y[2],1),mll.meanvar.EFK(y2[n+1]-y2[2],y[n+1]-y[2],n-1)+pen)
#   lastchangecpts[1,]=c(0,1)
#   lastchangelike[2,]=c(mll.meanvar.EFK(y2[3],y[3],2),mll.meanvar.EFK(y2[n+1]-y2[3],y[n+1]-y[3],n-2)+pen)
#   lastchangecpts[2,]=c(0,2)
#   lastchangelike[3,]=c(mll.meanvar.EFK(y2[4],y[4],3),mll.meanvar.EFK(y2[n+1]-y2[4],y[n+1]-y[4],n-3)+pen)
#   lastchangecpts[3,]=c(0,3)
#   noprune=NULL
#   for(tstar in 4:n){
#     tmplike=NULL
#     tmpt=c(checklist, tstar-2)
#     tmplike=lastchangelike[tmpt,1]+mll.meanvar.EFK(y2[tstar+1]-y2[tmpt+1],y[tstar+1]-y[tmpt+1],tstar-tmpt)+pen
#     if(tstar==n){
#       lastchangelike[tstar,]=c(min(c(tmplike,mll.meanvar.EFK(y2[tstar+1],y[tstar+1],tstar)),na.rm=TRUE),0)
#     }
#     else{
#       lastchangelike[tstar,]=c(min(c(tmplike,mll.meanvar.EFK(y2[tstar+1],y[tstar+1],tstar)),na.rm=TRUE),mll.meanvar.EFK(y2[n+1]-y2[tstar+1],y[n+1]-y[tstar+1],n-tstar)+pen)
#     }
#     if(lastchangelike[tstar,1]==mll.meanvar.EFK(y2[tstar+1],y[tstar+1],tstar)){
#       lastchangecpts[tstar,]=c(0,tstar)
#     }
#     else{
#       cpt=tmpt[tmplike==lastchangelike[tstar,1]][1]
#       lastchangecpts[tstar,]=c(cpt,tstar)
#     }
#     checklist=tmpt[tmplike<=lastchangelike[tstar,1]+pen]
#     if(nprune==TRUE){
#       noprune=c(noprune,length(checklist))
#     }
#   }
#   if(nprune==TRUE){
#     return(noprune)
#   }
#   else{
#     fcpt=NULL
#     last=n
#     while(last!=0){
# 	fcpt=c(fcpt,lastchangecpts[last,2])
# 	last=lastchangecpts[last,1]
#     }
#     return(cpt=sort(fcpt))
#   }
# }


segneigh.var.norm=function(data,Q=5,pen=0,know.mean=FALSE,mu=NA){
  n=length(data)
  if(n<4){stop('Data must have atleast 4 observations to fit a changepoint model.')}
  
  if(Q>((n/2)+1)){stop(paste('Q is larger than the maximum number of segments',(n/2)+1))}
  if((know.mean==FALSE)&(is.na(mu))){
    mu=mean(data)
  }
  all.seg=matrix(0,ncol=n,nrow=n)
  for(i in 1:n){
    ssq=0
    for(j in i:n){
      m=j-i+1
      ssq=ssq+(data[j]-mu)^2
      if(ssq<=0){sigmasq=0.00000000001/m}
      else{sigmasq=ssq/m}
      all.seg[i,j]=-(m/2)*(log(2*pi)+log(sigmasq)+1)
    }
  }
  like.Q=matrix(0,ncol=n,nrow=Q)
  like.Q[1,]=all.seg[1,]
  cp=matrix(NA,ncol=n,nrow=Q)
  for(q in 2:Q){
    for(j in q:n){
      like=NULL
      if((j-2-q)<0){
        like=-Inf
      }
      else{
        v=(q):(j-2)
        like=like.Q[q-1,v]+all.seg[v+1,j]
      }
      
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
  
  return(list(cps=t(apply(cps.Q,1,sort,na.last=TRUE)),cpts=cpts,op.cpts=op.cps,pen=pen,like=criterion[op.cps+1],like.Q=-2*like.Q[,n]))
}


segneigh.mean.norm=function(data,Q=5,pen=0){
  n=length(data)
  if(n<2){stop('Data must have atleast 2 observations to fit a changepoint model.')}
  
  if(Q>((n/2)+1)){stop(paste('Q is larger than the maximum number of segments',(n/2)+1))}
  all.seg=matrix(0,ncol=n,nrow=n)
  for(i in 1:n){
    ssq=0
    sumx=0
    for(j in i:n){
      len=j-i+1
      sumx=sumx+data[j]
      ssq=ssq+data[j]^2
      all.seg[i,j]=-0.5*(ssq-(sumx^2)/len)
    }
  }
  like.Q=matrix(0,ncol=n,nrow=Q)
  like.Q[1,]=all.seg[1,]
  cp=matrix(NA,ncol=n,nrow=Q)
  for(q in 2:Q){
    for(j in q:n){
      like=NULL
      v=(q-1):(j-1)
      like=like.Q[q-1,v]+all.seg[v+1,j]
      
      like.Q[q,j]= max(like,na.rm=TRUE)
      cp[q,j]=which(like==max(like,na.rm=TRUE))[1]+(q-2)
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
  
  return(list(cps=t(apply(cps.Q,1,sort,na.last=TRUE)),cpts=cpts,op.cpts=op.cps,pen=pen,like=criterion[op.cps+1],like.Q=-2*like.Q[,n]))
}


segneigh.meanvar.norm=function(data,Q=5,pen=0){
  n=length(data)
  if(n<4){stop('Data must have atleast 4 observations to fit a changepoint model.')}
  
  if(Q>((n/2)+1)){stop(paste('Q is larger than the maximum number of segments',(n/2)+1))}
  all.seg=matrix(0,ncol=n,nrow=n)
  for(i in 1:n){
    ssq=0
    sumx=0
    for(j in i:n){
      len=j-i+1
      sumx=sumx+data[j]
      ssq=ssq+data[j]^2
      sigmasq=(1/len)*(ssq-(sumx^2)/len)
      if(sigmasq<=0){sigmasq=0.00000000001}
      all.seg[i,j]=-(len/2)*(log(2*pi)+log(sigmasq)+1)
    }
  }
  like.Q=matrix(0,ncol=n,nrow=Q)
  like.Q[1,]=all.seg[1,]
  cp=matrix(NA,ncol=n,nrow=Q)
  for(q in 2:Q){
    for(j in q:n){
      like=NULL
      if((j-2-q)<0){
        like=-Inf
      }
      else{
        v=(q):(j-2)
        like=like.Q[q-1,v]+all.seg[v+1,j]
      }
      
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
  
  
  return(list(cps=t(apply(cps.Q,1,sort,na.last=TRUE)),cpts=cpts,op.cpts=op.cps,pen=pen,like=criterion[op.cps+1],like.Q=-2*like.Q[,n]))
}


#binseg.var.norm=function(data,Q=5,pen=0,know.mean=FALSE,mu=NA){
#  mll.var=function(x,n){
#    neg=x<=0
#    x[neg==TRUE]=0.00000000001    
#    return( -0.5*n*(log(2*pi)+log(x/n)+1))
#  }
#  n=length(data)
#  if((know.mean==FALSE)&(is.na(mu))){
#	mu=mean(data)
#  }
#  y2=c(0,cumsum((data-mu)^2))
#  tau=c(0,n)
#  cpt=matrix(0,nrow=2,ncol=Q)
#  oldmax=1000
#
#  for(q in 1:Q){
#    lambda=rep(0,n-1)
#    i=1
#    st=tau[1]+1;end=tau[2]
#    null=mll.var(y2[end+1]-y2[st],end-st+1)
#    for(j in 1:(n-1)){
#      if(j==end){
#        st=end+1;i=i+1;end=tau[i+1]
#        null=mll.var(y2[end+1]-y2[st],end-st+1)
#      }else{
#        lambda[j]=mll.var(y2[j+1]-y2[st],j-st+1)+mll.var(y2[end+1]-y2[j+1],end-j)-null
#      }
#    }
#    k=which.max(lambda)[1]
#    cpt[1,q]=k;cpt[2,q]=min(oldmax,max(lambda))
#    oldmax=min(oldmax,max(lambda))
#    tau=sort(c(tau,k))
#  }
#  op.cps=NULL
#  p=1:(Q-1)
#  for(i in 1:length(pen)){
#    criterion=(2*cpt[2,])>=pen[i]
#    if(sum(criterion)==0){
#      op.cps=0
#    }
#    else{
#      op.cps=c(op.cps,max(which((criterion)==TRUE)))
#    }
#  }
#  return(list(cps=cpt,op.cpts=op.cps,pen=pen))
#}


# binseg.mean.norm=function(data,Q=5,pen=0){
#   mll.mean=function(x2,x,n){
#     return( -0.5*(x2-(x^2)/n))
#   }
#   n=length(data)
#   y2=c(0,cumsum(data^2))
#   y=c(0,cumsum(data))
#   tau=c(0,n)
#   cpt=matrix(0,nrow=2,ncol=Q)
#   oldmax=1000
# 
#   for(q in 1:Q){
#     lambda=rep(0,n-1)
#     i=1
#     st=tau[1]+1;end=tau[2]
#     null=mll.mean(y2[end+1]-y2[st],y[end+1]-y[st],end-st+1)
#     for(j in 1:(n-1)){
#       if(j==end){
#         st=end+1;i=i+1;end=tau[i+1]
#         null=mll.mean(y2[end+1]-y2[st],y[end+1]-y[st],end-st+1)
#       }else{
#         lambda[j]=mll.mean(y2[j+1]-y2[st],y[j+1]-y[st],j-st+1)+mll.mean(y2[end+1]-y2[j+1],y[end+1]-y[j+1],end-j)-null
#       }
#     }
#     k=which.max(lambda)[1]
#     cpt[1,q]=k;cpt[2,q]=min(oldmax,max(lambda)) # done so that when we do the decision later we can take the max(which(criterion==T)), rather than min(which(criterion==F))-1
#     oldmax=min(oldmax,max(lambda))
#     tau=sort(c(tau,k))
#   }
#   op.cps=NULL
#   p=1:(Q-1)
#   for(i in 1:length(pen)){
#     criterion=(2*cpt[2,])>=pen[i]
#     if(sum(criterion)==0){
#       op.cps=0
#     }
#     else{
#       op.cps=c(op.cps,max(which((criterion)==TRUE)))
#     }
#   }
#   return(list(cps=cpt,op.cpts=op.cps,pen=pen))
# }

# binseg.meanvar.norm=function(data,Q=5,pen=0){
#   mll.meanvar=function(x2,x,n){
#     sigmasq=(1/n)*(x2-(x^2)/n)
#     neg=sigmasq<=0
#     sigmasq[neg==TRUE]=0.00000000001
#     return(-(n/2)*(log(2*pi)+log(sigmasq)+1))
#   }
#   n=length(data)
#   y2=c(0,cumsum(data^2))
#   y=c(0,cumsum(data))
#   tau=c(0,n)
#   cpt=matrix(0,nrow=2,ncol=Q)
#   oldmax=1000
# 
#   for(q in 1:Q){
#     lambda=rep(0,n-1)
#     i=1
#     st=tau[1]+1;end=tau[2]
#     null=mll.meanvar(y2[end+1]-y2[st],y[end+1]-y[st],end-st+1)
#     for(j in 1:(n-1)){
#       if(j==end){
#         st=end+1;i=i+1;end=tau[i+1]
#         null=mll.meanvar(y2[end+1]-y2[st],y[end+1]-y[st],end-st+1)
#       }else{
# 	if((j-st)<2){lambda[j]=-1*10^(100)}
# 	else if((end-j)<2){lambda[j]=-1*10^(100)}
# 	else{lambda[j]=mll.meanvar(y2[j+1]-y2[st],y[j+1]-y[st],j-st+1)+mll.meanvar(y2[end+1]-y2[j+1],y[end+1]-y[j+1],end-j)-null}
#       }
#     }
#     k=which.max(lambda)[1]
#     cpt[1,q]=k;cpt[2,q]=min(oldmax,max(lambda))
#     oldmax=min(oldmax,max(lambda))
#     tau=sort(c(tau,k))
#   }
#   op.cps=NULL
#   p=1:(Q-1)
#   for(i in 1:length(pen)){
#     criterion=(2*cpt[2,])>=pen[i]
#     if(sum(criterion)==0){
#       op.cps=0
#     }
#     else{
#       op.cps=c(op.cps,max(which((criterion)==TRUE)))
#     }
#   }
#   return(list(cps=cpt,op.cpts=op.cps,pen=pen))
# }


multiple.var.norm=function(data,mul.method="PELT",penalty="MBIC",pen.value=0,Q=5,know.mean=FALSE,mu=NA,class=TRUE,param.estimates=TRUE, minseglen=2){
  if(!((mul.method=="PELT")||(mul.method=="BinSeg")||(mul.method=="SegNeigh"))){
    stop("Multiple Method is not recognised")
  }
  costfunc = "var.norm"
  if(penalty =="MBIC"){
    if(mul.method=="SegNeigh"){
      stop('MBIC penalty not implemented for SegNeigh method, please choose an alternative penalty')
    }
    costfunc = "var.norm.mbic"
  }
  diffparam=1
  if(is.null(dim(data))==TRUE){
    # single dataset
    n=length(data)
    mu=mu[1]
  }
  else{
    n=ncol(data)
  }
  if(n<4){stop('Data must have atleast 4 observations to fit a changepoint model.')}
  
  pen.value = penalty_decision(penalty, pen.value, n, diffparam, asymcheck=costfunc, method=mul.method)

    if(is.null(dim(data))==TRUE){
    # single dataset
    if((know.mean==FALSE)&(is.na(mu))){
      mu=mean(coredata(data))
    }
    out = data_input(data=data,method=mul.method,pen.value=pen.value,costfunc=costfunc,minseglen=minseglen,Q=Q,var=mu)
    
    if(class==TRUE){
      out=class_input(data, cpttype="variance", method=mul.method, test.stat="Normal", penalty=penalty, pen.value=pen.value, minseglen=minseglen, param.estimates=param.estimates, out=out, Q=Q)
      param.est(out)=c(param.est(out),mean=mu)
      return(out)
    }
    else{ return(out[[2]])}
  }
  else{
    rep=nrow(data)
    out=list()
    if(length(mu)!=rep){
      mu=rep(mu,rep)
    }
    
    for(i in 1:rep){
      if((know.mean==FALSE)&(is.na(mu[i]))){
        mu=mean(coredata(data[i,]))
      }
      out[[i]]=data_input(data[i,],method=mul.method,pen.value=pen.value,costfunc=costfunc,minseglen=minseglen,Q=Q,var=mu)
    }
    
      cpts=lapply(out, '[[', 2)
        
    if(class==TRUE){
      ans=list()
      for(i in 1:rep){
        ans[[i]]=class_input(data[i,], cpttype="variance", method=mul.method, test.stat="Normal", penalty=penalty, pen.value=pen.value, minseglen=minseglen, param.estimates=param.estimates, out=out[[i]], Q=Q)
        param.est(ans[[i]])=c(param.est(ans[[i]]),mean=mu[i])
      }
      return(ans)
    }
    else{return(cpts)}
  }
}


multiple.mean.norm=function(data,mul.method="PELT",penalty="MBIC",pen.value=0,Q=5,class=TRUE,param.estimates=TRUE,minseglen){
  if(!((mul.method=="PELT")||(mul.method=="BinSeg")||(mul.method=="SegNeigh"))){
    stop("Multiple Method is not recognised")
  }
  costfunc = "mean.norm"
  if(penalty=="MBIC"){
    if(mul.method=="SegNeigh"){
      stop('MBIC penalty not implemented for SegNeigh method, please choose an alternative penalty')
    }
    costfunc = "mean.norm.mbic"
  }
  diffparam=1
  if(is.null(dim(data))==TRUE){
    # single dataset
    n=length(data) # still works if data is of class ts
  }
  else{
    n=ncol(data)
  }
  
  pen.value = penalty_decision(penalty, pen.value, n, diffparam, asymcheck = costfunc, method=mul.method)

    if(is.null(dim(data))==TRUE){
    # single dataset
    out = data_input(data=data,method=mul.method,pen.value=pen.value,costfunc=costfunc,minseglen=minseglen,Q=Q)
     
    if(class==TRUE){
      return(class_input(data, cpttype="mean", method=mul.method, test.stat="Normal", penalty=penalty, pen.value=pen.value, minseglen=minseglen, param.estimates=param.estimates, out=out, Q=Q))
    }
    else{ return(out[[2]])}
  }
  else{
    rep=nrow(data)
    out=list()
    if(class==TRUE){cpts=list()}
    for(i in 1:rep){
      out[[i]]=data_input(data[i,],method=mul.method,pen.value=pen.value,costfunc=costfunc,minseglen=minseglen,Q=Q)
    }
    
    cps=lapply(out, '[[', 2)
    
    if(class==TRUE){
      ans=list()
      for(i in 1:rep){
        ans[[i]]=class_input(data[i,], cpttype="mean", method=mul.method, test.stat="Normal", penalty=penalty, pen.value=pen.value, minseglen=minseglen, param.estimates=param.estimates, out=out[[i]], Q=Q)
      }
      return(ans)
    }
    else{return(cps)}
  }
}

multiple.meanvar.norm=function(data,mul.method="PELT",penalty="MBIC",pen.value=0,Q=5,class=TRUE,param.estimates=TRUE,minseglen){
  if(!((mul.method=="PELT")||(mul.method=="BinSeg")||(mul.method=="SegNeigh"))){
    stop("Multiple Method is not recognised")
  }
  costfunc = "meanvar.norm"
  if(penalty=="MBIC"){
    if(mul.method=="SegNeigh"){
      stop('MBIC penalty not implemented for SegNeigh method, please choose an alternative penalty')
    }
    costfunc = "meanvar.norm.mbic"
  }
  diffparam=2
  if(is.null(dim(data))==TRUE){
    # single dataset
    n=length(data)
  }
  else{
    n=ncol(data)
  }
  pen.value = penalty_decision(penalty, pen.value, n, diffparam, asymcheck = costfunc, method=mul.method)

    if(is.null(dim(data))==TRUE){
    # single dataset
    out = data_input(data=data,method=mul.method,pen.value=pen.value,costfunc=costfunc,minseglen=minseglen,Q=Q)

    if(class==TRUE){
      return(class_input(data, cpttype="mean and variance", method=mul.method, test.stat="Normal", penalty=penalty, pen.value=pen.value, minseglen=minseglen, param.estimates=param.estimates, out=out, Q=Q))
    }
    else{ return(out[[2]])}
  }
  else{
    rep=nrow(data)
    out=list()
    for(i in 1:rep){
      out[[i]]=data_input(data[i,],method=mul.method,pen.value=pen.value,costfunc=costfunc,minseglen=minseglen,Q=Q)
    }
  
    cps=lapply(out, '[[', 2)
  
    if(class==TRUE){
      ans=list()
      for(i in 1:rep){
        ans[[i]] = class_input(data[i,], cpttype="mean and variance", method=mul.method, test.stat="Normal", penalty=penalty, pen.value=pen.value, minseglen=minseglen, param.estimates=param.estimates, out=out[[i]], Q=Q)
      }
      return(ans)
    }
    else{return(cps)}
  }
}
