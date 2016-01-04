segneigh.var.css=function(data,Q=5,pen=0){
  n=length(data)
  if(n<4){stop('Data must have atleast 4 observations to fit a changepoint model.')}
  if(Q>((n/2)+1)){stop(paste('Q is larger than the maximum number of segments',(n/2)+1))}
  
  y2=c(0,cumsum(data^2))
  oldmax=1000
  
  test=NULL
  like.Q=matrix(0,ncol=n,nrow=Q)
  cp=matrix(NA,ncol=n,nrow=Q)
  for(q in 2:Q){ # no of segments
    for(j in q:n){
      like=NULL
      v=(q-1):(j-1)
      if(q==2){
        like=abs(sqrt(j/2)*(y2[v+1]/y2[j+1] -v/j))
      }
      else{
        like=like.Q[q-1,v]+abs(sqrt((j-cp[q-1,v])/2)*((y2[v+1]-y2[cp[q-1,v]+1])/(y2[j+1]-y2[cp[q-1,v]+1]) -(v-cp[q-1,v])/(j-cp[q-1,v])))
      }
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
  
  op.cps=0
  flag=0
  for(q in 2:Q){
    criterion=NULL
    cpttmp=c(0,sort(cps.Q[q,1:(q-1)]),n)
    for(i in 1:(q-1)){
      criterion[i]=abs(sqrt((cpttmp[i+2]-cpttmp[i])/2)*((y2[cpttmp[i+1]+1]-y2[cpttmp[i]+1])/(y2[cpttmp[i+2]+1]-y2[cpttmp[i]+1]) -(cpttmp[i+1]-cpttmp[i])/(cpttmp[i+2]-cpttmp[i])))
      if(criterion[i]<pen){flag=1}
    }
    if(flag==1){
      break
    }
    op.cps=op.cps+1
  }
  if(op.cps==(Q-1)){warning('The number of segments identified is Q, it is advised to increase Q to make sure changepoints have not been missed.')}
  if(op.cps==0){cpts=n}
  else{cpts=c(sort(cps.Q[op.cps+1,][cps.Q[op.cps+1,]>0]),n)}
  
  return(list(cps=t(apply(cps.Q,1,sort,na.last=TRUE)),cpts=cpts,op.cpts=op.cps,pen=pen,like=criterion[op.cps+1],like.Q=like.Q[,n]))
}



binseg.var.css=function(data,Q=5,pen=0){
  n=length(data)
  if(n<4){stop('Data must have atleast 4 observations to fit a changepoint model.')}
  if(Q>((n/2)+1)){stop(paste('Q is larger than the maximum number of segments',(n/2)+1))}
  
  y2=c(0,cumsum(data^2))
  tau=c(0,n)
  cpt=matrix(0,nrow=2,ncol=Q)
  oldmax=Inf
  
  for(q in 1:Q){
    lambda=rep(0,n-1)
    i=1
    st=tau[1]+1;end=tau[2]
    for(j in 1:(n-1)){
      if(j==end){
        st=end+1;i=i+1;end=tau[i+1]
      }else{
        lambda[j]=sqrt((end-st+1)/2)*((y2[j+1]-y2[st])/(y2[end+1]-y2[st]) -(j-st+1)/(end-st+1))
      }
    }
    k=which.max(abs(lambda))
    cpt[1,q]=k;cpt[2,q]=min(oldmax,max(abs(lambda),na.rm=T))
    oldmax=min(oldmax,max(abs(lambda),na.rm=T))
    tau=sort(c(tau,k))
  }
  op.cps=NULL
  p=1:(Q-1)
  for(i in 1:length(pen)){
    criterion=(cpt[2,])>=pen[i]
    if(sum(criterion)==0){
      op.cps=0
    }
    else{
      op.cps=c(op.cps,max(which((criterion)==TRUE)))
    }
  }
  if(op.cps==Q){warning('The number of changepoints identified is Q, it is advised to increase Q to make sure changepoints have not been missed.')}
  
  if(op.cps==0){cpts=n}
  else{cpts=c(sort(cpt[1,1:op.cps]),n)}
  
  return(list(cps=cpt,cpts=cpts,op.cpts=op.cps,pen=pen))
}


multiple.var.css=function(data,mul.method="BinSeg",penalty="MBIC",pen.value=0,Q=5,class=TRUE,param.estimates=TRUE,minseglen){
  if(mul.method=="PELT"){ stop("CSS does not satisfy the assumptions of PELT, use SegNeigh or BinSeg instead.") }
  else if(!((mul.method=="BinSeg")||(mul.method=="SegNeigh"))){
    stop("Multiple Method is not recognised")
  }
  if(penalty!="MBIC"){
    costfunc = "var.css"
  }else{
    stop("MBIC penalty is not valid for nonparametric test statistics.")
  }
  diffparam=1
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
    if(mul.method=="BinSeg"){
      			out=binseg.var.css(data,Q,pen.value)
    }
    else if(mul.method=="SegNeigh"){
      out=segneigh.var.css(data,Q,pen.value)
    }
    if(class==TRUE){
      return(class_input(data, cpttype="variance", method=mul.method, test.stat="CSS", penalty=penalty, pen.value=pen.value, minseglen=minseglen, param.estimates=param.estimates, out=out, Q=Q))
    }
    else{ return(out)}
  }
  else{
    rep=nrow(data)
    out=list()
    if(class==TRUE){cpts=list()}
    if(mul.method=="BinSeg"){
      for(i in 1:rep){
  			out=c(out,list(binseg.var.css(data[i,],Q,pen.value)))
      }
      if(class==TRUE){cpts=out}
    }
    else if(mul.method=="SegNeigh"){
      for(i in 1:rep){
        out=c(out,list(segneigh.var.css(data[i,],Q,pen.value)))
      }
    }
    if(class==TRUE){
      ans=list()
      for(i in 1:rep){
        ans[[i]]=class_input(data[i,], cpttype="variance", method=mul.method, test.stat="CSS", penalty=penalty, pen.value=pen.value, minseglen=minseglen, param.estimates=param.estimates, out=out[[i]], Q=Q)
      }
      return(ans)
    }
    else{return(out)}
  }
}

















segneigh.mean.cusum=function(data,Q=5,pen=0){
  n=length(data)
  if(n<2){stop('Data must have atleast 2 observations to fit a changepoint model.')}
  if(Q>((n/2)+1)){stop(paste('Q is larger than the maximum number of segments',(n/2)+1))}
  
  y=c(0,cumsum(data))
  oldmax=1000
  
  test=NULL
  like.Q=matrix(0,ncol=n,nrow=Q)
  cp=matrix(NA,ncol=n,nrow=Q)
  for(q in 2:Q){ # no of segments
    for(j in q:n){
      like=NULL
      v=(q-1):(j-1)
      if(q==2){
        like=abs((y[v+1]-(v/j)*y[j+1])/j)
      }
      else{
        like=like.Q[q-1,v]+abs(((y[v+1]-y[cp[q-1,v]+1])-((v-cp[q-1,v])/(j-cp[q-1,v]))*(y[j+1]-y[cp[q-1,v]+1]))/(j-cp[q-1,v]))
      }
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
  
  op.cps=0
  flag=0
  for(q in 2:Q){
    criterion=NULL
    cpttmp=c(0,sort(cps.Q[q,1:(q-1)]),n)
    for(i in 1:(q-1)){
      criterion[i]=abs(((y[cpttmp[i+1]+1]-y[cpttmp[i]+1])-((cpttmp[i+1]-cpttmp[i])/(cpttmp[i+2]-cpttmp[i]))*(y[cpttmp[i+2]+1]-y[cpttmp[i]+1]))/(cpttmp[i+2]-cpttmp[i]))
      if(criterion[i]<pen){flag=1}
    }
    if(flag==1){
      break
    }
    op.cps=op.cps+1
  }
  
  if(op.cps==(Q-1)){warning('The number of segments identified is Q, it is advised to increase Q to make sure changepoints have not been missed.')}
  
  if(op.cps==0){cpts=n}
  else{cpts=c(sort(cps.Q[op.cps+1,][cps.Q[op.cps+1,]>0]),n)}
  
  return(list(cps=t(apply(cps.Q,1,sort,na.last=TRUE)),cpts=cpts,op.cpts=op.cps,pen=pen,like=criterion[op.cps+1],like.Q=like.Q[,n]))
}


binseg.mean.cusum=function(data,Q=5,pen=0){
  n=length(data)
  if(n<2){stop('Data must have atleast 2 observations to fit a changepoint model.')}
  if(Q>((n/2)+1)){stop(paste('Q is larger than the maximum number of segments',(n/2)+1))}
  
  y=c(0,cumsum(data))
  tau=c(0,n)
  cpt=matrix(0,nrow=2,ncol=Q)
  oldmax=Inf
  
  for(q in 1:Q){
    lambda=rep(0,n-1)
    i=1
    st=tau[1]+1;end=tau[2]
    for(j in 1:(n-1)){
      #       for(j in 1:(n-1)){#minseglen:(n-minseglen)
      #         if(j==end){#end-sinseglen+1
      #           st=end+1;i=i+1;end=tau[i+1]
      #           # j=j+minseglen
      
      
      
      if(j==end){
        st=end+1;i=i+1;end=tau[i+1]
      }else{
        lambda[j]=((y[j+1]-y[st])-((j-st+1)/(end-st+1))*(y[end+1]-y[st]))/(end-st+1)
      }
    }
    k=which.max(abs(lambda))
    cpt[1,q]=k;cpt[2,q]=min(oldmax,max(abs(lambda)))
    oldmax=min(oldmax,max(abs(lambda)))
    tau=sort(c(tau,k))
  }
  op.cps=NULL
  p=1:(Q-1)
  for(i in 1:length(pen)){
    criterion=(cpt[2,])>=pen[i]
    if(sum(criterion)==0){
      op.cps=0
    }
    else{
      op.cps=c(op.cps,max(which((criterion)==TRUE)))
    }
  }
  if(op.cps==Q){warning('The number of changepoints identified is Q, it is advised to increase Q to make sure changepoints have not been missed.')}
  
  if(op.cps==0){cpts=n}
  else{cpts=c(sort(cpt[1,1:op.cps]),n)}
  
  return(list(cps=cpt,cpts=cpts,op.cpts=op.cps,pen=pen))
}


multiple.mean.cusum=function(data,mul.method="BinSeg",penalty="Asymptotic",pen.value=0.05,Q=5,class=TRUE,param.estimates=TRUE,minseglen){
  
  if(mul.method=="PELT"){ stop("CUSUM does not satisfy the assumptions of PELT, use SegNeigh or BinSeg instead.") }
  else if(!((mul.method=="BinSeg")||(mul.method=="SegNeigh"))){
    stop("Multiple Method is not recognised")
  }
  if(penalty!="MBIC"){
    costfunc = "mean.cusum"
  }else{
    stop("MBIC penalty is not valid for nonparametric test statistics.")
  }
  diffparam=1
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
    if(mul.method=="BinSeg"){
			out=binseg.mean.cusum(coredata(data),Q,pen.value)
    }
    else if(mul.method=="SegNeigh"){
      out=segneigh.mean.cusum(coredata(data),Q,pen.value)
    }
    if(class==TRUE){
      return(class_input(data, cpttype="mean", method=mul.method, test.stat="CUSUM", penalty=penalty, pen.value=pen.value, minseglen=minseglen, param.estimates=param.estimates, out=out, Q=Q))
    }
    else{ return(out)}
  }
  else{
    rep=nrow(data)
    out=list()
    if(class==TRUE){cpts=list()}
    if(mul.method=="BinSeg"){
      for(i in 1:rep){
				out=c(out,list(binseg.mean.cusum(data[i,],Q,pen.value)))
      }
      if(class==TRUE){cpts=out}
    }
    else if(mul.method=="SegNeigh"){
      for(i in 1:rep){
        out=c(out,list(segneigh.mean.cusum(data[i,],Q,pen.value)))
      }
    }
    if(class==TRUE){
      ans=list()
      for(i in 1:rep){
        ans[[i]]=class_input(data[i,], cpttype="mean", method=mul.method, test.stat="CUSUM", penalty=penalty, pen.value=pen.value, minseglen=minseglen, param.estimates=param.estimates, out=out, Q=Q)
      }
      return(ans)
    }
    else{return(out)}
  }
}
