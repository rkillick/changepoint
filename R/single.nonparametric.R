single.var.css.calc <-
  function(data,extrainf=TRUE, minseglen){
    singledim=function(data,extrainf=TRUE,minseglen){
      n=length(data)
      y2=c(0,cumsum(data^2))
      taustar=minseglen:(n-minseglen+1)
      tmp=(y2[taustar+1]/y2[n+1])-taustar/n
      
      D=max(abs(tmp),na.rm=T)
      tau=which.max(abs(tmp))
      if(extrainf==TRUE){
        out=c(tau,sqrt(n/2)*D)
        names(out)=c('cpt','test statistic')
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
        cpt=matrix(0,ncol=2,nrow=rep)
        for(i in 1:rep){
          cpt[i,]=singledim(data[i,],extrainf,minseglen)
        }
        colnames(cpt)=c('cpt','test statistic')
      }
      return(cpt)
    }
  }


single.var.css<-function(data,penalty="MBIC",pen.value=0,class=TRUE,param.estimates=TRUE,minseglen){
  if(length(pen.value)>1){stop('Only one dimensional penalties can be used for CSS')}
  if(penalty=="MBIC"){stop("MBIC penalty is not valid for nonparametric test statistics.")}
  diffparam=1
  if(is.null(dim(data))==TRUE){
    # single dataset
    n=length(data)
  }
  else{
    n=ncol(data)
  }
  if(n<4){stop('Data must have atleast 4 observations to fit a changepoint model.')}
  
  pen.value = penalty_decision(penalty, pen.value, n, diffparam, asymcheck = "var.css", method="AMOC")
  if(is.null(dim(data))==TRUE){
    tmp=single.var.css.calc(coredata(data),extrainf=TRUE,minseglen)
    ans=decision(tau=tmp[1],null=tmp[2],penalty="Manual",n=n,diffparam=1,pen.value=pen.value)
    if(class==TRUE){
      out=new("cpt")
      data.set(out)=data; cpttype(out)="variance"; method(out)="AMOC"; test.stat(out)="CSS"; pen.type(out)=penalty; pen.value(out)=ans$pen;ncpts.max(out)=1
      if(ans$cpt != n){cpts(out)=c(ans$cpt,n)}
      else{cpts(out)=ans$cpt}
      if(param.estimates==TRUE){
        out=param(out)
      }
      return(out)
    }
    else{ return(ans$cpt)}
  }
  else{ 
    tmp=single.var.css.calc(data,extrainf=TRUE,minseglen)
    ans=decision(tau=tmp[,1],null=tmp[,2],penalty="Manual",n=n,diffparam=1,pen.value=pen.value)
    if(class==TRUE){
      rep=nrow(data)
      out=list()
      for(i in 1:rep){
        out[[i]]=new("cpt")
        data.set(out[[i]])=ts(data[i,]); cpttype(out[[i]])="variance"; method(out[[i]])="AMOC"; test.stat(out[[i]])="CSS"; pen.type(out[[i]])=penalty;pen.value(out[[i]])=ans$pen;ncpts.max(out[[i]])=1
        if(ans$cpt[i] != n){cpts(out[[i]])=c(ans$cpt[i],n)}
        else{cpts(out[[i]])=ans$cpt[i]}
        if(param.estimates==TRUE){
          out[[i]]=param(out[[i]])
        }
      }
      return(out)
    }
    else{ return(ans$cpt)}
  }
}










single.mean.cusum.calc <-
  function(data,extrainf=TRUE,minseglen){
    singledim=function(data,extrainf=TRUE,minseglen){
      n=length(data)
      ybar=mean(data)
      y=c(0,cumsum(data-ybar))
      y=y/n
      
      M=max(abs(y[minseglen:(n-minseglen+1)]),na.rm=T)
      tau=which.max(abs(y[minseglen:(n-minseglen+1)]))+minseglen-1
      if(extrainf==TRUE){
        out=c(tau,M)
        names(out)=c('cpt','test statistic')
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
        cpt=matrix(0,ncol=2,nrow=rep)
        for(i in 1:rep){
          cpt[i,]=singledim(data[i,],extrainf,minseglen)
        }
        colnames(cpt)=c('cpt','test statistic')
      }
      return(cpt)
    }
  }


single.mean.cusum<-function(data,penalty="Asymptotic",pen.value=0.05,class=TRUE,param.estimates=TRUE,minseglen){
  if(length(pen.value)>1){stop('Only one dimensional penalties can be used for CUSUM')}
  if(penalty=="MBIC"){stop("MBIC penalty is not valid for nonparametric test statistics.")}

  if(is.null(dim(data))==TRUE){
    # single dataset
    n=length(data)
  }
  else{
    n=ncol(data)
  }
  if(n<2){stop('Data must have atleast 2 observations to fit a changepoint model.')}
  
  pen.value = penalty_decision(penalty, pen.value, n, diffparam=1, asymcheck="mean.cusum", method="AMOC")
  if(is.null(dim(data))==TRUE){
    tmp=single.mean.cusum.calc(coredata(data),extrainf=TRUE,minseglen)
    ans=decision(tau=tmp[1],null=tmp[2],penalty=penalty,n=n,diffparam=1,pen.value=pen.value)
    if(class==TRUE){
      out=new("cpt")
      data.set(out)=data; cpttype(out)="mean"; method(out)="AMOC"; test.stat(out)="CUSUM"; pen.type(out)=penalty; pen.value(out)=ans$pen;ncpts.max(out)=1
      if(ans$cpt != n){cpts(out)=c(ans$cpt,n)}
      else{cpts(out)=ans$cpt}
      if(param.estimates==TRUE){
        out=param(out)
      }
      return(out)
    }
    else{ return(ans$cpt)}
  }
  else{ 
    tmp=single.mean.cusum.calc(data,extrainf=TRUE,minseglen)
    ans=decision(tau=tmp[,1],null=tmp[,2],penalty=penalty,n=n,diffparam=1,pen.value=pen.value)
    if(class==TRUE){
      rep=nrow(data)
      out=list()
      for(i in 1:rep){
        out[[i]]=new("cpt")
        data.set(out[[i]])=ts(data[i,]); cpttype(out[[i]])="mean"; method(out[[i]])="AMOC"; test.stat(out[[i]])="CUSUM"; pen.type(out[[i]])=penalty;pen.value(out[[i]])=ans$pen;ncpts.max(out[[i]])=1
        if(ans$cpt[i] != n){cpts(out[[i]])=c(ans$cpt[i],n)}
        else{cpts(out[[i]])=ans$cpt[i]}
        if(param.estimates==TRUE){
          out[[i]]=param(out[[i]])
        }
      }
      return(out)
    }
    else{ return(ans$cpt)}
  }
}

