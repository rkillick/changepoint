# Fitting functions for each model form
fit.mean=function(object,cpts=NULL){
  if(is.null(cpts)){cpts=c(0,object@cpts)}
  data=data.set(object)
  tmpmean=NULL
  for(j in 1:(length(cpts)-1)){ # quicker to use this than nseg for cpt.range, -1 as cpts includes 0 and n
    tmpmean[j]=mean(data[(cpts[j]+1):(cpts[j+1])])
  }
  return(tmpmean)
}
fit.var=function(object,cpts=NULL){
  if(is.null(cpts)){cpts=c(0,object@cpts)}
  data=data.set(object)
  seglen=diff(cpts) # not using seg.len as needs ncpts for cpt.range, might aswell use diff(cpts) as less flops
  tmpvar=NULL
  for(j in 1:length(seglen)){
    tmpvar[j]=var(data[(cpts[j]+1):(cpts[j+1])])
  }
  tmpvar=tmpvar*(seglen-1)/seglen # correctly for the fact that the MLE estimate is /n but the var function is /n-1
  return(tmpvar)
}
fit.scale=function(object,shape,cpts=NULL){
  if(is.null(cpts)){cpts=c(0,object@cpts)}
  data=data.set(object)
  y=c(0,cumsum(data))
  tmpscale=NULL
  for(j in 1:(length(cpts)-1)){ # quicker than nseg for cpt.range
    tmpscale[j]=(y[(cpts[j+1]+1)]-y[(cpts[j]+1)])/((cpts[j+1]-cpts[j])*shape)
  }
  return(tmpscale)
}
fit.trend=function(object,cpts=NULL){
  if(is.null(cpts)){cpts=c(0,object@cpts)}
  seglen=diff(cpts) # not using seg.len as needs ncpts for cpt.range, might aswell use diff(cpts) as less flops
  data=data.set(object)
  n=length(data)
  sumstat=cbind(cumsum(c(0,data)),cumsum(c(0,data*c(1:n))))
  cptsumstat=matrix(sumstat[object@cpts+1,]-sumstat[c(0,cpts(object))+1,],ncol=2)
  cptsumstat[,2]=cptsumstat[,2]-cptsumstat[,1]*c(0,cpts(object)) # i.e. creating newx3

  thetaS=(2*cptsumstat[,1]*(2*seglen + 1) - 6*cptsumstat[,2]) / (2*seglen*(2*seglen + 1) - 3*seglen*(seglen+1))
  thetaT=(6*cptsumstat[,2])/((seglen+1)*(2*seglen+1)) + (thetaS * (1-((3*seglen)/((2*seglen)+1))))
  return(cbind(thetaS,thetaT))
}
fit.meanar=function(object,cpts=NULL){
  if(is.null(cpts)){cpts=c(0,object@cpts)}
  seglen=diff(cpts) # not using seg.len as needs ncpts for cpt.range, might aswell use diff(cpts) as less flops
  data=data.set(object)
  n=length(data)-1
  sumstat=cbind(cumsum(c(0,data[-1])),cumsum(c(0,data[-(n+1)])),cumsum(c(0,data[-1]*data[-(n+1)])),cumsum(c(0,data[-1]^2)),cumsum(c(0,data[-(n+1)]^2)))
  cptsumstat=matrix(sumstat[cpts[-1],]-sumstat[cpts[-length(cpts)]+1,],ncol=5)
  beta2=(2*seglen*cptsumstat[,3]-cptsumstat[,1]*cptsumstat[,2])/(2*seglen*cptsumstat[,5]*(1-cptsumstat[,2]^2));
  beta1=(2*cptsumstat[,1]-beta2*cptsumstat[,2])/(2*seglen);

  return(cbind(beta1,beta2))
}
fit.trendar=function(object,cpts=NULL){
  if(is.null(cpts)){cpts=c(0,object@cpts)}
  seglen=diff(cpts) # not using seg.len as needs ncpts for cpt.range, might aswell use diff(cpts) as less flops
  data=data.set(object)
  n=length(data)-1
  sumstat=cbind(cumsum(c(0,data[-1])),cumsum(c(0,data[-(n+1)])),cumsum(c(0,data[-1]*data[-(n+1)])),cumsum(c(0,data[-1]*c(1:n))),cumsum(c(0,data[-(n+1)]*c(0:(n-1)))),cumsum(c(0,data[-1]^2)),cumsum(c(0,data[-(n+1)]^2)))
  cptsumstat=matrix(sumstat[cpts[-1]+1,]-sumstat[cpts[-length(cpts)]+1,],ncol=7)
  cptsumstat[,4]=cptsumstat[,4]-cptsumstat[,1]*cpts[-length(cpts)] # i.e. creating newx4
  cptsumstat[,5]=cptsumstat[,5]-cptsumstat[,2]*cpts[-length(cpts)] # i.e. creating newx5
  betatop=seglen*(seglen-1)*(seglen*(seglen-1)*cptsumstat[,3] + 2*(2*seglen+1)*cptsumstat[,1]*(cptsumstat[,5]-seglen*cptsumstat[,2]) + 6*cptsumstat[,4]*(cptsumstat[,2]-cptsumstat[,5]))
  betabottom=seglen*(seglen-1)*cptsumstat[,7] + 2*(2*seglen+1)*cptsumstat[,2]*(seglen*cptsumstat[,2]-cptsumstat[,5]) + 6*cptsumstat[,5]*(cptsumstat[,5]-cptsumstat[,2]);
  beta=betatop/betabottom;
  thetajpo=(6*(seglen+2)*(cptsumstat[,4]-beta*cptsumstat[,5]))/((seglen+1)*(2*seglen+1)) - 2*(cptsumstat[,1]-beta*cptsumstat[,2])
  thetaj=(2*(2*seglen+1)*(cptsumstat[,1]-beta*cptsumstat[,2])-6*(cptsumstat[,4]-beta*cptsumstat[,5]))/(seglen-1)

  return(cbind(beta,thetajpo,thetaj))
}
fit.reg=function(object,cpts=NULL){
  if(is.null(cpts)){cpts=c(0,object@cpts)}
  data=data.set(object)
  p=ncol(data)-1
  tmpbeta=matrix(NA,ncol=p,nrow=nseg(object))
  tmpsigma=rep(NA,nseg(object))
  for(j in 1:(length(cpts)-1)){
    formula=paste('-1+data[',cpts[j]+1,':',cpts[j+1],',2]',sep='')
    if(p>1){
      for(i in 2:p){
        formula=paste(formula,'+data[',(cpts[j]+1),':',cpts[j+1],',',i+1,']',sep='')
      }
    }
    tmpfit=eval(parse(text=paste('lm(data[',(cpts[j]+1),':',cpts[j+1],',1]~',formula,')',sep='')))
    tmpbeta[j,]=tmpfit$coefficients
    tmpsigma[j]=sum(tmpfit$residuals^2)/(length(tmpfit$residuals)-length(tmpfit$coefficients)) ##var(tmpfit$residuals)
  }
  return(list(beta=tmpbeta,sig2=tmpsigma))
}

