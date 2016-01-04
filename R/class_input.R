class_input <- function(data, cpttype, method, test.stat, penalty, pen.value, minseglen, param.estimates, out=list(), Q=NA, shape=NA){
  if(method=="BinSeg" || method=="SegNeigh" || penalty=="CROPS"){
    ans=new("cpt.range")
  }else{
    ans=new("cpt")
  }
  
  data.set(ans)=data;cpttype(ans)=cpttype;method(ans)=method; test.stat(ans)=test.stat;pen.type(ans)=penalty;pen.value(ans)=pen.value;minseglen(ans)=minseglen;
  if(penalty!="CROPS"){ # crops is only one that doesn't give a single set of cpts
    cpts(ans)=out[[2]]
    
    if(param.estimates==TRUE){
      if(test.stat == "Gamma"){
      ans=param(ans, shape)
      }else{
      ans=param(ans)
      }
    }
  }
  
  if(method=="PELT"){
      ncpts.max(ans)=Inf
  }
  else if(method=="AMOC"){
    ncpts.max(ans)=1
  }
  else{
    ncpts.max(ans)=Q
  }
  
  if(method=="BinSeg"){
    l=list()
    for(i in 1:(length(out$cps)/2)){
      l[[i]] = out$cps[1,1:i] 
    }
    m = t(sapply(l, '[', 1:max(sapply(l, length))))
    
    cpts.full(ans)=m
    pen.value.full(ans)=out$cps[2,]
  }else if(method=="SegNeigh"){
    cpts.full(ans)=out$cps[-1,]
    pen.value.full(ans)=-diff(out$like.Q)
  }else if(penalty=="CROPS"){
    m = t(sapply(out[[2]], '[', 1:max(sapply(out[[2]], length))))
    
    cpts.full(ans) = m
    pen.value.full(ans) = out[[1]][1,]
    if(test.stat=="Gamma"){param.est(ans)$shape=shape}
  }
  
  return(ans)
}