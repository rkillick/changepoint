data_input <- function(data, method, pen.value, costfunc, minseglen, Q, var=0, shape=1){
  if(var !=0){
    mu<-var
  }else{
  mu <- mean(data)
  }
  sumstat=cbind(c(0,cumsum(coredata(data))),c(0,cumsum(coredata(data)^2)),cumsum(c(0,(coredata(data)-mu)^2)))
  if(method=="PELT"){
    #out=PELT.meanvar.norm(coredata(data),pen.value)
    out=PELT(sumstat,pen=pen.value,cost_func = costfunc,minseglen=minseglen, shape=shape)  ## K NEW ##
    #cpts=out[[2]]
  }
  else if(method=="BinSeg"){
    out=BINSEG(sumstat,pen=pen.value,cost_func = costfunc,minseglen=minseglen,Q=Q, shape=shape)  ## K NEW ##
    #cpts=out[[2]]
    #   		out=binseg.meanvar.norm(coredata(data),Q,pen.value)
    # 			if(out$op.cpts==0){cpts=n}
    # 			else{cpts=c(sort(out$cps[1,1:out$op.cpts]),n)}
    # the above is now inside the BINSEG function
  }
  else if(method=="SegNeigh"){
    #out=segneigh.meanvar.norm(coredata(data),Q,pen.value)
    out=SEGNEIGH(data=data, pen.value=pen.value, Q=Q, costfunc=costfunc, var=var, shape=shape)
#     if(out$op.cpts==0){cpts=n}
#     else{cpts=c(sort(out$cps[out$op.cpts+1,][out$cps[out$op.cpts+1,]>0]),n)}
  }
  return(out)
  
}