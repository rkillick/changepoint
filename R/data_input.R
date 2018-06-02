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
  else {
    stop('Unknown method, should be either PELT or BinSeg.')
  }
  return(out)
  
}