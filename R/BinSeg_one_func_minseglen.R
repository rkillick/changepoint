BINSEG = function(sumstat, pen = 0, cost_func = "norm.mean", shape = 1, minseglen = 2,  Q=5){

  n = length(sumstat[,1]) - 1
  if(n<2){stop('Data must have at least 2 observations to fit a changepoint model.')}
  if(Q>((n/minseglen)+1)){stop(paste('Q is larger than the maximum number of segments',(n/minseglen)+1))}
  if(Q>n){stop(paste("Q is larger than the length of the data length"))}
  if(Q<=0){stop(paste('Q is the maximum number of changepoints so should be greater than 0'))}
  
  storage.mode(sumstat) = 'double'
  
  cptsout=rep(0,Q) # sets up null vector for changepoint answer
  likeout=rep(0,Q) # sets up null vector for likelihood of changepoints in cptsout
  storage.mode(cptsout)='integer'
  storage.mode(likeout)='double'
  op_cps = 0

  answer=.C('binseg', cost_func = cost_func, sumstat = sumstat, n = as.integer(n), pen = as.double(pen), Q = as.integer(Q), cptsout = cptsout, minseglen = as.integer(minseglen), likeout = likeout, op_cps = as.integer(op_cps), shape = as.double(shape))
  if((answer$op_cps == Q)&(pen!=0)){warning('The number of changepoints identified is Q, it is advised to increase Q to make sure changepoints have not been missed.')}
  if(answer$op_cps == 0){cpts=n}
  else{cpts=c(sort(answer$cptsout[1:answer$op_cps]),n)}
  return(list(cps=rbind(answer$cptsout,2*answer$likeout),cpts=cpts,op.cpts=answer$op_cps,pen=pen))
  }





