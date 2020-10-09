BINSEG = function(sumstat, pen = 0, cost_func = "mean.norm", shape = 1, minseglen = 2,  Q=5){

  n = length(sumstat[,1]) - 1
  m = length(sumstat[1,])
  tol = 0
  if(cost_func == "mean.norm" || cost_func == "var.norm" || cost_func == "meanvar.norm" || cost_func == "meanvar.exp" || cost_func == "meanvar.gamma" || cost_func == "meanvar.poisson"){
    MBIC = 0
  }else{
    MBIC = 1
  }
  if(n<2){stop('Data must have at least 2 observations to fit a changepoint model.')}
  if(Q>((n/minseglen)+1)){stop(paste('Q is larger than the maximum number of segments',(n/minseglen)+1))}
  if(Q>n){stop(paste("Q is larger than the length of the data length"))}
  if(Q<=0){stop(paste('Q is the maximum number of changepoints so should be greater than 0'))}
  shape = 1

  storage.mode(sumstat) = 'double'

  cptsout = rep(0,Q) # sets up null vector for changepoint answer
  likeout = rep(0,Q) # sets up null vector for likelihood of changepoints in cptsout
  storage.mode(cptsout)='integer'
  storage.mode(likeout)='double'
  op_cps = 0
  min = 0
  optimal = 0
  max = 0
  error = 0

  answer=.C('binseg', cost_func = cost_func, sumstat = sumstat, n = as.integer(n), m = as.integer(m), pen = as.double(pen), Q = as.integer(Q), cptsout = cptsout, error = as.integer(error), minorder = as.integer(min), optimalorder = as.integer(optimal), maxorder = as.integer(max), minseglen = as.integer(minseglen), likeout = likeout, op_cps = as.integer(op_cps), shape = as.double(shape), tol = as.double(tol), MBIC = as.integer(MBIC))
  if((answer$op_cps == Q)&(pen!=0)){warning('The number of changepoints identified is Q, it is advised to increase Q to make sure changepoints have not been missed.')}
  if(answer$op_cps == 0){cpts=n}
  else{cpts=c(sort(answer$cptsout[1:answer$op_cps]),n)}
  return(list(cps=rbind(answer$cptsout,2*answer$likeout),cpts=cpts,op.cpts=answer$op_cps,pen=pen))
  }
