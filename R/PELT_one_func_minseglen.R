PELT = function(sumstat,pen=0, cost_func = "norm.mean", shape = 1, minseglen = 1){
  # function that uses the PELT method to calculate changes in mean where the segments in the data are assumed to be Normal
  n = length(sumstat)/3 - 1
  if(n<2){stop('Data must have atleast 2 observations to fit a changepoint model.')}
  
  storage.mode(sumstat) = 'double'
  error=0
  
  lastchangelike = array(0,dim = n+1)
  lastchangecpts = array(0,dim = n+1)
  numchangecpts = array(0,dim = n+1)
  
  cptsout=rep(0,n) # sets up null vector for changepoint answer
  storage.mode(cptsout)='integer'
  
  answer=list()
  answer[[6]]=1
  on.exit(.C("FreePELT",answer[[6]]))
  
  storage.mode(lastchangelike) = 'double'
  storage.mode(lastchangecpts) = 'integer'
  storage.mode(numchangecpts) = 'integer'
  
  # answer=.C('PELT',cost_func, y3, y2,y,as.integer(n),as.double(pen),cptsout,as.integer(error),as.double(shape))
  answer=.C('PELT',cost_func, sumstat,as.integer(n),as.double(pen),cptsout,as.integer(error),as.double(shape), as.integer(minseglen), lastchangelike, lastchangecpts,numchangecpts)

  if(answer[[6]]>0){
    stop("C code error:",answer[[6]],call.=F)
  }
  return(list(answer[[10]],sort(answer[[5]][answer[[5]]>0]), answer[[9]], answer[[11]]))
  
}
