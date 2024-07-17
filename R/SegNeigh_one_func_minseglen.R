dataToString <- function(data){
  return(paste0("c(", paste(data, collapse=", "), ")"))
}



SEGNEIGH <- function(data, pen.value, costfunc, Q, var=0, shape=1){

  #split the costfunction by . and then use the first two elements in eval(parse())
  if(Q<=0){stop(paste('Q is the maximum number of changepoints so should be greater than 0'))}

  split=strsplit(costfunc, "\\.")
  if(var!=0){
  text = c("segneigh",".", split[[1]][1],".", split[[1]][2],"(",dataToString(data),",",Q,",",pen.value,",","know.mean=",TRUE,",","mu=",var,")")
  }else{
    text = c("segneigh",".", split[[1]][1],".", split[[1]][2],"(",dataToString(data),",",Q,",",pen.value,")")
  }



  out = eval(parse(text=paste(text, collapse="")))

  return(out)
}


SEGNEIGH_C <- function(data, pen.value, costfunc, Q, var, shape) {
  # function that uses the PELT method to calculate changes in mean where the segments in the data are assumed to be Normal
  n = length(data)
  if(n<2){stop('Data must have atleast 2 observations to fit a changepoint model.')}

  reg = SEGNEIGH(data, pen.value, costfunc, Q, var, shape)

  error = 0
  op.cps = 0
  min_criterion = 0

  cps.Q=matrix(-.Machine$integer.max,ncol=Q,nrow=Q)
  like.Q=matrix(0,ncol=n,nrow=Q)

  cptsout=rep(0,n) # sets up null vector for changepoint answer

  answer=list()
  # answer[[6]]=1
  # on.exit(.C("FreePELT",answer[[6]]))

  storage.mode(data) = 'double'
  storage.mode(cptsout)='integer'
  storage.mode(cps.Q) = 'integer'
  storage.mode(like.Q) = 'double'

  answer=.C('segneigh',costfunc, data, as.integer(n), as.integer(Q), as.double(pen.value), cptsout, as.integer(error), cps.Q, op.cps, min_criterion, like.Q)

  if(answer[[7]]>0){
    stop("C code error:",answer[[7]],call.=F)
  }

  answer[[8]][answer[[8]] == -.Machine$integer.max] = NA

  ret = (list(cps=t(apply(answer[[8]],1,sort,na.last=TRUE)),cpts=sort(answer[[6]][answer[[6]]>0]),op.cpts=answer[[9]],pen=answer[[5]],like=answer[[10]],like.Q=-2*(answer[[11]])[,n]))

  elements = c("cps", "cpts", "op.cpts", "pen", "like", "like.Q")

  for (ele in elements) {
    if (isFALSE(all.equal(reg[ele], ret[ele]))) {
      print(ret[ele])
      print(reg[ele])
      stop("Non-matching segneigh ", ele, call. = F)
    }
  }

  return(reg)
}
