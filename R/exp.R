single.meanvar.exp.calc <-
  function(data, extrainf = TRUE, minseglen) {
    singledim = function(data, extrainf = TRUE, minseglen) {
      n = length(data)
      y = c(0, cumsum(data))
      null = 2 * n * log(y[n + 1]) - 2 * n * log(n)
      taustar = minseglen:(n - minseglen)
      tmp = 2 * taustar * log(y[taustar + 1]) - 2 * taustar * log(taustar) + 2 *
        (n - taustar) * log((y[n + 1] - y[taustar + 1])) - 2 * (n - taustar) * log(n -
                                                                                     taustar)
      
      tau = which(tmp == min(tmp, na.rm = T))[1]
      taulike = tmp[tau]
      tau = tau + minseglen - 1 # correcting for the fact that we are starting at minseglen
      if (extrainf == TRUE) {
        out = c(tau, null, taulike)
        names(out) = c('cpt', 'null', 'alt')
        return(out)
      }
      else{
        return(tau)
      }
    }
    
    if (is.null(dim(data)) == TRUE) {
      # single data set
      cpt = singledim(data, extrainf, minseglen)
      return(cpt)
    }
    else{
      rep = nrow(data)
      n = ncol(data)
      cpt = NULL
      if (extrainf == FALSE) {
        for (i in 1:rep) {
          cpt[i] = singledim(data[i, ], extrainf, minseglen)
        }
      }
      else{
        cpt = matrix(0, ncol = 3, nrow = rep)
        for (i in 1:rep) {
          cpt[i, ] = singledim(data[i, ], extrainf, minseglen)
        }
        colnames(cpt) = c('cpt', 'null', 'alt')
      }
      return(cpt)
    }
  }


single.meanvar.exp <-
  function(data,
           penalty = "MBIC",
           pen.value = 0,
           class = TRUE,
           param.estimates = TRUE,
           minseglen) {
    if (sum(data < 0) > 0) {
      stop('Exponential test statistic requires positive data')
    }
    if (is.null(dim(data)) == TRUE) {
      # single dataset
      n = length(data)
    }
    else{
      n = ncol(data)
    }
    if (n < 4) {
      stop('Data must have atleast 4 observations to fit a changepoint model.')
    }
    if (n < (2 * minseglen)) {
      stop('Minimum segment legnth is too large to include a change in this data')
    }
    
    penalty_decision(
      penalty,
      pen.value,
      n,
      diffparam = 1,
      asymcheck = "meanvar.exp",
      method = "AMOC"
    )
    if (is.null(dim(data)) == TRUE) {
      tmp = single.meanvar.exp.calc(coredata(data), extrainf = TRUE, minseglen)
      if (penalty == "MBIC") {
        tmp[3] = tmp[3] + log(tmp[1]) + log(n - tmp[1] + 1)
      }
      ans = decision(tmp[1], tmp[2], tmp[3], penalty, n, diffparam = 1, pen.value)
      if (class == TRUE) {
        return(
          class_input(
            data,
            cpttype = "mean and variance",
            method = "AMOC",
            test.stat = "Exponential",
            penalty = penalty,
            pen.value = ans$pen,
            minseglen = minseglen,
            param.estimates = param.estimates,
            out = c(0, ans$cpt)
          )
        )
      }
      else{
        an = (2 * log(log(n))) ^ (1 / 2)
        bn = 2 * log(log(n)) + (1 / 2) * log(log(log(n))) - (1 / 2) * log(pi)
        out = c(ans$cpt, exp(-2 * exp(-an * sqrt(
          abs(tmp[2] - tmp[3])
        ) + an * bn)) - exp(-2 * exp(an * bn)))  # Chen & Gupta (2000) pg149
        names(out) = c('cpt', 'p value')
        return(out)
      }
    }
    else{
      tmp = single.meanvar.exp.calc(data, extrainf = TRUE, minseglen)
      if (penalty == "MBIC") {
        tmp[, 3] = tmp[, 3] + log(tmp[, 1]) + log(n - tmp[, 1] + 1)
      }
      ans = decision(tmp[, 1], tmp[, 2], tmp[, 3], penalty, n, diffparam =
                       1, pen.value)
      if (class == TRUE) {
        rep = nrow(data)
        out = list()
        for (i in 1:rep) {
          out[[i]] = class_input(
            data[i, ],
            cpttype = "mean and variance",
            method = "AMOC",
            test.stat = "Exponential",
            penalty = penalty,
            pen.value = ans$pen,
            minseglen = minseglen,
            param.estimates = param.estimates,
            out = c(0, ans$cpt[i])
          )
        }
        return(out)
      }
      else{
        an = (2 * log(log(n))) ^ (1 / 2)
        bn = 2 * log(log(n)) + (1 / 2) * log(log(log(n))) - (1 / 2) * log(pi)
        out = cbind(ans$cpt, exp(-2 * exp(-an * sqrt(
          abs(tmp[, 2] - tmp[, 3])
        ) + bn)) - exp(-2 * exp(bn)))  # Chen & Gupta (2000) pg149
        colnames(out) = c('cpt', 'p value')
        rownames(out) = NULL
        return(out)
      }
    }
  }


# PELT.meanvar.exp=function(data,pen=0,nprune=FALSE){
#   mll.meanvar.EFK=function(x,n){
#     return( 2*n*log(x)-2*n*log(n))
#   }
#   n=length(data)
#   y=c(0,cumsum(data))
#
#   lastchangecpts=matrix(NA,nrow=n,ncol=2)
#   lastchangelike=matrix(NA,nrow=n,ncol=2)
#   checklist=NULL
#   lastchangelike[1,]=c(mll.meanvar.EFK(y[2],1),mll.meanvar.EFK(y[n+1]-y[2],n-1)+pen)
#   lastchangecpts[1,]=c(0,1)
#   lastchangelike[2,]=c(mll.meanvar.EFK(y[3],2),mll.meanvar.EFK(y[n+1]-y[3],n-2)+pen)
#   lastchangecpts[2,]=c(0,2)
#   lastchangelike[3,]=c(mll.meanvar.EFK(y[4],3),mll.meanvar.EFK(y[n+1]-y[4],n-3)+pen)
#   lastchangecpts[3,]=c(0,3)
#   noprune=NULL
#   for(tstar in 4:n){
#     tmplike=NULL
#     tmpt=c(checklist, tstar-2)
#     tmplike=lastchangelike[tmpt,1]+mll.meanvar.EFK(y[tstar+1]-y[tmpt+1],tstar-tmpt)+pen
#     if(tstar==n){
#       lastchangelike[tstar,]=c(min(c(tmplike,mll.meanvar.EFK(y[tstar+1]-y[1],tstar)),na.rm=TRUE),0)
#     }
#     else{
#       lastchangelike[tstar,]=c(min(c(tmplike,mll.meanvar.EFK(y[tstar+1]-y[1],tstar)),na.rm=TRUE),mll.meanvar.EFK(y[n+1]-y[tstar+1],n-tstar)+pen)
#     }
#     if(lastchangelike[tstar,1]==mll.meanvar.EFK(y[tstar+1]-y[1],tstar)){
#       lastchangecpts[tstar,]=c(0,tstar)
#     }
#     else{
#       cpt=tmpt[tmplike==lastchangelike[tstar,1]][1]
#       lastchangecpts[tstar,]=c(cpt,tstar)
#     }
#     checklist=tmpt[tmplike<=lastchangelike[tstar,1]+pen]
#     if(nprune==TRUE){
#       noprune=c(noprune,length(checklist))
#     }
#   }
#   if(nprune==TRUE){
#     return(nprune=noprune)
#   }
#   else{
#     fcpt=NULL
#     last=n
#     while(last!=0){
# 	fcpt=c(fcpt,lastchangecpts[last,2])
# 	last=lastchangecpts[last,1]
#     }
#     return(cpt=sort(fcpt))
#   }
# }

# binseg.meanvar.exp=function(data,Q=5,pen=0){
#   mll.meanvar=function(x,n){
#     return(n*log(n)-n*log(x))
#   }
#   n=length(data)
#   y=c(0,cumsum(data))
#   tau=c(0,n)
#   cpt=matrix(0,nrow=2,ncol=Q)
#   oldmax=1000
#
#   for(q in 1:Q){
#     lambda=rep(0,n-1)
#     i=1
#     st=tau[1]+1;end=tau[2]
#     null=mll.meanvar(y[end+1]-y[st],end-st+1)
#     for(j in 1:(n-1)){
#       if(j==end){
#         st=end+1;i=i+1;end=tau[i+1]
#         null=mll.meanvar(y[end+1]-y[st],end-st+1)
#       }else{
# 	if((j-st)<2){lambda[j]=-1*10^(100)}
# 	else if((end-j)<2){lambda[j]=-1*10^(100)}
# 	else{lambda[j]=mll.meanvar(y[j+1]-y[st],j-st+1)+mll.meanvar(y[end+1]-y[j+1],end-j)-null}
#       }
#     }
#     k=which.max(lambda)[1]
#     cpt[1,q]=k;cpt[2,q]=min(oldmax,max(lambda))
#     oldmax=min(oldmax,max(lambda))
#     tau=sort(c(tau,k))
#   }
#   op.cps=NULL
#   p=1:(Q-1)
#   for(i in 1:length(pen)){
#     criterion=(2*cpt[2,])>=pen[i]
#     if(sum(criterion)==0){
#       op.cps=0
#     }
#     else{
#       op.cps=c(op.cps,max(which((criterion)==TRUE)))
#     }
#   }
#   return(list(cps=cpt,op.cpts=op.cps,pen=pen))
# }

multiple.meanvar.exp = function(data,
                                mul.method = "PELT",
                                penalty = "MBIC",
                                pen.value = 0,
                                Q = 5,
                                class = TRUE,
                                param.estimates = TRUE,
                                minseglen) {
  if (sum(data < 0) > 0) {
    stop('Exponential test statistic requires positive data')
  }
  if (!((mul.method == "PELT") || (mul.method == "BinSeg"))) {
    stop("Multiple Method is not recognised, must be PELT or BinSeg.")
  }
  costfunc = "meanvar.exp"
  if (penalty == "MBIC") {
    costfunc = "meanvar.exp.mbic"
  }
  
  diffparam = 1
  if (is.null(dim(data)) == TRUE) {
    # single dataset
    n = length(data)
  }
  else{
    n = ncol(data)
  }
  if (n < (2 * minseglen)) {
    stop('Minimum segment legnth is too large to include a change in this data')
  }
  
  pen.value = penalty_decision(
    penalty,
    pen.value,
    n,
    diffparam = 1,
    asymcheck = costfunc,
    method = mul.method
  )
  
  if (is.null(dim(data)) == TRUE) {
    # single dataset
    out = data_input(
      data = data,
      method = mul.method,
      pen.value = pen.value,
      costfunc = costfunc,
      minseglen = minseglen,
      Q = Q
    )
    
    if (class == TRUE) {
      return(
        class_input(
          data,
          cpttype = "mean and variance",
          method = mul.method,
          test.stat = "Exponential",
          penalty = penalty,
          pen.value = pen.value,
          minseglen = minseglen,
          param.estimates = param.estimates,
          out = out,
          Q = Q
        )
      )
    }
    else{
      return(out[[2]])
    }
  }
  else{
    rep = nrow(data)
    out = list()
    for (i in 1:rep) {
      out[[i]] = data_input(
        data[i, ],
        method = mul.method,
        pen.value = pen.value,
        costfunc = costfunc,
        minseglen = minseglen,
        Q = Q
      )
    }
    
    cpts = lapply(out, '[[', 2)
    
    if (class == TRUE) {
      ans = list()
      for (i in 1:rep) {
        ans[[i]] = class_input(
          data[i, ],
          cpttype = "mean and variance",
          method = mul.method,
          test.stat = "Exponential",
          penalty = penalty,
          pen.value = pen.value,
          minseglen = minseglen,
          param.estimates = param.estimates,
          out = out[[i]],
          Q = Q
        )
      }
      return(ans)
    }
    else{
      return(cpts)
    }
  }
}
