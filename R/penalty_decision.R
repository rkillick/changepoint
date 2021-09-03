penalty_decision = function(penalty, pen.value, n, diffparam, asymcheck, method){
  
  
  if((penalty=="SIC0") || (penalty=="BIC0")){
    pen.return=diffparam*log(n)
  }
  else if((penalty=="SIC") || (penalty=="BIC")){
    pen.return=(diffparam+1)*log(n)
  }
  else if(penalty=="MBIC"){
    pen.return=(diffparam+2)*log(n)
  }
  else if((penalty=="SIC1") || (penalty=="BIC1")){
    stop("SIC1 and BIC1 have been depreciated, use SIC or BIC for the same result.")
  }
  else if(penalty=="AIC0"){
    pen.return=2*diffparam
  }
  else if(penalty=="AIC"){
    pen.return=2*(diffparam+1)
  }
  else if(penalty=="AIC1"){
    stop("AIC1 has been depreciated, use AIC for the same result.")
  }
  else if(penalty=="Hannan-Quinn0"){
    pen.return=2*diffparam*log(log(n))
  }
  else if(penalty=="Hannan-Quinn"){
    pen.return=2*(diffparam+1)*log(log(n))
  }
  else if(penalty=="Hannan-Quinn1"){
    stop("Hannan-Quinn1 has been depreciated, use Hannan-Quinn for the same result.")
  }
  else if(penalty=="None"){
    pen.return=0
  }
  else if((penalty!="Manual")&&(penalty!="Asymptotic")){
    stop('Unknown Penalty')
  }
  if((penalty=="Manual")&&(is.numeric(pen.value)==FALSE)){
    pen.value=try(eval(parse(text=paste(pen.value))),silent=TRUE)
    if(class(pen.value)=='try-error'){
      stop('Your manual penalty cannot be evaluated')
    }else{
      pen.return=pen.value
    }
  }
  
  if((penalty=="Manual")&&(is.numeric(pen.value)==TRUE)){
    pen.return=pen.value
  }
  
  
  if(penalty=="Asymptotic"){
    if((pen.value <= 0) || (pen.value > 1)){
      stop('Asymptotic penalty values must be > 0 and <= 1')
    }
    if(method != "AMOC"){
      warning('Asymptotic penalty value is not accurate for multiple changes, it should be treated the same as a manual penalty choice.')
    }
    if(asymcheck == "mean.norm"){
      alpha=pen.value
      alogn=(2*log(log(n)))^(-(1/2))
      blogn=(alogn^(-1))+(1/2)*alogn*log(log(log(n)))
      pen.return=(-alogn*log(log((1-alpha+exp(-2*(pi^(1/2))*exp(blogn/alogn)))^(-1/(2*(pi^(1/2))))))+blogn)^2
    }else if(asymcheck == "var.norm"){ 
      alpha=pen.value
      alogn=sqrt(2*log(log(n)))
      blogn=2*log(log(n))+ (log(log(log(n))))/2 - log(gamma(1/2))
      pen.return=(-(log(log((1-alpha+exp(-2*exp(blogn)))^(-1/2))))/alogn + blogn/alogn)^2
    }else if(asymcheck == "meanvar.norm"){
      alpha=pen.value
      alogn=sqrt(2*log(log(n)))
      blogn=2*log(log(n))+ log(log(log(n)))
      pen.return=(-(log(log((1-alpha+exp(-2*exp(blogn)))^(-1/2))))/alogn + blogn/alogn)^2
    }else if(asymcheck == "reg.norm"){
      alpha=pen.value
      top=-(log(log((1 - alpha + exp(-2*exp(2*(log(log(n)))+(diffparam/2)*(log(log(log(n))))- log(gamma(diffparam/2)))))^(-1/2))))  +  2*(log(log(n)))+(diffparam/2)*(log(log(log(n))))- log(gamma(diffparam/2))
      bottom=(2*log(log(n)))^(1/2)
      pen.return=(top/bottom)^2
    }else if(asymcheck == "var.css"){
      if(pen.value==0.01){pen.return=1.628}
      else if(pen.value==0.05){pen.return=1.358}
      else if(pen.value==0.1){pen.return=1.224}
      else if(pen.value==0.25){pen.return=1.019}
      else if(pen.value==0.5){pen.return=0.828}
      else if(pen.value==0.75){pen.return=0.677}
      else if(pen.value==0.9){pen.return=0.571}
      else if(pen.value==0.95){pen.return=0.520}
      else{stop('Only alpha values of 0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95 are valid for CSS')}
    }else if(asymcheck == "mean.cusum"){
      stop('Asymptotic penalties have not been implemented yet for CUSUM')
    }else if(asymcheck == "meanvar.gamma"){
      stop('Asymptotic penalties for the Gamma test statistic are not defined, please choose an alternative penalty type')
    }else if(asymcheck == "meanvar.exp"){
      alpha=pen.value
      an=(2*log(log(n)))^(1/2)
      bn=2*log(log(n))+(1/2)*log(log(log(n)))-(1/2)*log(pi)
      pen.return=(-1/an)*log(-0.5*log(1-alpha))+bn
      if(alpha==1){pen.return=1.42417} # value of 1 gives log(0), this is alpha=0.99999999999999993
    }else if(asymcheck == "meanvar.poisson"){
      stop('Asymptotic penalties for the Poisson test statistic are not available yet, please choose an alternative penalty type')
    }
  }
#if(method=="AMOC"){
#  pen.return=pen.value
#}
  if(pen.return < 0){
    stop('pen.value cannot be negative, please change your penalty value')
  }else{
    return(pen.return)
  }
}


