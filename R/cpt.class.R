	setClass("cpt",slots=list(data.set="ts", cpttype="character", method="character", 	test.stat="character",pen.type="character",pen.value="numeric",minseglen="numeric",cpts="numeric",ncpts.max="numeric",param.est="list",date="character",version="character"),prototype=prototype(date=date(),version=as(packageVersion("changepoint"),'character')))

	setClass("cpt.reg",slots=list(data.set="matrix", cpttype="character", method="character", test.stat="character",pen.type="character",pen.value="numeric",minseglen="numeric",cpts="numeric",ncpts.max="numeric",param.est="list",date="character",version="character"),prototype=prototype(cpttype="regression",date=date(),version=as(packageVersion("changepoint"),"character")))
  
#   setClass("cpt", representation(), prototype())
# # cpts is the optimal segementation
#   
  setClass("cpt.range",slots=list(cpts.full="matrix", pen.value.full="numeric"), prototype=prototype(), contains="cpt")
  # cpts.full is the entire matrix
  # pen.value.full (beta) values as an extra slot (vector)

# retrival functions for slots
	if(!isGeneric("data.set")) {
		if (is.function("data.set")){
			fun <- data.set
		}
	  else {fun <- function(object){
				standardGeneric("data.set")
			}
		}
		setGeneric("data.set", fun)
	}
	setMethod("data.set","cpt",function(object) coredata(object@data.set))
	setMethod("data.set","cpt.reg",function(object) coredata(object@data.set))

	if(!isGeneric("data.set.ts")) {
	  if (is.function("data.set.ts")){
	    fun <- data.set.ts
	  }
	  else {fun <- function(object){
	    standardGeneric("data.set.ts")
	  }
	  }
	  setGeneric("data.set.ts", fun)
	}
	setMethod("data.set.ts","cpt",function(object) object@data.set)

  if(!isGeneric("cpttype")) {
		if (is.function("cpttype")){
			fun <- cpttype
		}
		else {fun <- function(object){
				standardGeneric("cpttype")
			}
		}
		setGeneric("cpttype", fun)
	}
	setMethod("cpttype","cpt",function(object) object@cpttype)
	setMethod("cpttype","cpt.reg",function(object) object@cpttype)

	if(!isGeneric("method")) {
		if (is.function("method")){
			fun <- method
		}
		else {fun <- function(object){
				standardGeneric("method")
			}
		}
		setGeneric("method", fun)
	}
	setMethod("method","cpt",function(object) object@method)
	setMethod("method","cpt.reg",function(object) object@method)
	
	# distribution remains for backwards compatability, changed to test.stat version 1.0
	if(!isGeneric("distribution")) {
	  if (is.function("distribution")){
	    fun <- distribution
	  }
	  else {fun <- function(object){
	    standardGeneric("distribution")
	  }
	  }
	  setGeneric("distribution", fun)
	}
	setMethod("distribution","cpt",function(object) object@test.stat)
	setMethod("distribution","cpt.reg",function(object) object@test.stat)
	
  if(!isGeneric("test.stat")) {
		if (is.function("test.stat")){
			fun <- test.stat
		}
		else {fun <- function(object){
				standardGeneric("test.stat")
			}
		}
		setGeneric("test.stat", fun)
	}
	setMethod("test.stat","cpt",function(object) object@test.stat)
	setMethod("test.stat","cpt.reg",function(object) object@test.stat)
	
	if(!isGeneric("pen.type")) {
		if (is.function("pen.type")){
			fun <- pen.type
		}
		else {fun <- function(object){
				standardGeneric("pen.type")
			}
		}
		setGeneric("pen.type", fun)
	}
	setMethod("pen.type","cpt",function(object) object@pen.type)
	setMethod("pen.type","cpt.reg",function(object) object@pen.type)
	
	if(!isGeneric("pen.value")) {
		if (is.function("pen.value")){
			fun <- pen.value
		}
		else {fun <- function(object){
				standardGeneric("pen.value")
			}
		}
		setGeneric("pen.value", fun)
	}
	setMethod("pen.value","cpt",function(object) object@pen.value)
	setMethod("pen.value","cpt.reg",function(object) object@pen.value)

	if(!isGeneric("pen.value.full")) {
	  if (is.function("pen.value.full")){
	    fun <- pen.value.full
	  }
	  else {fun <- function(object){
	    standardGeneric("pen.value.full")
	  }
	  }
	  setGeneric("pen.value.full", fun)
	}
	setMethod("pen.value.full","cpt.range",function(object) object@pen.value.full)

  if(!isGeneric("minseglen")) {
	  if (is.function("minseglen")){
	    fun <- minseglen
	  }
	  else {fun <- function(object){
	    standardGeneric("minseglen")
	  }
	  }
	  setGeneric("minseglen", fun)
	}
	setMethod("minseglen","cpt",function(object) object@minseglen)
	
	if(!isGeneric("cpts")) {
		if (is.function("cpts")){
			fun <- cpts
		}
		else {fun <- function(object){
				standardGeneric("cpts")
			}
		}
		setGeneric("cpts", fun)
	}
	setMethod("cpts","cpt",function(object) object@cpts[-length(object@cpts)])
	setMethod("cpts","cpt.reg",function(object) object@cpts[-length(object@cpts)])
	
	if(!isGeneric("cpts.full")) {
	  if (is.function("cpts.full")){
	    fun <- cpts.full
	  }
	  else {fun <- function(object){
	    standardGeneric("cpts.full")
	  }
	  }
	  setGeneric("cpts.full", fun)
	}
	setMethod("cpts.full","cpt.range",function(object) object@cpts.full)

  if(!isGeneric("cpts.ts")) {
		if (is.function("cpts.ts")){
			fun <- cpts.ts
		}
		else {fun <- function(object){
				standardGeneric("cpts.ts")
			}
		}
		setGeneric("cpts.ts", fun)
	}
	setMethod("cpts.ts","cpt",function(object) index(data.set.ts(object))[cpts(object)] )

	if(!isGeneric("ncpts.max")) {
		if (is.function("ncpts.max")){
			fun <- ncpts.max
		}
		else {fun <- function(object){
				standardGeneric("ncpts.max")
			}
		}
		setGeneric("ncpts.max", fun)
	}
	setMethod("ncpts.max","cpt",function(object) object@ncpts.max)
	setMethod("ncpts.max","cpt.reg",function(object) object@ncpts.max)

	if(!isGeneric("param.est")) {
		if (is.function("param.est")){
			fun <- param.est
		}
		else {fun <- function(object){
				standardGeneric("param.est")
			}
		}
		setGeneric("param.est", fun)
	}
	setMethod("param.est","cpt",function(object) object@param.est)
	setMethod("param.est","cpt.reg",function(object) object@param.est)


  setMethod("coef","cpt",function(object) object@param.est)
	setMethod("coef","cpt.reg",function(object) object@param.est)
	
# ncpts function
	if(!isGeneric("ncpts")) {
		if (is.function("ncpts")){
			fun <- ncpts
		}
		else {fun <- function(object){
				standardGeneric("ncpts")
			}
		}
		setGeneric("ncpts", fun)
	}
	setMethod("ncpts","cpt",function(object) length(cpts(object)))
	setMethod("ncpts","cpt.reg",function(object) length(cpts(object)))

# seg.len function
	if(!isGeneric("seg.len")) {
		if (is.function("seg.len")){
			fun <- seg.len
		}
		else {fun <- function(object){
				standardGeneric("seg.len")
			}
		}
		setGeneric("seg.len", fun)
	}
	setMethod("seg.len","cpt",function(object){object@cpts-c(0,object@cpts[-length(object@cpts)])})
	setMethod("seg.len","cpt.reg",function(object){object@cpts-c(0,object@cpts[-length(object@cpts)])})
#i.e. if there is a changepoint in the data, return segment length. If not, return length of the data
	
  # nseg function
	if(!isGeneric("nseg")) {
	  if (is.function("nseg")){
	    fun <- nseg
	  }
	  else {fun <- function(object){
	    standardGeneric("nseg")
	  }
	  }
	  setGeneric("nseg", fun)
	}
	setMethod("nseg","cpt",function(object){ncpts(object)+1})
	setMethod("nseg","cpt.reg",function(object){ncpts(object)+1})
  
  
# replacement functions for slots
	setGeneric("data.set<-", function(object, value) standardGeneric("data.set<-"))
	setReplaceMethod("data.set", "cpt", function(object, value) {
		if(is.ts(value)){object@data.set <- value}else{object@data.set <- ts(value)}
		return(object)
	})
	setReplaceMethod("data.set", "cpt.reg", function(object, value) {
		object@data.set <- value
		return(object)
	})
	
	setGeneric("cpttype<-", function(object, value) standardGeneric("cpttype<-"))
	setReplaceMethod("cpttype", "cpt", function(object, value) {
		object@cpttype <- value
		return(object)
	})
	setReplaceMethod("cpttype", "cpt.reg", function(object, value) {
		object@cpttype <- value
		return(object)
	})
	
	setGeneric("method<-", function(object, value) standardGeneric("method<-"))
	setReplaceMethod("method", "cpt", function(object, value) {
		object@method <- value
		return(object)
	})
	setReplaceMethod("method", "cpt.reg", function(object, value) {
		object@method <- value
		return(object)
	})
	
  # distribution remains for backwards compatability, changed to test.stat version 1.0
	setGeneric("distribution<-", function(object, value) standardGeneric("distribution<-"))
	setReplaceMethod("distribution", "cpt", function(object, value) {
	  object@test.stat <- value
	  return(object)
	})
	setReplaceMethod("distribution", "cpt.reg", function(object, value) {
	  object@test.stat <- value
	  return(object)
	})

  setGeneric("test.stat<-", function(object, value) standardGeneric("test.stat<-"))
	setReplaceMethod("test.stat", "cpt", function(object, value) {
		object@test.stat <- value
		return(object)
	})
	setReplaceMethod("test.stat", "cpt.reg", function(object, value) {
		object@test.stat <- value
		return(object)
	})
	
	setGeneric("pen.type<-", function(object, value) standardGeneric("pen.type<-"))
	setReplaceMethod("pen.type", "cpt", function(object, value) {
		object@pen.type <- value
		return(object)
	})
	setReplaceMethod("pen.type", "cpt.reg", function(object, value) {
		object@pen.type <- value
		return(object)
	})
	
	setGeneric("pen.value<-", function(object, value) standardGeneric("pen.value<-"))
	setReplaceMethod("pen.value", "cpt", function(object, value) {
		object@pen.value <- value
		return(object)
	})
	setReplaceMethod("pen.value", "cpt.reg", function(object, value) {
		object@pen.value <- value
		return(object)
	})

	setGeneric("minseglen<-", function(object, value) standardGeneric("minseglen<-"))
	setReplaceMethod("minseglen", "cpt", function(object, value) {
	  object@minseglen <- value
	  return(object)
	})
	setReplaceMethod("minseglen", "cpt.range", function(object, value) {
	  object@minseglen <- value
	  return(object)
	})
	setReplaceMethod("minseglen", "cpt.reg", function(object, value) {
	  object@minseglen <- value
	  return(object)
	})
	
	setGeneric("cpts<-", function(object, value) standardGeneric("cpts<-"))
	setReplaceMethod("cpts", "cpt", function(object, value) {
    if(value[length(value)]==length(object@data.set)){object@cpts <- value}
    else{		object@cpts <- c(value,length(object@data.set))  }
		return(object)
	})
	setReplaceMethod("cpts", "cpt.reg", function(object, value) {
	  if(value[length(value)]==nrow(object@data.set)){object@cpts <- value}
	  else{  	object@cpts <- c(value,nrow(object@data.set))  }
	  return(object)
	})

	setGeneric("ncpts.max<-", function(object, value) standardGeneric("ncpts.max<-"))
	setReplaceMethod("ncpts.max", "cpt", function(object, value) {
		object@ncpts.max <- value
		return(object)
	})
	setReplaceMethod("ncpts.max", "cpt.reg", function(object, value) {
		object@ncpts.max <- value
		return(object)
	})
	
	setGeneric("param.est<-", function(object, value) standardGeneric("param.est<-"))
	setReplaceMethod("param.est", "cpt", function(object, value) {
		object@param.est <- value
		return(object)
	})
	setReplaceMethod("param.est", "cpt.reg", function(object, value) {
		object@param.est <- value
		return(object)
	})
  
	setGeneric("cpts.full<-", function(object, value) standardGeneric("cpts.full<-"))
	setReplaceMethod("cpts.full", "cpt.range", function(object, value) {
	  object@cpts.full <- value
	  return(object)
	})
	setGeneric("pen.value.full<-", function(object, value) standardGeneric("pen.value.full<-"))
	setReplaceMethod("pen.value.full", "cpt.range", function(object, value) {
	  object@pen.value.full <- value
	  return(object)
	})
# 	setGeneric("pen.value.input<-", function(object, value) standardGeneric("pen.value.input<-"))
# 	setReplaceMethod("pen.value.input", "cpt", function(object, value) {
# 	  object@pen.value.input <- value
# 	  return(object)
# 	})
  

# parameter functions
	setGeneric("param", function(object,...) standardGeneric("param"))
	setMethod("param", "cpt", function(object,shape,...) {			
		param.mean=function(object){
			cpts=c(0,object@cpts)
			#nseg=length(cpts)-1
			data=data.set(object)
			tmpmean=NULL
			for(j in 1:nseg(object)){
				tmpmean[j]=mean(data[(cpts[j]+1):(cpts[j+1])])
			}
			return(tmpmean)
		}
		param.var=function(object){
			cpts=c(0,object@cpts)
			#nseg=length(cpts)-1
			data=data.set(object)
			tmpvar=NULL
			for(j in 1:nseg(object)){
				tmpvar[j]=var(data[(cpts[j]+1):(cpts[j+1])])
			}
			return(tmpvar)
		}
		param.scale=function(object,shape){
			cpts=c(0,object@cpts)
			#nseg=length(cpts)-1
			data=data.set(object)
			y=c(0,cumsum(data))
			tmpscale=NULL
			for(j in 1:nseg(object)){
				tmpscale[j]=(y[(cpts[j+1]+1)]-y[(cpts[j]+1)])/((cpts[j+1]-cpts[j])*shape)
			}
			return(tmpscale)			
		}
		if(cpttype(object)=="mean"){
			param.est(object)<-list(mean=param.mean(object))
		}
		else if(cpttype(object)=="variance"){
			param.est(object)<-list(variance=param.var(object))
		}
		else if(cpttype(object)=="mean and variance"){
			if(test.stat(object)=="Normal"){
				param.est(object)<-list(mean=param.mean(object),variance=param.var(object))
			}
			else if(test.stat(object)=="Gamma"){
				param.est(object)<-list(scale=param.scale(object,shape=shape),shape=shape)
			}
			else if(test.stat(object)=="Exponential"){
				param.est(object)<-list(rate=1/param.mean(object))
			}
			else if(test.stat(object)=="Poisson"){
			  param.est(object)<-list(lambda=param.mean(object))
			}
			else{
				stop("Unknown test statistic for a change in mean and variance")
			}
		}
		else{
			stop("Unknown changepoint type, must be 'mean', 'variance' or 'mean and variance'")
		}
		return(object)
	})

	setMethod("param", "cpt.range", function(object,ncpts=NA,shape,...) {
	  if(is.na(ncpts)){
	    cpts=c(0,object@cpts)
	  }
	  else{
	    ncpts.full=apply(cpts.full(object),1,function(x){sum(x>0,na.rm=TRUE)})
	    row=try(which(ncpts.full==ncpts),silent=TRUE)
	    if(class(row)=='try-error'){
	      stop("Your input object doesn't have a segmentation with the requested number of changepoints.")
	    }
	    cpts=c(0,cpts.full(object)[row,1:ncpts],length(data.set(object)))
	  }
	  
	 	param.mean=function(object,cpts){
	 	  nseg=length(cpts)-1
	    data=data.set(object)
	    tmpmean=NULL
	    for(j in 1:nseg){
	      tmpmean[j]=mean(data[(cpts[j]+1):(cpts[j+1])])
	    }
	    return(tmpmean)
	  }
	  param.var=function(object,cpts){
	    nseg=length(cpts)-1
	    data=data.set(object)
	    tmpvar=NULL
	    for(j in 1:nseg){
	      tmpvar[j]=var(data[(cpts[j]+1):(cpts[j+1])])
	    }
	    return(tmpvar)
	  }
	  param.scale=function(object,cpts,shape){
	    nseg=length(cpts)-1
	    data=data.set(object)
	    y=c(0,cumsum(data))
	    tmpscale=NULL
	    for(j in 1:nseg){
	      tmpscale[j]=(y[(cpts[j+1]+1)]-y[(cpts[j]+1)])/((cpts[j+1]-cpts[j])*shape)
	    }
	    return(tmpscale)			
	  }
	  
	  if(cpttype(object)=="mean"){
	    param.est<-list(mean=param.mean(object,cpts))
	  }
	  else if(cpttype(object)=="variance"){
	    param.est<-list(variance=param.var(object,cpts))
	  }
	  else if(cpttype(object)=="mean and variance"){
	    if(test.stat(object)=="Normal"){
	      param.est<-list(mean=param.mean(object,cpts),variance=param.var(object,cpts))
	    }
	    else if(test.stat(object)=="Gamma"){
	      param.est<-list(scale=param.scale(object,cpts,shape=shape),shape=shape)
	    }
	    else if(test.stat(object)=="Exponential"){
	      param.est<-list(rate=1/param.mean(object,cpts))
	    }
	    else if(test.stat(object)=="Poisson"){
	      param.est<-list(lambda=param.mean(object,cpts))
	    }
	    else{
	      stop("Unknown test statistic for a change in mean and variance")
	    }
	  }
	  else{
	    stop("Unknown changepoint type, must be 'mean', 'variance' or 'mean and variance'")
	  }
	  if(is.na(ncpts)){
	    param.est(object)=param.est
	    return(object)
	  }
	  out=new('cpt.range')
	  param.est(out)=param.est
	  return(out)
	})
	
	setMethod("param", "cpt.reg", function(object,shape,...) {			
		param.norm=function(object){
			cpts=c(0,cpts(object))
		#	nseg=length(cpts)-1 #nseg(object)
			data=data.set(object)
			p=ncol(data)-1
			tmpbeta=matrix(NA,ncol=p,nrow=nseg(object))
			for(j in 1:nseg(object)){
				tmpbeta[j,]=solve(t(data[(cpts[j]+1):cpts[j+1],2:(p+1)])%*%data[(cpts[j]+1):cpts[j+1],2:(p+1)],t(data[(cpts[j]+1):cpts[j+1],2:(p+1)])%*%data[(cpts[j]+1):cpts[j+1],1])
			}
			return(tmpbeta)
		}
		if(test.stat(object)=="Normal"){
			param.est(object)<-list(beta=param.norm(object))
		}
		else{
			stop("Unknown test statistic, must be 'Normal'")
		}
		return(object)
	})

# summary functions
	setMethod("summary","cpt",function(object){
	    cat("Created Using changepoint version",object@version,'\n')
	    cat("Changepoint type      : Change in",cpttype(object),'\n')
	    cat("Method of analysis    :",method(object),"\n")
	    cat("Test Statistic  :", test.stat(object),"\n")
	    cat("Type of penalty       :", pen.type(object), "with value,",pen.value(object),"\n")
      cat("Minimum Segment Length :", minseglen(object),"\n")
	    cat("Maximum no. of cpts   :", ncpts.max(object),"\n")
	    if(length(cpts(object))<=20){cat("Changepoint Locations :",cpts(object),"\n")}
	    else{cat("Number of changepoints:", ncpts(object),"\n")}
	})

	setMethod("summary","cpt.range",function(object){
	  cat("Created Using changepoint version",object@version,'\n')
	  cat("Changepoint type      : Change in",cpttype(object),'\n')
	  cat("Method of analysis    :",method(object),"\n")
	  cat("Test Statistic  :", test.stat(object),"\n")
	  cat("Type of penalty       :", pen.type(object), "with value,",pen.value(object),"\n")
	  cat("Minimum Segment Length :", minseglen(object),"\n")
	  cat("Maximum no. of cpts   :", ncpts.max(object),"\n")
	  if(length(cpts(object))<=20){cat("Changepoint Locations :",cpts(object),"\n")}
	  else{cat("Number of changepoints:", ncpts(object),"\n")}
    if((nrow(cpts.full(object))<=5)&(ncol(cpts.full(object)<=20))){cat("Range of segmentations:\n");print(cpts.full(object));cat("\n For penalty values:", pen.value.full(object),"\n")}
    else{cat("Number of segmentations recorded:", nrow(cpts.full(object)), " with between ", sum(cpts.full(object)[nrow(cpts.full(object)),]>0,na.rm=T), " and ", sum(cpts.full(object)[1,]>0,na.rm=T), "changepoints.\n Penalty value ranges from:",min(pen.value.full(object))," to ",max(pen.value.full(object)))}
	})

  setMethod("summary","cpt.reg",function(object){
    cat("Created Using changepoint version",object@version,'\n')
    cat("Changepoint type     : Change in",cpttype(object),'\n')
	    cat("Method of analysis   :",method(object),"\n")
	    cat("Test Statistic :", test.stat(object),"\n")
	    cat("Type of penalty      :", pen.type(object), "with value,",pen.value(object),"\n")
	    cat("Maximum no. of cpts   :", ncpts.max(object),"\n")
	    if(length(cpts(object))<=20){cat("Changepoint Locations :",cpts(object),"\n")}
	    else{cat("Number of changepoints:", ncpts(object),"\n")}
	})

# show functions
	setMethod("show","cpt",function(object){
	    cat("Class 'cpt' : Changepoint Object\n")
	    cat("       ~~   : S4 class containing", length(attributes(object))-1, "slots with names\n")
	    cat("             ", names(attributes(object))[1:(length(attributes(object))-1)], "\n\n")
	    cat("Created on  :", object@date, "\n\n")
	    cat("summary(.)  :\n----------\n")
	    summary(object)
	})
	setMethod("show","cpt.reg",function(object){
	    cat("Class 'cpt.reg' : Changepoint Regression Object\n")
	    cat("       ~~   : S4 class containing", length(attributes(object))-1, "slots with names\n")
	    cat("             ", names(attributes(object))[1:(length(attributes(object))-1)], "\n\n")
	    cat("Created on  :", object@date, "\n\n")
	    cat("summary(.)  :\n----------\n")
	    summary(object)
	})

# plot functions
	setMethod("plot","cpt",function(x,cpt.col='red',cpt.width=1,cpt.style=1,...){
		plot(data.set.ts(x),...)
		if(cpttype(x)=="variance"){
			abline(v=index(data.set.ts(x))[cpts(x)],col=cpt.col,lwd=cpt.width,lty=cpt.style)
		}
		else if(cpttype(x)=="mean"  ||  cpttype(x)=="mean and variance"){
			#nseg=length(cpts(x))+1
			cpts=c(0,x@cpts)
			if((test.stat(x)=="Normal")||(test.stat(x)=="CUSUM")){
				means=param.est(x)$mean
			}
			else if(test.stat(x)=="Gamma"){
				means=param.est(x)$scale*param.est(x)$shape
			}
			else if(test.stat(x)=="Exponential"){
				means=1/param.est(x)$rate
			}
			else if(test.stat(x)=="Poisson"){
			  means=param.est(x)$lambda
			}
			else{
				stop('Invalid Changepoint test statistic')
			}
			for(i in 1:nseg(x)){
				segments(index(data.set.ts(x))[cpts[i]+1],means[i],index(data.set.ts(x))[cpts[i+1]],means[i],col=cpt.col,lwd=cpt.width,lty=cpt.style)
			}
		}
		else{
			stop('Invalid Changepoint Type for plotting.\n Can only plot mean, variance, mean and variance')
		}
	})

	setMethod("plot","cpt.range",function(x,ncpts=NA,diagnostic=FALSE,cpt.col='red',cpt.width=1,cpt.style=1,...){
	  if(diagnostic==TRUE){
      return(plot(apply(cpts.full(x),1,function(x){sum(x>0,na.rm=TRUE)}),pen.value.full(x),type='l',xlab='Number of Changepoints',ylab='Difference in Test Statistic',...))
	  }
	  plot(data.set.ts(x),...)
	  if(is.na(ncpts)){
	    if(pen.type(x)=="CROPS"){
	      stop('CROPS does not supply an optimal set of changepoints, set ncpts to the desired segmentation to plot or use diagnostic=TRUE to identify an appropriate number of changepoints')
	    }
	    cpts.to.plot=cpts(x)
	    param.est=x
	  }
	  else{
	    ncpts.full=apply(cpts.full(x),1,function(x){sum(x>0,na.rm=TRUE)})
	    row=which(ncpts.full==ncpts)
	    if(length(row)==0){
	      stop(paste("Your input object doesn't have a segmentation with the requested number of changepoints.\n Possible ncpts are: "),paste(ncpts.full,collapse=','))
	    }
	    cpts.to.plot=cpts.full(x)[row,1:ncpts]
	    if(test.stat(x)=="Gamma"){
	      param.est=param(x,ncpts,shape=param.est(x)$shape)
	    }
	    else{
	      param.est=param(x,ncpts)
	    }
	  }
	  if(cpttype(x)=="variance"){
	    abline(v=index(data.set.ts(x))[cpts.to.plot],col=cpt.col,lwd=cpt.width,lty=cpt.style)
	  }
	  else if(cpttype(x)=="mean"  ||  cpttype(x)=="mean and variance"){
	    if((test.stat(x)=="Normal")||(test.stat(x)=="CUSUM")){
	      means=param.est(param.est)$mean
	    }
	    else if(test.stat(x)=="Gamma"){
	      means=param.est(param.est)$scale*param.est(param.est)$shape
	    }
	    else if(test.stat(x)=="Exponential"){
	      means=1/param.est(param.est)$rate
	    }
	    else if(test.stat(x)=="Poisson"){
	      means=param.est(param.est)$lambda
	    }
	    else{
	      stop('Invalid Changepoint test statistic')
	    }
	    nseg=length(means)
	    cpts.to.plot=c(0,cpts.to.plot,length(data.set(x)))
	    for(i in 1:nseg){
	      segments(index(data.set.ts(x))[cpts.to.plot[i]+1],means[i],index(data.set.ts(x))[cpts.to.plot[i+1]],means[i],col=cpt.col,lwd=cpt.width,lty=cpt.style)
	    }
	  }
	  else{
	    stop('Invalid Changepoint Type for plotting.\n Can only plot mean, variance, mean and variance')
	  }
	})
	
	setMethod("plot","cpt.reg",function(x,cpt.col='red',cpt.width=1,cpt.style=1,...){
		if(dim(data.set(x))[2]>3){
			stop("A plot function for regression of more than one regressor is not available")
		}
		else if(dim(data.set(x))[2]==3){
			if(data.set(x)[1,2]==1){
				#nseg=length(cpts(x))+1
				cpts=c(0,x@cpts)
				betas=param.est(x)$beta
				plot(data.set(x[,3]),data.set(x[,1]),...)
				for(i in 1:nseg(x)){
					segments(cpts[i]+1,betas[i,1]*data.set(x)[cpts[i]+1,2]+betas[i,2]*data.set(x)[cpts[i]+1,3],cpts[i+1],betas[i,1]*data.set(x)[cpts[i+1],2]+betas[i,2]*data.set(x)[cpts[i+1],3],col=cpt.col,lwd=cpt.width,lty=cpt.style)
				}
			}
			if(data.set(x)[1,3]==1){
				#nseg=length(cpts(x))+1
				cpts=c(0,x@cpts)
				betas=param.est(x)$beta
				plot(data.set(x[,2]),data.set(x[,1]),...)
				for(i in 1:nseg(x)){
					segments(cpts[i]+1,betas[i,2]*data.set(x)[cpts[i]+1,3]+betas[i,1]*data.set(x)[cpts[i]+1,2],cpts[i+1],betas[i,2]*data.set(x)[cpts[i+1],3]+betas[i,1]*data.set(x)[cpts[i+1],2],col=cpt.col,lwd=cpt.width,lty=cpt.style)
				}
			}
			else{
				stop("A plot function for regression of more than one regressor is not available")
			}
		}
		else{
			#nseg=length(cpts(x))+1
			cpts=c(0,x@cpts)
			betas=param.est(x)$beta
			plot(data.set(x[,2]),data.set(x[,1]),...)
			for(i in 1:nseg(x)){
				segments(cpts[i]+1,betas[i,1]*data.set(x)[cpts[i]+1,2],cpts[i+1],betas[i,2]+betas[i,1]*data.set(x)[cpts[i+1],2],col=cpt.col,lwd=cpt.width,lty=cpt.style)
			}
		}
	})

# likelihood functions
	setMethod("logLik", "cpt", function(object) {
		if(test.stat(object)=="Normal"){
			if(cpttype(object)=="mean"){
				mll.mean=function(x2,x,n){
				  return( x2-(x^2)/n)
				}
				y2=c(0,cumsum(data.set(object)^2))
				y=c(0,cumsum(data.set(object)))
				cpts=c(0,object@cpts)
				#nseg=length(cpts)-1
				tmplike=0
				for(j in 1:nseg(object)){
			    tmplike=tmplike+mll.mean(y2[cpts[j+1]+1]-y2[cpts[j]+1],y[cpts[j+1]+1]-y[cpts[j]+1],cpts[j+1]-cpts[j])
				}
				##c(tmplike, tmplike+(nseg-2)*pen.value(object)+sum(log(cpts[-1]-cpts[-(nseg+1)])))
        if(pen.type(object)=="MBIC"){
          like=c(tmplike, tmplike+(nseg(object)-2)*pen.value(object)+sum(log(cpts[-1]-cpts[-(nseg(object)+1)])))
        }else{
				  like=c(tmplike,tmplike+(nseg(object)-1)*pen.value(object))
        }
				names(like)=c("-like","-likepen")
			}
			else if(cpttype(object)=="variance"){
				mll.var=function(x,n){
					neg=x<=0
					x[neg==TRUE]=0.00000000001    
					return( n*(log(2*pi)+log(x/n)+1))
				}
				y2=c(0,cumsum(data.set(object)^2))
				cpts=c(0,object@cpts)
				#nseg=length(cpts)-1
				tmplike=0
				for(j in 1:nseg(object)){
					tmplike=tmplike+mll.var(y2[cpts[j+1]+1]-y2[cpts[j]+1],cpts[j+1]-cpts[j])
				}
				if(pen.type(object)=="MBIC"){
				  like=c(tmplike, tmplike+(nseg(object)-2)*pen.value(object)+sum(log(cpts[-1]-cpts[-(nseg(object)+1)])))
				}else{
				  like=c(tmplike,tmplike+(nseg(object)-1)*pen.value(object))
				}
				names(like)=c("-like","-likepen")
			}
			else if(cpttype(object)=="mean and variance"){
				mll.meanvar=function(x2,x,n){
					sigmasq=(1/n)*(x2-(x^2)/n)
					neg=sigmasq<=0
					sigmasq[neg==TRUE]=0.00000000001
					return( n*(log(2*pi)+log(sigmasq)+1))
				}
				y2=c(0,cumsum(data.set(object)^2))
				y=c(0,cumsum(data.set(object)))
				cpts=c(0,object@cpts)
				#nseg=length(cpts)-1
				tmplike=0
				for(j in 1:nseg(object)){
					tmplike=tmplike+mll.meanvar(y2[cpts[j+1]+1]-y2[cpts[j]+1],y[cpts[j+1]+1]-y[cpts[j]+1],cpts[j+1]-cpts[j])
				}
				if(pen.type(object)=="MBIC"){
				  like=c(tmplike, tmplike+(nseg(object)-2)*pen.value(object)+sum(log(cpts[-1]-cpts[-(nseg(object)+1)])))
				}else{
				  like=c(tmplike,tmplike+(nseg(object)-1)*pen.value(object))
				}
				names(like)=c("-like","-likepen")
			}
			else{
				stop("Unknown changepoint type, must be 'mean', 'variance' or 'mean and variance'")
			}
		}
		else if(test.stat(object)=="Gamma"){
			if(cpttype(object)!="mean and variance"){
				stop("Unknown changepoint type for test.stat='Gamma', must be 'mean and variance'")
			}
			else{
			  mll.meanvarg=function(x,n,shape){
			    return(n*shape*log(n*shape)-n*shape*log(x))
			  }
				y=c(0,cumsum(data.set(object)))
				shape=param.est(object)$shape
				cpts=c(0,object@cpts)
				#nseg=length(cpts)-1
				tmplike=0
				for(j in 1:nseg(object)){
					tmplike=tmplike+mll.meanvarg(y[cpts[j+1]+1]-y[cpts[j]+1],cpts[j+1]-cpts[j],shape)
				}
				if(pen.type(object)=="MBIC"){
				  like=c(tmplike, tmplike+(nseg(object)-2)*pen.value(object)+sum(log(cpts[-1]-cpts[-(nseg(object)+1)])))
				}else{
				  like=c(tmplike,tmplike+(nseg(object)-1)*pen.value(object))
				}
				names(like)=c("-like","-likepen")
			}
		}
		else if(test.stat(object)=="Exponential"){
			if(cpttype(object)!="mean and variance"){
				stop("Unknown changepoint type for test.stat='Exponential', must be 'mean and variance'")
			}
			else{
			  mll.meanvare=function(x,n){
			    return(n*log(n)-n*log(x))
			  }
				y=c(0,cumsum(data.set(object)))
				cpts=c(0,object@cpts)
				#nseg=length(cpts)-1
				tmplike=0
				for(j in 1:nseg(object)){
					tmplike=tmplike+mll.meanvare(y[cpts[j+1]+1]-y[cpts[j]+1],cpts[j+1]-cpts[j])
				}
				if(pen.type(object)=="MBIC"){
				  like=c(tmplike, tmplike+(nseg(object)-2)*pen.value(object)+sum(log(cpts[-1]-cpts[-(nseg(object)+1)])))
				}else{
				  like=c(tmplike,tmplike+(nseg(object)-1)*pen.value(object))
				}
				names(like)=c("-like","-likepen")
			}
		}
		else if(test.stat(object)=="Poisson"){
		  if(cpttype(object)!="mean and variance"){
		    stop("Unknown changepoint type for test.stat='Poisson', must be 'mean and variance'")
		  }
		  else{
		    mll.meanvarp=function(x,n){
		      return(x*log(x)-x*log(n))
		    }
		    y=c(0,cumsum(data.set(object)))
		    cpts=c(0,object@cpts)
		    #nseg=length(cpts)-1
		    tmplike=0
		    for(j in 1:nseg(object)){
		      tmplike=tmplike+mll.meanvarp(y[cpts[j+1]+1]-y[cpts[j]+1],cpts[j+1]-cpts[j])
		    }
		    if(pen.type(object)=="MBIC"){
		      like=c(tmplike, tmplike+(nseg(object)-2)*pen.value(object)+sum(log(cpts[-1]-cpts[-(nseg(object)+1)])))
		    }else{
		      like=c(tmplike,tmplike+(nseg(object)-1)*pen.value(object))
		    }
		    names(like)=c("-like","-likepen")
		  }
		}
		else{stop("logLik is only valid for distributional assumptions, not CUSUM or CSS")}
		return(like)
	})

	setMethod("logLik", "cpt.range", function(object,ncpts=NA) {
	  if(is.na(ncpts)){
	    if(pen.type(object)=="CROPS"){
	      stop('CROPS does not supply an optimal set of changepoints, set ncpts argument to the desired segmentation to plot or use diagnostic=TRUE to identify an appropriate number of changepoints')
	    }
	    cpts=c(0,object@cpts)
      pen.value=pen.value(object)
	  }
	  else{
	    ncpts.full=apply(cpts.full(object),1,function(x){sum(x>0,na.rm=TRUE)})
	    row=which(ncpts.full==ncpts)
	    if(length(row)==0){
	      stop(paste("Your input object doesn't have a segmentation with the requested number of changepoints.\n Possible ncpts are: "),paste(ncpts.full,collapse=','))
	    }
	    cpts=c(0,cpts.full(object)[row,1:ncpts],length(data.set(object)))
      pen.value=pen.value.full(object)[row]
	  }
	  nseg=length(cpts)-1
	  
	  if(test.stat(object)=="Normal"){
	    if(cpttype(object)=="mean"){
	      mll.mean=function(x2,x,n){
	        return( x2-(x^2)/n)
	      }
	      y2=c(0,cumsum(data.set(object)^2))
	      y=c(0,cumsum(data.set(object)))
	      tmplike=0
	      for(j in 1:nseg){
	        tmplike=tmplike+mll.mean(y2[cpts[j+1]+1]-y2[cpts[j]+1],y[cpts[j+1]+1]-y[cpts[j]+1],cpts[j+1]-cpts[j])
	      }
	      ##c(tmplike, tmplike+(nseg-2)*pen.value(object)+sum(log(cpts[-1]-cpts[-(nseg+1)])))
	      if(pen.type(object)=="MBIC"){
	        like=c(tmplike, tmplike+(nseg-2)*pen.value+sum(log(cpts[-1]-cpts[-(nseg+1)])))
	      }else{
	        like=c(tmplike,tmplike+(nseg-1)*pen.value)
	      }
	      names(like)=c("-like","-likepen")
	    }
	    else if(cpttype(object)=="variance"){
	      mll.var=function(x,n){
	        neg=x<=0
	        x[neg==TRUE]=0.00000000001    
	        return( n*(log(2*pi)+log(x/n)+1))
	      }
	      y2=c(0,cumsum((data.set(object)-param.est(object)$mean)^2))
	      tmplike=0
	      for(j in 1:nseg){
	        tmplike=tmplike+mll.var(y2[cpts[j+1]+1]-y2[cpts[j]+1],cpts[j+1]-cpts[j])
	      }
	      if(pen.type(object)=="MBIC"){
	        like=c(tmplike, tmplike+(nseg-2)*pen.value+sum(log(cpts[-1]-cpts[-(nseg+1)])))
	      }else{
	        like=c(tmplike,tmplike+(nseg-1)*pen.value)
	      }
	      names(like)=c("-like","-likepen")
	    }
	    else if(cpttype(object)=="mean and variance"){
	      mll.meanvar=function(x2,x,n){
	        sigmasq=(1/n)*(x2-(x^2)/n)
	        neg=sigmasq<=0
	        sigmasq[neg==TRUE]=0.00000000001
	        return( n*(log(2*pi)+log(sigmasq)+1))
	      }
	      y2=c(0,cumsum(data.set(object)^2))
	      y=c(0,cumsum(data.set(object)))
	      tmplike=0
	      for(j in 1:nseg){
	        tmplike=tmplike+mll.meanvar(y2[cpts[j+1]+1]-y2[cpts[j]+1],y[cpts[j+1]+1]-y[cpts[j]+1],cpts[j+1]-cpts[j])
	      }
	      if(pen.type(object)=="MBIC"){
	        like=c(tmplike, tmplike+(nseg-2)*pen.value+sum(log(cpts[-1]-cpts[-(nseg+1)])))
	      }else{
	        like=c(tmplike,tmplike+(nseg-1)*pen.value)
	      }
	      names(like)=c("-like","-likepen")
	    }
	    else{
	      stop("Unknown changepoint type, must be 'mean', 'variance' or 'mean and variance'")
	    }
	  }
	  else if(test.stat(object)=="Gamma"){
	    if(cpttype(object)!="mean and variance"){
	      stop("Unknown changepoint type for test.stat='Gamma', must be 'mean and variance'")
	    }
	    else{
	      mll.meanvarg=function(x,n,shape){
	        return(n*shape*log(n*shape)-n*shape*log(x))
	      }
	      y=c(0,cumsum(data.set(object)))
	      shape=param.est(object)$shape
	      cpts=c(0,object@cpts)
	      #nseg=length(cpts)-1
	      tmplike=0
	      for(j in 1:nseg){
	        tmplike=tmplike+mll.meanvarg(y[cpts[j+1]+1]-y[cpts[j]+1],cpts[j+1]-cpts[j],shape)
	      }
	      if(pen.type(object)=="MBIC"){
	        like=c(tmplike, tmplike+(nseg-2)*pen.value+sum(log(cpts[-1]-cpts[-(nseg+1)])))
	      }else{
	        like=c(tmplike,tmplike+(nseg-1)*pen.value)
	      }
	      names(like)=c("-like","-likepen")
	    }
	  }
	  else if(test.stat(object)=="Exponential"){
	    if(cpttype(object)!="mean and variance"){
	      stop("Unknown changepoint type for test.stat='Exponential', must be 'mean and variance'")
	    }
	    else{
	      mll.meanvare=function(x,n){
	        return(n*log(n)-n*log(x))
	      }
	      y=c(0,cumsum(data.set(object)))
	      cpts=c(0,object@cpts)
	      #nseg=length(cpts)-1
	      tmplike=0
	      for(j in 1:nseg){
	        tmplike=tmplike+mll.meanvare(y[cpts[j+1]+1]-y[cpts[j]+1],cpts[j+1]-cpts[j])
	      }
	      if(pen.type(object)=="MBIC"){
	        like=c(tmplike, tmplike+(nseg-2)*pen.value+sum(log(cpts[-1]-cpts[-(nseg+1)])))
	      }else{
	        like=c(tmplike,tmplike+(nseg-1)*pen.value)
	      }
	      names(like)=c("-like","-likepen")
	    }
	  }
	  else if(test.stat(object)=="Poisson"){
	    if(cpttype(object)!="mean and variance"){
	      stop("Unknown changepoint type for test.stat='Poisson', must be 'mean and variance'")
	    }
	    else{
	      mll.meanvarp=function(x,n){
	        return(x*log(x)-x*log(n))
	      }
	      y=c(0,cumsum(data.set(object)))
	      cpts=c(0,object@cpts)
	      #nseg=length(cpts)-1
	      tmplike=0
	      for(j in 1:nseg){
	        tmplike=tmplike+mll.meanvarp(y[cpts[j+1]+1]-y[cpts[j]+1],cpts[j+1]-cpts[j])
	      }
	      if(pen.type(object)=="MBIC"){
	        like=c(tmplike, tmplike+(nseg-2)*pen.value+sum(log(cpts[-1]-cpts[-(nseg+1)])))
	      }else{
	        like=c(tmplike,tmplike+(nseg-1)*pen.value)
	      }
	      names(like)=c("-like","-likepen")
	    }
	  }
	  else{stop("logLik is only valid for distributional assumptions, not CUSUM or CSS")}
	  return(like)
	})

	setGeneric("likelihood", function(object) standardGeneric("likelihood"))
	setMethod("likelihood", "cpt", function(object) {
		return(logLik(object))
	})
