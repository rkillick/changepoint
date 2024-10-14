setClass("cpt",slots=list(data.set="ts", cpttype="character", method="character", 	test.stat="character",pen.type="character",pen.value="numeric",minseglen="numeric",cpts="numeric",ncpts.max="numeric",param.est="list",date="character",version="character"),prototype=prototype(cpttype="Not Set",date=date(),version=as(packageVersion("changepoint"),'character')))

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
		else {fun <- function(object,...){
				standardGeneric("cpts")
			}
		}
		setGeneric("cpts", fun)
	}
	setMethod("cpts","cpt",function(object) object@cpts[-length(object@cpts)])
	setMethod("cpts","cpt.reg",function(object) object@cpts[-length(object@cpts)])
	setMethod("cpts","cpt.range",function(object,ncpts=NA){
	  if(is.na(ncpts)){return(object@cpts[-length(object@cpts)])}
	  else{
	    ncpts.full=apply(cpts.full(object),1,function(x){sum(x>0,na.rm=TRUE)})
	    row=try(which(ncpts.full==ncpts),silent=TRUE)
	    if(inherits(row,'try-error')){
	      stop("Your input object doesn't have a segmentation with the requested number of changepoints.\n Possible ncpts are: ",paste(ncpts.full,collapse=','))
	    }
	    else{return(cpts.full(object)[row,1:ncpts])}
	  }
	 })

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
		else {fun <- function(object,...){
				standardGeneric("seg.len")
			}
		}
		setGeneric("seg.len", fun)
	}
	setMethod("seg.len","cpt",function(object){object@cpts-c(0,object@cpts[-length(object@cpts)])})
	setMethod("seg.len","cpt.reg",function(object){object@cpts-c(0,object@cpts[-length(object@cpts)])})
  setMethod("seg.len","cpt.range",function(object,ncpts=NA){
    if(is.na(ncpts)){return(object@cpts-c(0,object@cpts[-length(object@cpts)]))} # works with no cpts specified too
    cpts=c(0,cpts(object,ncpts=ncpts),length(data.set(object)))
    return(cpts[-1]-cpts[-length(cpts)])
  })
#i.e. if there is a changepoint in the data, return segment length. If not, return length of the data

  # nseg function
	if(!isGeneric("nseg")) {
	  if (is.function("nseg")){
	    fun <- nseg
	  }
	  else {fun <- function(object,...){
	    standardGeneric("nseg")
	  }
	  }
	  setGeneric("nseg", fun)
	}
	setMethod("nseg","cpt",function(object){ncpts(object)+1})
	setMethod("nseg","cpt.reg",function(object){ncpts(object)+1})
  setMethod("nseg","cpt.range",function(object,ncpts=NA){
    if(is.na(ncpts)){return(ncpts(object)+1)}
    return(ncpts+1)
  })


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
	  if((cpttype(object)=="meanar")|(cpttype(object)=="trendar")){
	    n=length(object@data.set)-1
	  }
	  else{n=length(object@data.set)}

    if(value[length(value)]==n){object@cpts <- value}
    else{		object@cpts <- c(value,n)  }
		return(object)
	})
	setReplaceMethod("cpts", "cpt.range", function(object, value) {
	  if((cpttype(object)=="meanar")|(cpttype(object)=="trendar")){
	    n=length(object@data.set)-1
	  }
	  else{n=length(object@data.set)}

	  if(value[length(value)]==n){object@cpts <- value}
	  else{		object@cpts <- c(value,n)  }
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
		if(cpttype(object)=="mean"){
			param.est(object)<-list(mean=fit.mean(object))
		}
		else if(cpttype(object)=="variance"){
			param.est(object)<-list(variance=fit.var(object))
		}
		else if(cpttype(object)=="mean and variance"){
			if(test.stat(object)=="Normal"){
				param.est(object)<-list(mean=fit.mean(object),variance=fit.var(object))
			}
			else if(test.stat(object)=="Gamma"){
				param.est(object)<-list(scale=fit.scale(object,shape=shape),shape=shape)
			}
			else if(test.stat(object)=="Exponential"){
				param.est(object)<-list(rate=1/fit.mean(object))
			}
			else if(test.stat(object)=="Poisson"){
			  param.est(object)<-list(lambda=fit.mean(object))
			}
			else{
				stop("Unknown test statistic for a change in mean and variance")
			}
		}
		else if(cpttype(object)=="trend"){
		  if(test.stat(object)=="Normal"){
		    tmp=fit.trend(object)
		    param.est(object)<-list(thetaS=tmp[,1],thetaT=tmp[,2])
		  }
		  else{
		    stop("Unknown test statistic for a change in trend")
		  }
		}
		else if(cpttype(object)=="trendar"){
		  if(test.stat(object)=="Normal"){
		    tmp=fit.trendar(object)
		    param.est(object)<-list(beta=tmp[,1],thetajpo=tmp[,2],thetaj=tmp[,3])
		  }
		  else{
		    stop("Unknown test statistic for a change in trend+ar")
		  }
		}
		else if(cpttype(object)=="meanar"){
		  if(test.stat(object)=="Normal"){
		    tmp=fit.meanar(object)
		    param.est(object)<-list(beta1=tmp[,1],beta2=tmp[,2])
		  }
		  else{
		    stop("Unknown test statistic for a change in mean+ar")
		  }
		}
		else{
			stop("Unknown changepoint type, must be 'mean', 'variance', 'mean and variance', 'trend', 'meanar' or 'trendar'.")
		}
		return(object)
	})

	setMethod("param", "cpt.range", function(object,ncpts=NA,shape,...) {
	  if(is.na(ncpts)){
	    cpts=object@cpts
	    if(cpts[1]!=0){cpts=c(0,cpts)} # PELT derivatives don't include the 0
	  }
	  else{
      cpts=c(0,cpts(object,ncpts),length(data.set(object))) # cpts returns without 0 and n
	  }

	  if(cpttype(object)=="mean"){
	    param.est<-list(mean=fit.mean(object,cpts))
	  }
	  else if(cpttype(object)=="variance"){
	    param.est<-list(variance=fit.var(object,cpts))
	  }
	  else if(cpttype(object)=="mean and variance"){
	    if(test.stat(object)=="Normal"){
	      param.est<-list(mean=fit.mean(object,cpts),variance=fit.var(object,cpts))
	    }
	    else if(test.stat(object)=="Gamma"){
	      param.est<-list(scale=fit.scale(object,cpts,shape=shape),shape=shape)
	    }
	    else if(test.stat(object)=="Exponential"){
	      param.est<-list(rate=1/fit.mean(object,cpts))
	    }
	    else if(test.stat(object)=="Poisson"){
	      param.est<-list(lambda=fit.mean(object,cpts))
	    }
	    else{
	      stop("Unknown test statistic for a change in mean and variance")
	    }
	  }
	  else if(cpttype(object)=="trend"){
	    if(test.stat(object)=="Normal"){
	      tmp=fit.trend(object,cpts)
	      param.est(object)<-list(thetaS=tmp[,1],thetaT=tmp[,2])
	    }
	    else{
	      stop("Unknown test statistic for a change in trend")
	    }
	  }
	  else if(cpttype(object)=="trendar"){
	    if(test.stat(object)=="Normal"){
	      tmp=fit.trendar(object,cpts)
	      param.est(object)<-list(beta=tmp[,1],thetajpo=tmp[,2],thetaj=tmp[,3])
	    }
	    else{
	      stop("Unknown test statistic for a change in trend+ar")
	    }
	  }
	  else if(cpttype(object)=="meanar"){
	    if(test.stat(object)=="Normal"){
	      tmp=fit.meanar(object,cpts)
	      param.est(object)<-list(beta1=tmp[,1],beta2=tmp[,2])
	    }
	    else{
	      stop("Unknown test statistic for a change in mean+ar")
	    }
	  }
	  else{
	    stop("Unknown changepoint type, must be 'mean', 'variance', 'mean and variance', 'trend', 'meanar' or 'trendar'")
	  }
	  if(is.na(ncpts)){
	    param.est(object)=param.est
	    return(object)
	  }
	  out=new('cpt.range')
	  param.est(out)=param.est
	  cpts(out)=cpts[-1] # keeps the n, not the 0
	  return(out)
	})

	setMethod("param", "cpt.reg", function(object,shape,...) {
		if(test.stat(object)=="Normal"){
			param.est(object)<-fit.reg(object)
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
	  if(length(param.est(x))==0){# i.e. parameter.estimates=FALSE in call
	    cat('Calculating parameter estimates...')
	    object=param(x)
	    cat('done.\n')
	  }
		plot(data.set.ts(x),...)
		if(cpttype(x)=="variance" || cpttype(x)=="nonparametric (empirical_distribution)"){
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
	  else if(cpttype(x)=="trend"){
	    cpts=c(0,x@cpts)
	    intercept=rep(param.est(x)$thetaS,x@cpts-c(0,cpts(x)))
	    slope=rep(param.est(x)$thetaT-param.est(x)$thetaS,x@cpts-c(0,cpts(x)))/rep(x@cpts-c(0,cpts(x)),x@cpts-c(0,cpts(x)))
	    cptn=rep(c(0,cpts(x)),x@cpts-c(0,cpts(x)))
	    n=length(data.set(x))
	    means=intercept+slope*((1:n)-cptn)
	    for(i in 1:nseg(x)){
	      segments(index(data.set.ts(x))[cpts[i]+1],means[cpts[i]+1],index(data.set.ts(x))[cpts[i+1]],means[cpts[i+1]],col=cpt.col,lwd=cpt.width,lty=cpt.style)
	    }
	  }
		else{
			stop('Invalid Changepoint Type for plotting.\n Can only plot mean, variance, mean and variance')
		}
	})

	setMethod("plot","cpt.range",function(x,ncpts=NA,diagnostic=FALSE,cpt.col='red',cpt.width=1,cpt.style=1,...){
	  if(diagnostic==TRUE){
	    n.changepoints = apply(cpts.full(x), 1, function(x) sum(x > 0, na.rm = TRUE))
	    n.changepoints=c(n.changepoints,n.changepoints[length(n.changepoints)]) # repeat the last value as this is also the number of changes for the upper pen.value tested
	    penalty.values = pen.value.full(x)
	    if (is.null(list(...)$type)) {
	      # By default, the type of the diagnostic plots is "lines".
	      plot(x = penalty.values, y = n.changepoints, type="s",ylab = 'Number of Changepoints', xlab = 'Penalty Value', ...)
	    } else {
	      plot(x = penalty.values, y = n.changepoints,ylab = 'Number of Changepoints', xlab = 'Penalty Value', ...)
	    }
	    return(invisible(NULL))
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
	    cpts.to.plot=cpts(x,ncpts=ncpts)
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
	  if(length(param.est(x))==0){# i.e. parameter.estimates=FALSE in call
	    cat('Calculating parameter estimates...')
	    object=param(x)
	    cat('done.\n')
	  }
	  plot(data.set(x)[,1],type='l',...)
	  if(test.stat(x)=="Normal"){
	    cpts=c(0,x@cpts)
	    betas=param.est(x)$beta
	    for(i in 1:nseg(x)){
	      lines((cpts[i]+1):cpts[i+1],betas[i,]%*%t(data.set(x)[(cpts[i]+1):cpts[i+1],-1]),col=cpt.col,lwd=cpt.width,lty=cpt.style)
	    }
	  }
	  else{
	    stop('Invalid Changepoint test statistic')
	  }
	})

# likelihood functions
	setMethod("logLik", "cpt", function(object) {
	  if(length(param.est(object))==0){# i.e. parameter.estimates=FALSE in call
	    cat('Calculating parameter estimates...')
	    object=param(object)
	    cat('done.\n')
	  }
		if(test.stat(object)=="Normal"){
			if(cpttype(object)=="mean"){
        		means=rep(param.est(object)$mean,object@cpts-c(0,cpts(object)))
        		rss=sum((data.set(object)-means)^2)
        		n=length(data.set(object))
        		like=n*(log(2*pi)+log(rss/n)+1) # -2*loglik
        		cpts=c(0,object@cpts)
        		if(pen.type(object)=="MBIC"){
        		  like=c(like, like+(nseg(object)-2)*pen.value(object)+sum(log(cpts[-1]-cpts[-(nseg(object)+1)])))
        		}else{
				  like=c(like,like+(nseg(object)-1)*pen.value(object))
        }
			}
			else if(cpttype(object)=="variance"){
        		rss=c(0,cumsum((data.set(object)-param.est(object)$mean)^2))
		        cpts=c(0,object@cpts)
        		n=length(data.set(object))
		        seglen=seg.len(object)
		        sigmas=(rss[cpts[-1]+1]-rss[cpts[-length(cpts)]+1])/seglen
        		like=n*log(2*pi)+sum(seglen*log(sigmas))+n
				if(pen.type(object)=="MBIC"){
				  like=c(like, like+(nseg(object)-2)*pen.value(object)+sum(log(seglen)))
				}else{
				  like=c(like,like+(nseg(object)-1)*pen.value(object))
				}
			}
			else if(cpttype(object)=="mean and variance"){
			  means=rep(param.est(object)$mean,object@cpts-c(0,cpts(object)))
			  rss=sum((data.set(object)-means)^2)
			  n=length(data.set(object))
			  cpts=c(0,object@cpts)
			  seglen=seg.len(object)
			  sigmas=param.est(object)$variance
			  like=n*log(2*pi)+sum(seglen*log(sigmas))+n
				if(pen.type(object)=="MBIC"){
				  like=c(like,like+(nseg(object)-2)*pen.value(object)+sum(log(seglen)))
				}else{
				  like=c(like,like+(nseg(object)-1)*pen.value(object))
				}
			}
		  else if(cpttype(object)=="trend"){
		    intercept=rep(param.est(object)$thetaS,object@cpts-c(0,cpts(object)))
		    slope=rep(param.est(object)$thetaT-param.est(object)$thetaS,object@cpts-c(0,cpts(object)))/rep(object@cpts-c(0,cpts(object)),object@cpts-c(0,cpts(object)))
		    cptn=rep(c(0,cpts(object)),object@cpts-c(0,cpts(object)))
		    n=length(data.set(object))
		    means=intercept+slope*((1:n)-cptn)
		    rss=sum((data.set(object)-means)^2)
		    like=n*(log(2*pi)+log(rss/n)+1) # -2*loglik
		    if(pen.type(object)=="MBIC"){
		      like=c(like, like+(nseg(object)-2)*pen.value(object)+sum(log(seg.len(object))))
		    }else{
		      like=c(like,like+(nseg(object)-1)*pen.value(object))
		    }
		  }
		  else if(cpttype(object)=="trendar"){
		    seglen=seg.len(object)
		    intercept=rep(param.est(object)$thetaj,seglen)
		    slope=rep(param.est(object)$thetajpo-param.est(object)$thetaj,seglen)/rep(seglen,seglen)
		    ar=rep(param.est(object)$beta,seglen)
		    cptn=rep(c(0,cpts(object)),seglen)
		    n=length(data.set(object))
		    means=NULL;means[1]=0
		    for(i in 2:n){means[i]=intercept+slope*((1:n)-cptn)+ar*means[i-1]}
		    means=means[-1]
		    rss=sum((data.set(object)[-1]-means)^2)
		    like=n*(log(2*pi)+log(rss/n)+1) # -2*loglik
		    if(pen.type(object)=="MBIC"){
		      like=c(like, like+(nseg(object)-2)*pen.value(object)+sum(log(seg.len(object))))
		    }else{
		      like=c(like,like+(nseg(object)-1)*pen.value(object))
		    }
		  }
		  else if(cpttype(object)=="meanar"){
		    seglen=seg.len(object)
		    intercept=rep(param.est(object)$beta1,seglen)
		    ar=rep(param.est(object)$beta2,seglen)
		    cptn=rep(c(0,cpts(object)),seglen)
		    n=length(data.set(object))
		    means[1]=0;for(i in 2:n){means[i]=intercept+ar*means[i-1]}
		    means=means[-1]
		    rss=sum((data.set(object)[-1]-means)^2)
		    like=n*(log(2*pi)+log(rss/n)+1) # -2*loglik
		    if(pen.type(object)=="MBIC"){
		      like=c(like, like+(nseg(object)-2)*pen.value(object)+sum(log(seg.len(object))))
		    }else{
		      like=c(like,like+(nseg(object)-1)*pen.value(object))
		    }
		  }
			else{
				stop("Unknown changepoint type, must be 'mean', 'variance', 'mean and variance', 'trend', 'meanar' or 'trendar'")
			}
		}
		else if(test.stat(object)=="Gamma"){
			if(cpttype(object)!="mean and variance"){
				stop("Unknown changepoint type for test.stat='Gamma', must be 'mean and variance'")
			}
			else{
			  warning("Not changed to be -2*logLik")
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
				  like=c(tmplike, tmplike+(nseg(object)-2)*pen.value(object)+sum(log(seg.len(object))))
				}else{
				  like=c(tmplike,tmplike+(nseg(object)-1)*pen.value(object))
				}
			}
		}
		else if(test.stat(object)=="Exponential"){
			if(cpttype(object)!="mean and variance"){
				stop("Unknown changepoint type for test.stat='Exponential', must be 'mean and variance'")
			}
			else{
			  warning("Not changed to be -2*logLik")
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
				  like=c(tmplike, tmplike+(nseg(object)-2)*pen.value(object)+sum(log(seg.len(object))))
				}else{
				  like=c(tmplike,tmplike+(nseg(object)-1)*pen.value(object))
				}
			}
		}
		else if(test.stat(object)=="Poisson"){
		  if(cpttype(object)!="mean and variance"){
		    stop("Unknown changepoint type for test.stat='Poisson', must be 'mean and variance'")
		  }
		  else{
		    warning("Not changed to be -2*logLik")
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
		      like=c(tmplike, tmplike+(nseg(object)-2)*pen.value(object)+sum(log(seg.len(object))))
		    }else{
		      like=c(tmplike,tmplike+(nseg(object)-1)*pen.value(object))
		    }
		  }
		}
		else{stop("logLik is only valid for distributional assumptions, not CUSUM or CSS")}
	  names(like)=c("-2*logLik","-2*Loglike+pen")
	  return(like)
	})

	setMethod("logLik", "cpt.range", function(object,ncpts=NA) {
	  warning("Not changed to be -2*logLik")
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
	        like=c(tmplike, tmplike+(nseg-2)*pen.value+sum(log(seg.len(object))))
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
	        like=c(tmplike, tmplike+(nseg-2)*pen.value+sum(log(seg.len(object))))
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
	        like=c(tmplike, tmplike+(nseg-2)*pen.value+sum(log(seg.len(object))))
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
	        like=c(tmplike, tmplike+(nseg-2)*pen.value+sum(log(seg.len(object))))
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
	        like=c(tmplike, tmplike+(nseg-2)*pen.value+sum(log(seg.len(object))))
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
	        like=c(tmplike, tmplike+(nseg-2)*pen.value+sum(log(seg.len(object))))
	      }else{
	        like=c(tmplike,tmplike+(nseg-1)*pen.value)
	      }
	      names(like)=c("-like","-likepen")
	    }
	  }
	  else{stop("logLik is only valid for distributional assumptions, not CUSUM or CSS")}
	  return(like)
	})

	setMethod("logLik", "cpt.reg", function(object) {
	  if(length(param.est(object))==0){# i.e. parameter.estimates=FALSE in call
	    cat('Calculating parameter estimates...')
	    object=param(object)
	    cat('done.\n')
	  }
	  if(test.stat(object)=="Normal"){
	    cpts=c(0,object@cpts)
	    seglen=seg.len(object)
	    data=data.set(object)
	    beta=param.est(object)$beta
	    sigmas=param.est(object)$sig2
	    rss=NULL
	    for(i in 1:length(seglen)){
	      rss[i]=sum((data[(cpts[i]+1):cpts[i+1],1]-data[(cpts[i]+1):cpts[i+1],-1,drop=FALSE]%*%beta[i,,drop=FALSE])^2)
	    }
      like=sum(seglen*log(2*pi*sigmas))+sum(rss/sigmas)
      if(pen.type(object)=="MBIC"){
        like=c(like, like+(nseg(object)-2)*pen.value(object)+sum(log(seg.len(object))))
      }else{
        like=c(like,like+(nseg(object)-1)*pen.value(object))
      }
	  }
	  else{stop("logLik is only valid for Normal distributional assumption.")}
	  return(like)
	})

	setGeneric("likelihood", function(object) standardGeneric("likelihood"))
	setMethod("likelihood", "cpt", function(object) {
		return(logLik(object))
	})

# acf functions
	setGeneric("acf", function(object,...) standardGeneric("acf"))
	setMethod("acf", "cpt", function(object,lag.max=NULL,...) {
    cpts=c(0,object@cpts)
    nseg=nseg(object)
    data=data.set(object)
    for(i in 1:nseg){
      stats::acf(data[(cpts[i]+1):cpts[i+1]],main=paste("Series part:",(cpts[i]+1),":",cpts[i+1]),...)
    }
	})

	setMethod("acf", "cpt.reg", function(object,lag.max=NULL,...) {
	  cpts=c(0,object@cpts)
	  nseg=nseg(object)
	  data=data.set(object)[,1]
	  for(i in 1:nseg){
	    stats::acf(data[(cpts[i]+1):cpts[i+1]],main=paste("Series part:",(cpts[i]+1),"-",cpts[i+1]),...)
	  }
	})
