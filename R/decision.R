decision<-function(tau,null,alt=NA,penalty="MBIC",n=0,diffparam=1,pen.value=0){
  if(sum(is.na(alt))){
		if(length(tau)!=length(null)){
			stop("Lengths of tau and null do not match")
		}
	}
	else{
		if((length(tau)!=length(null))||(length(tau)!=length(alt))){
			stop("Lengths of tau, null and alt do not match")
		}
	}
	single.decision=function(tau,null,alt,n=0,diffparam=1,pen.value=0){
		if(is.na(alt)){teststat=null}
		else{teststat=null-alt}
		if(teststat>=pen.value){return(tau)}
		else{return(n)}
	}
	if(length(tau)==1){
		out=single.decision(tau,null,alt,n,diffparam,pen.value)
		names(out)="cpt"
		return(list(cpt=out,pen=pen.value))
	}
	else{
		rep=length(tau)
		out=NULL
		for(i in 1:rep){
			out[i]=single.decision(tau[i],null[i],alt[i],n,diffparam,pen.value)
		}
		names(out)=rep("cpt",rep)
		return(list(cpt=out,pen=pen.value))
	}
}
