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