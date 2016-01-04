######## Function to run PELT for a Range Of Penalty values.  ############

range_of_penalties <- function(sumstat,cost = "mean.norm",PELT = T,min_pen=log(length(sumstat)/3-1),max_pen=10*log(length(sumstat)/3-1), shape = 1, minseglen) {
  
  NCALC=0
  pen_interval <- c(min_pen,max_pen)
  n = length(sumstat)/3 - 1
  
  test_penalties <- NULL
  numberofchangepoints <- NULL
  penal <- NULL
  overall_cost <- array() 
  segmentations <- NULL
  b_between <- array() 
  
  ##### Want to store and use Func, M and CP in PELT 
  
  anslastchangelike<- NULL
  ansnumchangecpts <- NULL
  anslastchangecpts <- NULL
  lastchangelike <- list(array(0,n+1),array(0,n+1))
  numchangecpts <- list(array(0,n+1),array(0,n+1))
  lastchangecpts <- list(array(0,n+1),array(0,n+1))
  
  count <- 0
  
  while (length(pen_interval) > 0){
    
    new_numcpts <- array()
    new_penalty <- array()
    new_cpts <- array() 
    
    for (b in 1:length(pen_interval)) {
      
      ans<- PELT(sumstat,pen=pen_interval[b], cost_func = cost , shape = shape, minseglen = minseglen,lastchangelike = lastchangelike[[b]],lastchangecpts = lastchangecpts[[b]], numchangecpts = numchangecpts[[b]])
      resultingcpts <- ans[[2]] 
      new_numcpts[b] <- length(resultingcpts)
      lastchangelike[[b]] <- ans[[3]]
      numchangecpts[[b]] <- ans[[4]]
      lastchangecpts[[b]] <- ans[[1]]
      cost.test <- array()
      new_cpts[b] <- list(resultingcpts[-length(resultingcpts)])
      new_penalty[b] <- ans[[3]][n+1]-(ans[[4]][n+1]-1)*pen_interval[b]
    }
    
    if (count == 0){
      print(paste("Maximum number of runs of algorithm = ", new_numcpts[1] - new_numcpts[2] + 2, sep = ""))
      count <- count + length(new_numcpts)
      print(paste("Completed runs = ", count, sep = ""))
    }
    
    else{
      count <- count + length(new_numcpts)
      print(paste("Completed runs = ", count, sep = ""))
    }
    
    
    ## Add the values calculated to the already stored values 
    test_penalties <- unique((sort(c(test_penalties,pen_interval))))
    new_numcpts <- c(numberofchangepoints,new_numcpts)
    new_penalty <- c(penal,new_penalty)
    
    new_cpts <- c(segmentations,new_cpts)
    numberofchangepoints <- -sort(-new_numcpts) ##can use sort to re-order 
    penal <- sort(new_penalty)
    
    anslastchangelike <- c(anslastchangelike, lastchangelike)
    ansnumchangecpts <- c( ansnumchangecpts, numchangecpts)
    anslastchangecpts <- c( anslastchangecpts, lastchangecpts)
    
    ls <- array()
    
    for (l in 1:length(new_cpts)){
      #  ls[l] <- length(new_cpts[[l]])
      ls[l] <- -anslastchangelike[[l]][n+1]
      
    } 
    
    
    ls1 <- sort(ls,index.return = T, decreasing = T)
    ls1 <- ls1$ix
    
    
    segmentations <- new_cpts[c(ls1)]
    anslastchangelike <- anslastchangelike[c(ls1)]
    ansnumchangecpts <-  ansnumchangecpts[c(ls1)]
    anslastchangecpts <-  anslastchangecpts[c(ls1)]
    
    pen_interval <- NULL
    tmppen_interval <- NULL
    lastchangelike <-   NULL
    numchangecpts <-   NULL
    lastchangecpts <-  NULL
    
    for (i in 1:(length(test_penalties)-1)){
      if(abs(numberofchangepoints[i]-numberofchangepoints[i+1])>1){ ##only need to add a beta if difference in cpts>1
        j <- i+1
        tmppen_interval <- (penal[j] - penal[i]) * (((numberofchangepoints[i]) - (numberofchangepoints[j]))^-1)
        pen_interval <- c(pen_interval, tmppen_interval )
        
        tmpnumchangecpts <- array(0,n+1)
        tmplastchangecpts <- array(0,n+1)
        tmplastchangelike <- array(0,n+1)
        
        
        #index=((1:(length(data) + 1))[ansnumchangecpts[[i]]== ansnumchangecpts[[j]]] && ansnumchangecpts[[i]] > 0)
        index=(1:(n + 1))[ansnumchangecpts[[i]]== ansnumchangecpts[[j]]]
        #  browser()
        if (length(index) > 0){
          tmpnumchangecpts[index] <-  ansnumchangecpts[[i]][index]
          tmplastchangecpts[index] <- anslastchangecpts[[i]][index]
          tmplastchangelike[index] <- anslastchangelike[[i]][index] + (tmppen_interval - test_penalties[i]) * (ansnumchangecpts[[i]][index]-1)
          
        }
        
        
        # index=(1:(length(data) + 1))[ansnumchangecpts[[i]]== ansnumchangecpts[[j]]+1 && (ansnumchangecpts[[i]] > 0 || ansnumchangecpts[[j]] > 0)]
        index=(1:(n + 1))[ansnumchangecpts[[i]]== ansnumchangecpts[[j]]+1 ]
        if(length(index)>0){
          a= anslastchangelike[[i]][index] +  (tmppen_interval - test_penalties[i]) * (ansnumchangecpts[[i]][index]-1)
          b= anslastchangelike[[j]][index] +  (tmppen_interval - test_penalties[j]) * (ansnumchangecpts[[j]][index]-1)
          
          tmplastchangelike[index]=pmin(a,b)
          tmpnumchangecpts[index]= (ansnumchangecpts[[i]][index])*(a<b)+ (ansnumchangecpts[[j]][index])*(a>=b)
          tmplastchangecpts[index]= anslastchangecpts[[i]][index]*(a<b)+ anslastchangecpts[[j]][index]*(a>=b)
        }
        
        numchangecpts <- c(numchangecpts,list(tmpnumchangecpts))
        lastchangelike <- c(lastchangelike,list(tmplastchangelike))
        lastchangecpts <- c(lastchangecpts, list(tmplastchangecpts))
      }
    }
    
    
    if(length(pen_interval)>0){
      for(k in length(pen_interval):1){ 
        if(min(abs(pen_interval[k]-test_penalties))<1e-8) {
          numchangecpts[[k]]= NULL
          lastchangelike[[k]]= NULL
          lastchangecpts[[k]]= NULL
          pen_interval=pen_interval[-k]
        }
      }
      
    }
  }
  
  
  ##PRUNE VALUES WITH SAME num_cp
  for(j in length(test_penalties):2){
    if(numberofchangepoints[j]==numberofchangepoints[j-1]){
      numberofchangepoints=numberofchangepoints[-j]
      test_penalties=test_penalties[-j]
      penal=penal[-j]
      segmentations = segmentations[-j]
      ansnumchangecpts =  ansnumchangecpts[-j]
      anslastchangelike = anslastchangelike[-j]
      anslastchangecpts =  anslastchangecpts[-j]
    }
  }
  
  
  
  ###calculate beta intervals
  nb=length(test_penalties)
  beta.int=rep(0,nb)
  beta.e=rep(0,nb)
  for(k in 1:nb){
    if(k==1){
      beta.int[1]=test_penalties[1]
    }else{
      beta.int[k]=beta.e[k-1]   
    }
    if(k==nb){
      beta.e[k]=test_penalties[k]
    }else{
      beta.e[k]=(penal[k]-penal[k+1])/(numberofchangepoints[k+1]-numberofchangepoints[k])
    }
    
  }
  
  return(list(cpt.out = rbind(beta_interval = beta.int,numberofchangepoints,penalised_cost = penal),changepoints = segmentations))
  #segmentations is output matrix
  #beta.int
}