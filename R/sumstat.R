SumStats <- function(test.stat, data, size, mu=NA){
  #output: matrix nrow = length(data) & ncol = 3
  if(grepl("binomial",tolower(test.stat))){
    #Col 1: sum of data
    #Col 2: sum of size
    #Col 3: BLANK (set to zero)
    y = c(0, cumsum(coredata(data)))
    if(length(data) == length(size)){
      m = c(0, cumsum(coredata(size)))
    }else if(length(size)==1){
      m = (0:length(data))*coredata(size)
    }else{
      stop("Dimenstion of size is not as expected.")
    }
    blank = rep(0,length(data)+1)
    sumstat <- cbind(y,m,blank)
  }else{
    ##Required for Normal, Exponential, Gamma and Poisson
    #col 1: sum of data
    #col 2: sum of squared data
    #col 3: sum of squared deviations from the sample mean (overall?)
    if(is.na(mu)) mu = mean(data)
    y = c(0,cumsum(coredata(data)))
    y2 = c(0, cumsum(coredata(data)^2))
    rss = c(0,cumsum((coredata(data)-mu)^2))
    sumstat <- cbind(y,y2,rss)
  }
  return(sumstat)
}