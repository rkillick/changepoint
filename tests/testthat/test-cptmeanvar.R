context("cpt.meanvar tests")

set.seed(1) # Note: new data sets must be added at the end.
singmeandata <- c(rnorm(100,0,1),rnorm(100,10,1))
mulmeandata <- c(rnorm(100,0,1),rnorm(100,10,1),rnorm(100,20,1),rnorm(100,50,1))
nochangedata <- c(rnorm(200,0,1))

singvardata <- c(rnorm(100,10,1),rnorm(100,10,5))
mulvardata <- c(rnorm(100,20,10),rnorm(100,20,15),rnorm(100,20,20),rnorm(100,20,25))

singmeanvardata <- c(rnorm(50,0,1),rnorm(50,3,10))
mulmeanvardata <- c(rnorm(50,0,1),rnorm(50,5,3),rnorm(50,10,1),rnorm(50,3,10))
mulmeanvarexpdata <- c(rexp(50,1), rexp(50,3), rexp(50,5), rexp(50,7)) #rate values correct
mulmeanvarpoisdata <- c(rpois(50,10), rpois(50,20), rpois(50,15), rpois(50,25)) #lambda values correct?

constantdata <- rep(1, 200)
shortdata <- c(2,4)
negativedata <- jitter(rep(-100, 200) )

#NAdata - creates 10 random NA within singmeandata
NAdata <- singmeanvardata
rn <- sample(1:length(singmeanvardata), 10, replace=F)

for(i in rn){
  NAdata[i] <- NA
}
###################

data <- list(singmeanvardata,mulmeanvardata, mulmeanvarexpdata, mulmeanvarpoisdata, nochangedata, constantdata, NAdata, shortdata, negativedata)

# meandata <- list(singmeandata, mulmeandata, nochangedata)
# vardata <-  list(singvardata, mulvardata, nochangedata)
# meanvardata <-  list(singmeanvardata, mulmeanvardata, nochangedata)

methods <- c("AMOC", "PELT", "BinSeg") #might want to change code to convert to uppercase so less likely to break code
#methods <- c("AMOC")

penalties <- c("None", "SIC", "BIC", "AIC", "Hannan-Quinn", "Asymptotic", "Manual", "MBIC")

asympenval <- list(1, 0.756, 0.234, 'return', -1, 0) #need to add character string and -1 and 0
#manpenval <- list("2+log(n)", "log(n)", "3*n", -1, "diffparam-1") 
manpenval <- list(1)

QValues <- list(3, -1, 'jamie', 200000) 
#QValues <- c(3, 5)

testStats <- c("Normal", "Gamma", "Exponential", "Poisson") 

class <- c(TRUE, FALSE)
param.estimates <- c(TRUE, FALSE)

shap <- c(1)

cropspenval = list(c(2,2.5), c(3,1), c(5,5,6), c("a", "b"), 5, "a")

t = 0 #count for number of iterations

checkManualPenalty <- function(methodLog){
  aQv <- Qv
  if(methodLog == TRUE){
    aqV <- QValues[[v]]
  }
  
  for(npv in 1:length(manpenval)){
    
    test_that(paste0("Test #",t," :data=",d,"penalty=",penalties[p],", method=",methods[m],",class=",cl,", param=",pe,", penvalue=",manpenval[npv],", test.stat=",testStats[ts]) ,{
      x <- cpt.meanvar(data=data[[d]], penalty=penalties[p], pen.value=manpenval[[npv]], method=methods[m], Q=aQv, test.stat=testStats[ts], class=cl, param.estimates=pe, shape=shap[sh])
      # browser()
      if(is.numeric(manpenval[[npv]]) == FALSE){
        texttest = try(eval(parse(text=paste(manpenval[[npv]]))),silent=TRUE)
        if(class(texttest)=='try-error'){
          expect_that(cpt.meanvar(data=data[[d]], penalty=penalties[p], pen.value=manpenval[[npv]], method=methods[m], Q=aQv, test.stat=testStats[ts], class=cl, param.estimates=pe, shape=shap[sh]), throws_error("Your manual penalty cannot be evaluated"))
          #currently failing on text that says can be used in the help file.
        }
      }
      #       if(manpenval[[npv]] < 0){
      #         expect_that(cpt.meanvar(data=data[[d]], penalty=penalties[p], pen.value=manpenval[[npv]], method=methods[m], Q=aQv, test.stat=testStats[ts], class=cl, param.estimates=pe, shape=shap[sh]), throws_error())
      #         #failing test (is this correct that must be +ve?)
      #       }
      #      expect_that(2+2, equals(4))
      
    })
    t = t+1
  }
  
}

checkAsymptoticPenalty <- function(methodLog){
  aQv <- Qv
  if(methodLog == TRUE){
    aqV <- QValues[[v]]
  }
  for(apv in 1:length(asympenval)){
    
    # browser()
    test_that(paste0("Test #",t," : data=",d,"penalty=",penalties[p],", method=",methods[m],",class=",cl,", param=",pe,", penvalue=",asympenval[apv],", test.stat=",testStats[ts]), {
      
      if(is.numeric(asympenval[[apv]]) == FALSE){
        expect_that(cpt.meanvar(data=data[[d]], penalty=penalties[p], pen.value=asympenval[[apv]], method=methods[m], Q=aQv, test.stat=testStats[ts], class=cl, param.estimates=pe, shape=shap[sh]), throws_error())
        #not user friendly error "Error in 1 - alpha : non-numeric argument to binary operator"
      }else if(asympenval[[apv]] <= 0 || asympenval[[apv]] > 1){
        
        expect_that(cpt.meanvar(data=data[[d]], penalty=penalties[p], pen.value=asympenval[[apv]], method=methods[m], Q=aQv, test.stat=testStats[ts], class=cl, param.estimates=pe, shape=shap[sh]), throws_error('Asymptotic penalty values must be > 0 and <= 1'))
        #NA Error?
        
      }
      else if(testStats[ts]=="Gamma"){
        expect_error(cpt.meanvar(data=data[[d]], penalty=penalties[p], pen.value=asympenval[[apv]], method=methods[m], Q=aQv, test.stat=testStats[ts], class=cl, param.estimates=pe, shape=shap[sh]),'Asymptotic penalties for the Gamma test statistic are not defined, please choose an alternative penalty type')
      }
      else if(methods[m] == "PELT" || methods[m] == "BinSeg"){
        expect_warning(cpt.meanvar(data=data[[d]], penalty=penalties[p], pen.value=asympenval[[apv]], method=methods[m], Q=aQv, test.stat=testStats[ts], class=cl, param.estimates=pe, shape=shap[sh])) 
      }
      else{
        x <- cpt.meanvar(data=data[[d]], penalty=penalties[p], pen.value=asympenval[[apv]], method=methods[m], Q=aQv, test.stat=testStats[ts], class=cl, param.estimates=pe, shape=shap[sh])
        expect_that(2+2, equals(4))
      }
    })  
    t = t+1
  }
}

checkOtherPenalties <- function(methodLog){
  aQv <- Qv
  if(methodLog == TRUE){
    aqV <- QValues[[v]]
  }
  
  test_that(paste0("Test #",t," :data=",d,"penalty=",penalties[p],", method=",methods[m],",class=",cl,", param=",pe,", test.stat=",testStats[ts],"data=", data[d]), {
    x <- cpt.meanvar(data=data[[d]], penalty=penalties[p], pen.value=0,shape=shap[sh], method=methods[m], Q=aQv, test.stat=testStats[ts], class=cl, param.estimates=pe)
    #                     if(length(data=data[[d]]) <= 2){
    #                       expect_that(cpt.meanvar(data[[d]], penalty=penalties[p], 0, method=methods[m], QValues[[v]], testStats[ts], class=cl, pe), throws_error())
    #                     }
    
    
    
    
    
    ###Returns properly####
    if(cl == TRUE){
      expect_that(x, is_a('cpt'))
    }else if(cl == FALSE && methodLog == TRUE){
      #expect_that(x, is_a('list'))#
    }else if(cl == FALSE && methodLog == FALSE){
      if(methods[m] == "PELT"){
        #     expect_that(x, is_a('integer'))
      }
      #                       }else if(methods[m] == "AMOC"){
      #                         #if(testStats[ts] == "CUSUM"){
      #                           expect_that(x, is_a('numeric'))    
      #                         #}else if(testStats[ts] != "CUSUM"){
      #                         #  expect_that(x, is_a('integer'))
      #                         #}
      #                       }
      
    }
    
  })
  t = t + 1
}

checkCROPS <- function(){
  #test pen.value + its length + missing
  #test the returns of the class
  
  if(methods[m]!="PELT"){
    expect_that(cpt.meanvar(data=data[[d]], method=methods[m],penalty=penalties[p], pen.value=cropspenval[[1]], test.stat=testStats[[ts]], class=cl, param.estimates=pe), throws_error('CROPS is a valid penalty choice only if method="PELT", please change your method or your penalty.'))    
    t=t+1
  }
  else{
    for(cr in 1:length(cropspenval)){
      test_that(paste0("Test #",t," :data=",d,"penalty=",penalties[p],", penval=",cropspenval[cr],", method=",methods[m],",class=",cl,", param=",pe,", test.stat=",testStats[ts]), {      
        
        if(is.numeric(cropspenval[[cr]]) == FALSE ){
          expect_that(cpt.meanvar(data=data[[d]], method=methods[m],penalty=penalties[p], pen.value=cropspenval[[cr]], test.stat=testStats[[ts]], class=cl, param.estimates=pe), throws_error('For CROPS, pen.value must be supplied'))
        }else if(length(cropspenval[[cr]]) != 2 ){
          expect_that(cpt.meanvar(data=data[[d]], method=methods[m],penalty=penalties[p], pen.value=cropspenval[[cr]], test.stat=testStats[[ts]],class=cl, param.estimates=pe), throws_error('The length of pen.value must be 2'))
          
          
        }else{
          #       expect_that(cpt.mean(data=data[[d]], penalty=penalties[p], class=cl, param.estimates=pe), throws_error('For CROPS, pen.value must be supplied'))
          
          if(testStats[[ts]] == "Normal" || testStats[[ts]] == "Gamma" || testStats[[ts]] == "Exponential" || testStats[[ts]] == "Poisson" ){
            x <- cpt.meanvar(data=data[[d]], method=methods[m],penalty=penalties[p], pen.value=cropspenval[[cr]], test.stat=testStats[[ts]], class=cl, param.estimates=pe)
            
            
            if(cl == TRUE){
              expect_that(x, is_a('cpt.range'))
            }
            
            
          }else{
            expect_that(cpt.meanvar(data=data[[d]], method=methods[m],penalty=penalties[p], pen.value=cropspenval[[cr]], test.stat=testStats[[ts]], class=cl, param.estimates=pe), throws_error('Only Normal, Exponential, Gamma and Poisson are valid test statistics'))   
          }
        }
        t=t+1
      })
      
      
    }
    #numeric return 'For CROPS, pen.value must be supplied'
    #length return 'The length of pen.value must be 2'    
  }
}


for(d in 1:length(data)){
  if(is.element(NA, data[[d]])){
    test_that(paste0("Test #",t," :data=",d,"penalty=",penalties[p],", method=",methods[m],",class=",cl,", param=",pe,", test.stat=",testStats[ts]), {
      
      expect_that(cpt.meanvar(data=data[[d]]),throws_error('Missing value: NA is not allowed in the data as changepoint methods assume regularly spaced data.'))
      #not user friendly error : Error in if (teststat >= pen.value) { : 
      #       missing value where TRUE/FALSE needed
      #       In addition: Warning message:
      #         In min(tmp, na.rm = T) : no non-missing arguments to min; returning Inf
      
      t = t + 1
      
    })
    
  }else if(length(data[[d]]) <= 2){
    test_that(paste0("Test #",t," :data=",d,"penalty=",penalties[p],", method=",methods[m],",class=",cl,", param=",pe,", test.stat=",testStats[ts]), {
      
      expect_that(cpt.meanvar(data=data[[d]]),throws_error("Data must have atleast"))
      t = t + 1
      
    }) 
  }else if(is.numeric(data[[d]]) == FALSE){
    test_that(paste0("Test #",t," :data=",d,"penalty=",penalties[p],", method=",methods[m],",class=",cl,", param=",pe,", test.stat=",testStats[ts]), {
      
      expect_that(cpt.meanvar(data=data[[d]]),throws_error('Only numeric data allowed'))
      t = t + 1
      
    }) 
  }
  else{
    for(p in 1:length(penalties)){
      for(m in 1:length(methods)){
        for(ts in 1:length(testStats)){
          for(cl in class){
            for(pe in param.estimates){ 
              for(sh in 1:length(shap)){
                Qv = 5
                
                if(penalties[p] == "CROPS"){
                  checkCROPS()
                }else{
                  if(testStats[ts] == "Gamma" ){
                    if( (sum(data[[d]]<0)) >0){
                      test_that(paste0("Test #",t," :data=",d,"penalty=",penalties[p],", method=",methods[m],",class=",cl,", param=",pe,", test.stat=",testStats[ts],", data=", data[d]), {
                        
                        expect_that(cpt.meanvar(data=data[[d]], penalty=penalties[p], method=methods[m], Q=Qv, test.stat=testStats[ts], class=cl, param.estimates=pe, shape=shap[sh]),throws_error("Gamma test statistic requires positive data"))
                        t = t + 1
                        
                      }) 
                    }else if(penalties[p] == "Asymptotic"){
                      checkAsymptoticPenalty(FALSE)
                    }
                  }else if(testStats[ts] == "Exponential" && (sum(data[[d]]<0)) >0){
                    ##THROWING ERROR on poisson data with exponential??
                    
                    test_that(paste0("Test #",t," :data=",d,"penalty=",penalties[p],", method=",methods[m],",class=",cl,", param=",pe,", test.stat=",testStats[ts]), {
                      
                      expect_that(cpt.meanvar(data=data[[d]], method=methods[m], Q=Qv, test.stat=testStats[ts], class=cl, param.estimates=pe, shape=shap[sh]),throws_error("Exponential test statistic requires positive data"))
                      t = t + 1
                      
                    }) 
                  }else if(testStats[ts] == "Poisson" ){
                    if ((sum(data[[d]]<0)) >0){
                      test_that(paste0("Test #",t," :data=",d,"penalty=",penalties[p],", method=",methods[m],",class=",cl,", param=",pe,", test.stat=",testStats[ts],"data=",data[d]), {
                        
                        expect_that(cpt.meanvar(data=data[[d]], penalty="SIC", method=methods[m], Q=Qv, test.stat=testStats[ts], class=cl, param.estimates=pe, shape=shap[sh]),throws_error("Poisson test statistic requires positive data"))
                        t = t + 1
                        
                      }) 
                    }else if(is.integer(data[[d]]) == FALSE && d != 6){
                      ## NOT FAILING ON CONSTANT DATA but is numeric type
                      test_that(paste0("Test #",t," :data=",d,"penalty=",penalties[p],", method=",methods[m],",class=",cl,", param=",pe,", test.stat=",testStats[ts], "data=", data[d]), {
                        expect_that(cpt.meanvar(data=data[[d]], penalty="SIC", method=methods[m], Q=Qv, test.stat=testStats[ts], class=cl, param.estimates=pe, shape=shap[sh]),throws_error("Poisson test statistic requires integer data"))
                      })
                    }
                    
                  }else{
                    #Q values only necessary when method is BINSEG
                    if(methods[m] == "BinSeg"){
                      for(v in 1:length(QValues)){
                        #causing a slight problem with when CSS and Asymptotic as cpt.meanvar throws wrong error
                        #if statement to get around it? if(teststat=CSS and pen=asymptotic)? loses tests?
                        test_that(paste0("Test #",t," :data=",d,"penalty=",penalties[p],", method=",methods[m],",class=",cl,", param=",pe,", test.stat=",testStats[ts],"QVal=",QValues[[v]]), {
                          
                          if(is.numeric(QValues[[v]]) == FALSE){
                            expect_that(cpt.meanvar(data=data[[d]], method=methods[m], Q=QValues[[v]], test.stat=testStats[ts], class=cl, param.estimates=pe, shape=shap[sh]), throws_error())
                            #error is not user friendly
                          }else if(QValues[[v]] < 0){
                            expect_that(cpt.meanvar(data=data[[d]], method=methods[m], Q=QValues[[v]], test.stat=testStats[ts], class=cl, param.estimates=pe, shape=shap[sh]), throws_error())
                            #error is not user friendly
                          }else if(QValues[[v]] > (length(data[[d]])/2+1)){
                            expect_that(cpt.meanvar(data=data[[d]], method=methods[m], Q=QValues[[v]], test.stat=testStats[ts], class=cl, param.estimates=pe, shape=shap[sh]), throws_error("Q is larger than the maximum number of segments"))
                            #specific user deined error "Q is larger than the maximum number of segments"
                          }
                          t = t + 1
                        })
                        
                        if(penalties[p] == "Manual" ){
                          checkManualPenalty(TRUE)
                        }else if(penalties[p] == "Asymptotic"){
                            checkAsymptoticPenalty(TRUE)
                        }else{
                          checkOtherPenalties(TRUE)
                        }  
                      } 
                    }else{
                      #Normal and Asymptotic penalty pen values
                      
                      if(penalties[p] == "Manual" ){
                        checkManualPenalty(FALSE)
                      }else if(penalties[p] == "Asymptotic"){
                          checkAsymptoticPenalty(FALSE)
                      }else{
                        checkOtherPenalties(FALSE)
                      }
                      
                    }
                  }
                  
                }
                t=t+1
              }
            }
            
          }
        }
      }
    }
  }
} 