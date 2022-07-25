context("cpt.mean tests")

set.seed(1) # Note: new data sets must be added at the end.
singmeandata <- c(rnorm(100, 0, 1), rnorm(100, 10, 1))
mulmeandata <- c(rnorm(100, 0, 1), rnorm(100, 10, 1), rnorm(100, 20, 1), rnorm(100, 50, 1))
nochangedata <- c(rnorm(200, 0, 1))

singvardata <- c(rnorm(100, 10, 1), rnorm(100, 10, 5))
mulvardata <- c(rnorm(100, 20, 10), rnorm(100, 20, 15), rnorm(100, 20, 20), rnorm(100, 20, 25))

singmeanvardata <- c(rnorm(50, 0, 1), rnorm(50, 3, 10))
mulmeanvardata <- c(rnorm(50, 0, 1), rnorm(50, 5, 3), rnorm(50, 10, 1), rnorm(50, 3, 10))
mulmeanvarexpdata <- c(rexp(50, 1), rexp(50, 3), rexp(50, 5), rexp(50, 7)) # rate values correct
mulmeanvarpoisdata <- c(rpois(50, 1), rpois(50, 2), rpois(50, 3), rpois(50, 5)) # lambda values correct?

constantdata <- rep(1, 200)
shortdata <- c(2)
negativedata <- jitter(rep(-100, 200))

characterdata <- rep("ert", 200)

# NAdata - creates 10 random NA within singmeandata
NAdata <- singmeandata
rn <- sample(1:length(singmeandata), 10, replace = F)

for (i in rn) {
  NAdata[i] <- NA
}
###################

data <- list(singmeandata, mulmeandata, nochangedata, constantdata, NAdata, shortdata, negativedata, characterdata)

# meandata <- list(singmeandata, mulmeandata, nochangedata)
# vardata <-  list(singvardata, mulvardata, nochangedata)
# meanvardata <-  list(singmeanvardata, mulmeanvardata, nochangedata)

methods <- c("AMOC", "PELT", "BinSeg") # might want to change code to convert to uppercase so less likely to break code

penalties <- c("None", "SIC", "BIC", "AIC", "Hannan-Quinn", "Asymptotic", "Manual", "MBIC") # , "CROPS")

asympenval <- list(1, 0.756, 0.234, "return", -1, 0) # need to add character string and -1 and 0
# manpenval <- list("2+log(n)", "log(n)", "3*n", -1, "diffparam-1") #null, alt, tau*2 don't work (comes back with user defind error "your manual cannot be evaluated")
manpenval <- list("-1")

QValues <- list(3, -1, "jamie", 200000) # data variable needs to be modified - larger than data length and over half data length
# QValues <- c(3, 5)

testStats <- c("Normal")
# asym and cusum return user defined "no asymptotic penalty" && "asymptotic penalties not implemented"

class <- c(TRUE, FALSE)
param.estimates <- c(TRUE, FALSE)

minseglen <- c(-1, 10, 20000)

cropspenval <- list(c(2, 2.5), c(3, 1), c(5, 5, 6), c("a", "b"), 5, "a")

t <- 0 # count for number of iterations

checkManualPenalty <- function(methodLog) {
  aQv <- Qv
  if (methodLog == TRUE) {
    aqV <- QValues[[v]]
  }

  for (npv in 1:length(manpenval)) {
    test_that(paste0("Test #", t, " :data=", d, "penalty=", penalties[p], ", method=", methods[m], ",class=", cl, ", param=", pe, ", penvalue=", manpenval[[npv]], ", test.stat=", testStats[ts]), {
      # x <- cpt.mean(data=data[[d]], penalty=penalties[p], pen.value=manpenval[[npv]], method=methods[m], Q=aQv, test.stat=testStats[ts], class=cl, param.estimates=pe)
      # browser()
      if (is.numeric(manpenval[[npv]]) == FALSE) {
        texttest <- try(eval(parse(text = paste(manpenval[[npv]]))), silent = TRUE)
        if (class(texttest) == "try-error") {
          expect_that(cpt.mean(data = data[[d]], penalty = penalties[p], pen.value = manpenval[[npv]], method = methods[m], Q = aQv, test.stat = testStats[ts], class = cl, param.estimates = pe), throws_error("Your manual penalty cannot be evaluated"))
          t <- t + 1
        }
      }
      if (manpenval[[npv]] < 0) {
        expect_that(cpt.mean(data = data[[d]], penalty = penalties[p], pen.value = manpenval[[npv]], method = methods[m], Q = aQv, test.stat = testStats[ts], class = cl, param.estimates = pe), throws_error("pen.value cannot be negative, please change your penalty value"))
      }
      expect_that(2 + 2, equals(4))
    })
    t <- t + 1
  }
}

checkAsymptoticPenalty <- function(methodLog) {
  aQv <- Qv
  if (methodLog == TRUE) {
    aqV <- QValues[[v]]
  }
  for (apv in 1:length(asympenval)) {


    # browser()
    test_that(paste0("Test #", t, " :data=", d, "penalty=", penalties[p], ", pen.value=", asympenval[[apv]], ", method=", methods[m], ",class=", cl, ", param=", pe, ", penvalue=", asympenval[[apv]], ", test.stat=", testStats[ts]), {
      if (is.numeric(asympenval[[apv]]) == FALSE) {
        expect_that(cpt.mean(data = data[[d]], penalty = penalties[p], pen.value = asympenval[[apv]], method = methods[m], Q = aQv, test.stat = testStats[ts], class = cl, param.estimates = pe), throws_error())
      } else if (asympenval[[apv]] <= 0 || asympenval[[apv]] > 1) {
        expect_that(cpt.mean(data = data[[d]], penalty = penalties[p], pen.value = asympenval[[apv]], method = methods[m], Q = aQv, test.stat = testStats[ts], class = cl, param.estimates = pe), throws_error("Asymptotic penalty values must be > 0 and <= 1"))
      } else if (methods[m] == "PELT" || methods[m] == "BinSeg") {
        expect_warning(cpt.mean(data = data[[d]], penalty = penalties[p], pen.value = asympenval[[apv]], method = methods[m], Q = aQv, test.stat = testStats[ts], class = cl, param.estimates = pe))
      } else {
        x <- cpt.mean(data = data[[d]], penalty = penalties[p], pen.value = asympenval[[apv]], method = methods[m], Q = aQv, test.stat = testStats[ts], class = cl, param.estimates = pe)
        expect_that(2 + 2, equals(4))
      }
    })
    t <- t + 1
  }
}

checkCROPS <- function() {
  # test pen.value + its length + missing
  # test the returns of the class

  if (methods[m] != "PELT") {
    expect_that(cpt.mean(data = data[[d]], method = methods[m], penalty = penalties[p], pen.value = cropspenval[[1]], test.stat = testStats[[ts]], class = cl, param.estimates = pe), throws_error('CROPS is a valid penalty choice only if method="PELT", please change your method or your penalty.'))
    t <- t + 1
  } else {
    for (cr in 1:length(cropspenval)) {
      test_that(paste0("Test #", t, "data:", d, " :penalty=", penalties[p], ", penval=", cropspenval[cr], ", method=", methods[m], ",class=", cl, ", param=", pe, ", test.stat=", testStats[ts]), {
        if (is.numeric(cropspenval[[cr]]) == FALSE) {
          expect_that(cpt.mean(data = data[[d]], method = methods[m], penalty = penalties[p], pen.value = cropspenval[[cr]], test.stat = testStats[[ts]], class = cl, param.estimates = pe), throws_error("For CROPS, pen.value must be supplied"))
        } else if (length(cropspenval[[cr]]) != 2) {
          expect_that(cpt.mean(data = data[[d]], method = methods[m], penalty = penalties[p], pen.value = cropspenval[[cr]], test.stat = testStats[[ts]], class = cl, param.estimates = pe), throws_error("The length of pen.value must be 2"))
        } else {
          #       expect_that(cpt.mean(data=data[[d]], penalty=penalties[p], class=cl, param.estimates=pe), throws_error('For CROPS, pen.value must be supplied'))

          if (testStats[[ts]] == "Normal" || testStats[[ts]] == "Gamma" || testStats[[ts]] == "Exponential" || testStats[[ts]] == "Poisson") {
            x <- cpt.mean(data = data[[d]], method = methods[m], penalty = penalties[p], pen.value = cropspenval[[cr]], test.stat = testStats[[ts]], class = cl, param.estimates = pe)


            if (cl == TRUE) {
              expect_that(x, is_a("cpt.range"))
            }
          } else {
            expect_that(cpt.mean(data = data[[d]], method = methods[m], penalty = penalties[p], pen.value = cropspenval[[cr]], test.stat = testStats[[ts]], class = cl, param.estimates = pe), throws_error("Only Normal, Exponential, Gamma and Poisson are valid test statistics"))
          }
        }
      })
      t <- t + 1
    }
    # numeric return 'For CROPS, pen.value must be supplied'
    # length return 'The length of pen.value must be 2'
  }
}

checkOtherPenalties <- function(methodLog) {
  aQv <- Qv
  if (methodLog == TRUE) {
    aqV <- QValues[[v]]
  }

  test_that(paste0("Test #", t, " :data=", d, "penalty=", penalties[p], ", method=", methods[m], ",class=", cl, ", param=", pe, ", test.stat=", testStats[ts]), {
    x <- cpt.mean(data = data[[d]], penalty = penalties[p], method = methods[m], Q = aQv, test.stat = testStats[ts], class = cl, param.estimates = pe)
    #                     if(length(data=data[[d]]) <= 2){
    #                       expect_that(cpt.mean(data[[d]], penalty=penalties[p], 0, method=methods[m], QValues[[v]], testStats[ts], class=cl, pe), throws_error())
    #                     }





    ### Returns properly####
    if (cl == TRUE) {
      if (methods[m] == "PELT" || methods[m] == "AMOC") {
        expect_that(x, is_a("cpt"))
      } else {
        expect_that(x, is_a("cpt.range"))
      }
      ###### change this based on methods
      #### check some values for the class
    } else if (cl == FALSE && methodLog == TRUE) {
      #  expect_that(x, is_a('list'))  #####this is now an integer in BINSEG.
    } else if (cl == FALSE && methodLog == FALSE) {
      if (methods[m] == "PELT") {
        expect_that(x, is_a("integer"))
      }
      #                       }else if(methods[m] == "AMOC"){
      #                         #if(testStats[ts] == "CUSUM"){
      #                           expect_that(x, is_a('numeric'))
      #                         #}else if(testStats[ts] != "CUSUM"){
      #                         #  expect_that(x, is_a('integer'))
      #                         #}
      #                       }
    }
    t <- t + 1
  })
}



for (d in 1:length(data)) {
  if (is.element(NA, data[[d]])) {
    test_that(paste0("Test #", t, " :data=", d), {
      expect_that(cpt.mean(data = data[[d]]), throws_error("Missing value: NA is not allowed in the data as changepoint methods assume regularly spaced data."))
      t <- t + 1
    })
  } else if (length(data[[d]]) < 2) {
    test_that(paste0("Test #", t, " :data=", d), {
      expect_that(cpt.mean(data = data[[d]]), throws_error())
      t <- t + 1
    })
  } else if (is.numeric(data[[d]]) == FALSE) {
    test_that(paste0("Test #", t, " :data=", d), {
      expect_that(cpt.mean(data = data[[d]]), throws_error("Only numeric data allowed"))
      t <- t + 1
    })
  } else {
    for (p in 1:length(penalties)) {
      for (m in 1:length(methods)) {
        for (ts in 1:length(testStats)) {
          for (cl in class) {
            for (pe in param.estimates) {
              Qv <- 5
              #                               if(t == 1112){
              #                               browser()
              #                               }
              #### data checks####
              if (penalties[p] == "CROPS") {
                checkCROPS()
              } else {
                # Q values only necessary when method is BINSEG
                if (methods[m] == "BinSeg") {
                  for (v in 1:length(QValues)) {
                    test_that(paste0("Test #", t, " :data=", d, "penalty=", penalties[p], ", method=", methods[m], ",class=", cl, ", param=", pe, ", test.stat=", testStats[ts], "QVal=", QValues[[v]]), {
                      if (is.numeric(QValues[[v]]) == FALSE) {
                        expect_that(cpt.mean(data = data[[d]], penalty = penalties[p], method = methods[m], Q = QValues[[v]], test.stat = testStats[ts], class = cl, param.estimates = pe), throws_error())
                      } else if (QValues[[v]] < 0) {
                        expect_that(cpt.mean(data = data[[d]], penalty = penalties[p], method = methods[m], Q = QValues[[v]], test.stat = testStats[ts], class = cl, param.estimates = pe), throws_error())
                      } else if (QValues[[v]] > (length(data[[d]])) / 2 + 1) {
                        expect_that(cpt.mean(data = data[[d]], penalty = penalties[p], method = methods[m], Q = QValues[[v]], test.stat = testStats[ts], class = cl, param.estimates = pe), throws_error())
                        # specific user defined error "Q is larger than the maximum number of segments"
                      }
                      t <- t + 1
                    })

                    if (penalties[p] == "Manual") {
                      checkManualPenalty(TRUE)
                    } else if (penalties[p] == "Asymptotic") {
                      checkAsymptoticPenalty(TRUE)
                    } else {
                      checkOtherPenalties(TRUE)
                    }
                  }
                } else {
                  # Normal and Asymptotic penalty pen values
                  if (penalties[p] == "Manual") {
                    checkManualPenalty(FALSE)
                  } else if (penalties[p] == "Asymptotic") {
                    checkAsymptoticPenalty(FALSE)
                  } else {
                    checkOtherPenalties(FALSE)
                  }
                }
              }
              t <- t + 1
            }
          }
        }
      }
    }
  }
}
