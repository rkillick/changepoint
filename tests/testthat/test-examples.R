context("man file example tests")

# From changepoint-package.Rd
# change in variance
set.seed(1)
x = c(rnorm(100, 0, 1), rnorm(100, 0, 10))
ansvar = cpt.var(x)
test_that("var1", expect_identical(cpts(ansvar), 100))

# change in mean
set.seed(1)
y = c(rnorm(100, 0, 1), rnorm(100, 5, 1))
ansmean = cpt.mean(y)
test_that("mean1", expect_identical(cpts(ansmean), 100))

# change in mean and variance
set.seed(1)
z = c(rnorm(100, 0, 1), rnorm(100, 2, 10))
ansmeanvar = cpt.meanvar(z)
test_that("meanvar1", expect_identical(cpts(ansmeanvar), 100))







# From cpt.mean.Rd
# Example of a change in mean at 100 in simulated normal data
set.seed(1)
x = c(rnorm(100, 0, 1), rnorm(100, 10, 1))
test_that("mean2", expect_equivalent(cpt.mean(x, penalty = "SIC", method = "AMOC", class = FALSE), c(100, 1)))
ans = cpt.mean(x, penalty = "Asymptotic", pen.value = 0.01, method = "AMOC")
test_that("mean3", expect_identical(cpts(ans), 100))

# Example of multiple changes in mean at 50,100,150 in simulated normal data
set.seed(1)
x = c(rnorm(50, 0, 1), rnorm(50, 5, 1), rnorm(50, 10, 1), rnorm(50, 3, 1))
test_that("mean5", expect_identical(cpt.mean(x, penalty = "Manual", pen.value = "2*log(n)", method = "BinSeg", Q = 5, class = FALSE), c(50, 100, 150, 200)))

# Example of using the CROPS penalty in data set above
set.seed(1)
x = c(rnorm(50, 0, 1), rnorm(50, 5, 1), rnorm(50, 10, 1), rnorm(50, 3, 1))
out = cpt.mean(x, pen.value = c(4, 1500), penalty = "CROPS", method = "PELT")
truth = matrix(NA, ncol = 7, nrow = 7)
truth[1:6, 1] = 50
truth[1:5, 2] = c(96, 96, 100, 100, 150)
truth[1:4, 3] = c(100, 100, 133, 150)
truth[1:3, 4] = c(133, 133, 150)
truth[1:2, 5] = c(150, 150)
truth[1, 6] = 159
truth[1, 7] = 180
test_that("crops1", expect_equivalent(cpts.full(out), truth))
truth = c(4.000000, 4.332496, 4.385247, 4.684254, 559.366988, 646.962719, 1311.335695)
test_that("crops2", expect_equal(pen.value.full(out), truth, tolerance = 1e-6))

# Example multiple datasets where the first row has multiple changes in mean and the second row has
# no change in mean
set.seed(1)
x = c(rnorm(50, 0, 1), rnorm(50, 5, 1), rnorm(50, 10, 1), rnorm(50, 3, 1))
y = rnorm(200, 0, 1)
z = rbind(x, y)
test_that("mean6", expect_equal(cpt.mean(z, penalty = "Asymptotic", pen.value = 0.01, method = "PELT", Q = 5, class = FALSE), list(c(50, 100, 150, 200), 200)))







# From cpt.meanvar.Rd
# Example of a change in scale parameter (mean and variance) at 100 in simulated gamma data
set.seed(1)
x = c(rgamma(100, shape = 1, rate = 1), rgamma(100, shape = 1, rate = 5))
test_that("meanvar2", expect_equivalent(cpt.meanvar(x, penalty = "SIC", method = "AMOC", test.stat = "Gamma", class = FALSE, shape = 1), 98))
ans = cpt.meanvar(x, penalty = "AIC", method = "AMOC", test.stat = "Gamma", shape = 1)
test_that("meanvar3", expect_equivalent(cpts(ans), 98))

# Example of multiple changes in mean and variance at 50,100,150 in simulated normal data
set.seed(1)
x = c(rnorm(50, 0, 1), rnorm(50, 5, 3), rnorm(50, 10, 1), rnorm(50, 3, 10))
test_that("meanvar4", expect_equal(cpt.meanvar(x, penalty = "Manual", pen.value = "4*log(n)", method = "BinSeg", Q = 5, class = FALSE), c(50, 100, 150, 152, 200)))

# Example of using the CROPS penalty in the above example
set.seed(1)
x = c(rnorm(50, 0, 1), rnorm(50, 5, 3), rnorm(50, 10, 1), rnorm(50, 3, 10))
out = cpt.meanvar(x, pen.value = c(2 * log(length(x)), 100 * log(length(x))), penalty = "CROPS", method = "PELT")
truth = matrix(NA, ncol = 9, nrow = 6)
truth[1:5, 1] = c(rep(15, 2), rep(50, 3))
truth[1:4, 2] = c(17, 17, 100, 100)
truth[1:4, 3] = c(22, 22, 133, 150)
truth[1:3, 4] = c(44, 50, 151)
truth[1:2, 5] = c(46, 100)
truth[1:2, 6] = c(50, 133)
truth[1:2, 7] = c(100, 151)
truth[1, 8] = 133
truth[1, 9] = 151
test_that("crops3", expect_equal(cpts.full(out), truth))
truth = c(10.59663, 10.68431, 11.31088, 11.38307, 119.78669, 191.42622)
test_that("crops4", expect_equal(pen.value.full(out), truth, tolerance = 1e-6))

# Example multiple datasets where the first row has multiple changes in mean and variance and the
# second row has no change in mean or variance
set.seed(1)
x = c(rnorm(50, 0, 1), rnorm(50, 5, 3), rnorm(50, 10, 1), rnorm(50, 3, 10))
y = rnorm(200, 0, 1)
z = rbind(x, y)
test_that("meanvar5", expect_equivalent(cpt.meanvar(z, penalty = "Asymptotic", pen.value = 0.01, method = "PELT", Q = 5, class = FALSE), list(c(50, 100, 150, 200), 200)))








# From cpt.range-class.Rd
x = new("cpt.range")
test_that("class1", expect_is(x, "cpt.range"))
cpts(x) = c(10, 50, 100)
test_that("class2", expect_equivalent(cpts(x), c(10, 50, 100)))

# Example of multiple changes in variance at 50,100,150 in simulated normal data
set.seed(1)
x = c(rnorm(50, 0, 1), rnorm(50, 0, 10), rnorm(50, 0, 5), rnorm(50, 0, 1))
out = cpt.var(x, pen.value = c(log(length(x)), 10 * log(length(x))), penalty = "CROPS", method = "PELT")
test_that("class3", expect_equivalent(logLik(out, ncpts = 3), c(925.8085, 947.0578))) # raw likelihood of the data with changepoints, second value is likelihood + penalty








# From cpt.reg-class.Rd
# x=new("cpt.reg") # creates a new object with the cpt.reg class defaults
# test_that('class4',expect_is(x,"cpt.reg"))
# test_that('class5',expect_is(data.set(x),"matrix"))
# data.set(x)<-matrix(1:10,nrow=5,ncol=2) # replaces the data.set slot from x with a matrix
# test_that('class6',expect_equivalent(data.set(x),matrix(1:10,nrow=5,ncol=2)))







# From cpt.var.Rd
# Example of a change in variance at 100 in simulated normal data
set.seed(1)
x = c(rnorm(100, 0, 1), rnorm(100, 0, 10))
test_that("var2", expect_equivalent(cpt.var(x, penalty = "SIC", method = "AMOC", class = FALSE), c(100, 1)))
ans = cpt.var(x, penalty = "Asymptotic", pen.value = 0.01, method = "AMOC")
test_that("var3", expect_equivalent(cpts(ans), 100))

# Example of multiple changes in variance at 50,100,150 using CROPS
set.seed(1)
x = c(rnorm(50, 0, 1), rnorm(50, 0, 10), rnorm(50, 0, 5), rnorm(50, 0, 1))
out = cpt.var(x, pen.value = c(log(length(x)), 100 * log(length(x))), penalty = "CROPS", method = "PELT")
truth = matrix(NA, ncol = 7, nrow = 7)
truth[1:6, 1] = 50
truth[1:5, 2] = c(77, rep(99, 3), 150)
truth[1:4, 3] = c(79, 114, 140, 150)
truth[1:3, 4] = c(99, 133, 150)
truth[1:2, 5] = c(114, 150)
truth[1, 6] = 133
truth[1, 7] = 150
test_that("var5", expect_equivalent(cpts.full(out), truth))
truth = c(5.298317, 5.548538, 6.149305, 7.083099, 26.592259, 142.417161, 145.146279)
test_that("var6", expect_equivalent(pen.value.full(out), truth))

# Example multiple datasets where the first row has multiple changes in variance and the second row
# has no change in variance
set.seed(10)
x = c(rnorm(50, 0, 1), rnorm(50, 0, 10), rnorm(50, 0, 5), rnorm(50, 0, 1))
y = rnorm(200, 0, 1)
z = rbind(x, y)
truth = list()
truth[[1]] = c(50, 100, 149, 200)
truth[[2]] = 200
test_that("var7", expect_equivalent(cpt.var(z, penalty = "Asymptotic", pen.value = 0.01, method = "PELT", Q = 5, class = FALSE), truth))






# From cpt-class.Rd
x = new("cpt") # creates a new object with the cpt class defaults
test_that("class7", expect_is(x, "cpt"))
test_that("class8", expect_equivalent(cpts(x), numeric()))
cpts(x) = c(10, 50, 100) # replaces the cpts slot from x with c(10,50,100)
test_that("class9", expect_equivalent(cpts(x), c(10, 50, 100)))

# Example of a change in variance at 100 in simulated normal data
set.seed(1)
x = c(rnorm(100, 0, 1), rnorm(100, 0, 10))
ans = cpt.var(x)
test_that("class10", expect_equivalent(logLik(ans), c(1003.2283241358, 1012.438665)))




# From cpts.full.Rd
x = new("cpt.range") # new cpt.range object
test_that("class11", expect_is(x, "cpt.range"))
test_that("class12", expect_is(cpts.full(x), "matrix")) # retrieves the cpts.full slot from x




# From cpts.full-.Rd
x = new("cpt.range") # new cpt.range object
cpts.full(x) = matrix(c(10, 20, 10, NA), nrow = 2, byrow = TRUE)
test_that("class13", expect_equivalent(cpts.full(x), matrix(c(10, 20, 10, NA), nrow = 2, byrow = TRUE)))




# From cpts.Rd
x = new("cpt") # new cpt object
test_that("class14", expect_equivalent(cpts(x), numeric())) # retrieves the cpts slot from x





# From cpts.ts.Rd
x = new("cpt") # new cpt object
test_that("class15", expect_equivalent(cpts.ts(x), numeric()))




# From cpts-.Rd
x = new("cpt") # new cpt object
cpts(x) = 10 # replaces the vector of changepoint in object x with 10
test_that("class16", expect_equivalent(cpts(x), 10))





# From cpttype.Rd
x = new("cpt") # new cpt object
test_that("class17", expect_equivalent(cpttype(x), "Not Set")) # retrieves the cpttype slot from x





# From cpttype-.Rd
x = new("cpt") # new cpt object
cpttype(x) = "mean" # replaces the existing cpttype in object x with "mean"
test_that("class18", expect_equivalent(cpttype(x), "mean"))




# From data.set.Rd
x = new("cpt") # new cpt object
test_that("class19", expect_equivalent(data.set(x), ts()))




# From data.set.ts.Rd
x = new("cpt") # new cpt object
test_that("class20", expect_equivalent(data.set.ts(x), ts()))





# From data.set-.Rd
x = new("cpt") # new cpt object
data.set(x) = c(1, 2, 3, 4, 5) # replaces the existing data.set slot in x with c(1,2,3,4,5)
test_that("class21", expect_equivalent(data.set(x), 1:5))




# From distribution.Rd
x = new("cpt") # new cpt object
test_that("class22", expect_equivalent(distribution(x), character()))





# From distribution-.Rd
x = new("cpt") # new cpt object
distribution(x) = "normal" # replaces the current distribution slot of x with "normal"
test_that("class23", expect_equivalent(distribution(x), "normal"))






# From likelihood.Rd
set.seed(1)
x = c(rnorm(50, 0, 1), rnorm(50, 0, 10), rnorm(50, 0, 5), rnorm(50, 0, 1))
out = cpt.var(x, penalty = "Manual", pen.value = "2*log(n)", method = "BinSeg", Q = 5)
test_that("logLik1", expect_equivalent(likelihood(out), c(925.8085, 957.5984)))





# From logLik-methods.Rd
set.seed(1)
x = c(rnorm(50, 0, 1), rnorm(50, 0, 10), rnorm(50, 0, 5), rnorm(50, 0, 1))
out = cpt.var(x, penalty = "Manual", pen.value = "2*log(n)", method = "BinSeg", Q = 5)
test_that("logLik1", expect_equivalent(logLik(out), c(925.8085, 957.5984)))




# From method.Rd
x = new("cpt") # new cpt object
test_that("class24", expect_equivalent(method(x), character()))





# From method-.Rd
x = new("cpt") # new cpt object
method(x) = "mean" # replaces the existing method slot in x with "mean"
test_that("class25", expect_equivalent(method(x), "mean"))




# From minseglen.Rd
x = new("cpt") # new cpt object
test_that("class26", expect_equivalent(minseglen(x), numeric()))





# From minseglen-.Rd
x = new("cpt") # new cpt object
minseglen(x) = 5 # replaces the existing minseglen slot in x with 5
test_that("class27", expect_equivalent(minseglen(x), 5))




# From ncpts.max.Rd
x = new("cpt") # new cpt object
test_that("class28", expect_equivalent(ncpts.max(x), numeric()))





# From ncpts.max-.Rd
x = new("cpt") # new cpt object
ncpts.max(x) = 10 # replaces the vector of changepoint in object x with 10
test_that("class29", expect_equivalent(ncpts.max(x), 10))






# From ncpts.Rd
x = new("cpt") # new cpt object
test_that("class30", expect_equivalent(ncpts(x), 0)) # returns the number of changepoints (i.e. length of the cpts slot in x minus 1)






# From nseg.Rd
x = new("cpt") # new cpt object
test_that("class31", expect_equivalent(nseg(x), 1))





# From param.est.Rd
x = new("cpt") # new cpt object
test_that("class32", expect_equivalent(param.est(x), list()))





# From param.est-.Rd
x = new("cpt") # new cpt object
param.est(x) = list(mean = 0) # replaces the current param.est list in x with list(mean=0)
test_that("class33", expect_equivalent(param.est(x), list(mean = 0)))






# From param.Rd
set.seed(1)
x = c(rnorm(100, 0, 1), rnorm(100, 0, 10))
ans = cpt.var(x, penalty = "Asymptotic", pen.value = 0.01, method = "AMOC", param.estimates = FALSE)
ans = param(ans) # fills the param.est slot with the parameter estimes.
test_that("class34", expect_equivalent(param.est(ans), list(variance = c(0.7986945, 90.8356989))))






# From pen.type.Rd
x = new("cpt") # new cpt object
test_that("class35", expect_equivalent(pen.type(x), character()))





# From pen.type-.Rd
x = new("cpt") # new cpt object
pen.type(x) = "SIC" # replaces the existing pen.type slot in x with "SIC"
test_that("class36", expect_equivalent(pen.type(x), "SIC"))






# From pen.value.full.Rd
x = new("cpt.range") # new cpt.range object
test_that("class37", expect_equivalent(pen.value.full(x), numeric()))





# From pen.value.full-.Rd
x = new("cpt.range") # new cpt.range object
pen.value.full(x) = 5 # replaces the existing pen.value.full slot in x with 5
test_that("class38", expect_equivalent(pen.value.full(x), 5))





# From pen.value.Rd
x = new("cpt") # new cpt object
test_that("class39", expect_equivalent(pen.value(x), numeric()))




# From pen.value-.Rd
x = new("cpt") # new cpt object
pen.value(x) = 5 # replaces the existing pen.value slot in x with 5
test_that("class40", expect_equivalent(pen.value(x), 5))





# From seglen.Rd
x = new("cpt") # new cpt object
test_that("class41", expect_equivalent(seg.len(x), numeric()))





# From test.stat.Rd
x = new("cpt") # new cpt object
test_that("class42", expect_equivalent(test.stat(x), character()))




# From test.stat-.Rd
x = new("cpt") # new cpt object
test.stat(x) = "normal" # replaces the current test.stat slot of x with "normal"
test_that("class43", expect_equivalent(test.stat(x), "normal"))
