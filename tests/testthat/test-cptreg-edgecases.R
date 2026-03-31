context("cpt.reg edge-case tests")

set.seed(42)
n <- 120
x1 <- rnorm(n)
x2 <- rnorm(n)
y <- c(rnorm(n / 2, mean = 0), rnorm(n / 2, mean = 2))
reg_data <- cbind(y, 1, x1, x2)

test_that("unsupported method is rejected", {
  expect_error(
    cpt.reg(reg_data, method = "BinSeg"),
    "Invalid method, must be AMOC or PELT"
  )
})

test_that("invalid minseglen values are rejected", {
  expect_error(
    cpt.reg(reg_data, minseglen = 0),
    "must be positive integer"
  )
  expect_error(
    cpt.reg(reg_data, minseglen = 2.5),
    "must be positive integer"
  )
})

test_that("small minseglen is bumped to number of columns", {
  expect_warning(
    fit <- cpt.reg(reg_data, method = "AMOC", minseglen = 1),
    "minseglen is too small"
  )
  expect_s4_class(fit, "cpt.reg")
})

test_that("multiple datasets return list of cpt.reg", {
  reg_data_2 <- reg_data
  reg_data_2[, 1] <- reg_data_2[, 1] + rnorm(nrow(reg_data_2), sd = 0.05)
  arr <- array(NA_real_, dim = c(2, nrow(reg_data), ncol(reg_data)))
  arr[1, , ] <- reg_data
  arr[2, , ] <- reg_data_2
  out <- cpt.reg(arr, method = "AMOC", minseglen = 5)
  expect_type(out, "list")
  expect_equal(length(out), 2)
  expect_s4_class(out[[1]], "cpt.reg")
  expect_s4_class(out[[2]], "cpt.reg")
})

test_that("CROPS penalty remains unsupported for cpt.reg", {
  expect_error(
    cpt.reg(reg_data, penalty = "CROPS", pen.value = c(2, 4)),
    "CROPS has not yet been implemented for cpt.reg"
  )
})
