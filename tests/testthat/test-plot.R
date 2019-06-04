context("plot(diagnostic = TRUE) tests")

# Generate cpt.range object
testdata <- changepoint::ftse100$V2
obj.cpt.range <- cpt.var(testdata, method = "PELT",
                         penalty = "CROPS", pen.value = c(5, 500))

# For code coverage
plot(obj.cpt.range, diagnostic = TRUE)
plot(obj.cpt.range, diagnostic = TRUE, type = "h")

# Tests for plots
#These functions are still somewhat experimental and even though the generated plots match the true plots
#there still seems to be some issues being caused. https://github.com/r-lib/vdiffr/issues
#we will readd this feature once the original issue is resolved.

#vdiffr::expect_doppelganger("Diagnostic plot (default)", plot(obj.cpt.range, diagnostic = TRUE))
#vdiffr::expect_doppelganger("Diagnostic plot (histogram)", plot(obj.cpt.range, diagnostic = TRUE, type = "h"))



