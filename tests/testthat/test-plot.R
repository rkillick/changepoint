context("plot(diagnostic = TRUE) tests")

# Generate cpt.range object
testdata <- changepoint::ftse100$V2
obj.cpt.range <- cpt.var(testdata, method = "PELT",
                         penalty = "CROPS", pen.value = c(5, 500))

# For code coverage
plot(obj.cpt.range, diagnostic = TRUE)
plot(obj.cpt.range, diagnostic = TRUE, type = "h")

# Tests for plots
vdiffr::expect_doppelganger("Diagnostic plot (default)",
                            plot(obj.cpt.range, diagnostic = TRUE))
vdiffr::expect_doppelganger("Diagnostic plot (histogram)",
                            plot(obj.cpt.range, diagnostic = TRUE, type = "h"))
