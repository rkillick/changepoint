set.seed(20260331)

suppressPackageStartupMessages({
  library(EnvCpt)
})

# Generate piecewise AR(1) and AR(2) signals with changepoints in dependence.
generate_piecewise_ar <- function(n_per_segment = 200) {
  seg1 <- as.numeric(stats::arima.sim(
    n = n_per_segment,
    model = list(ar = 0.2),
    sd = 1
  ))
  seg2 <- as.numeric(stats::arima.sim(
    n = n_per_segment,
    model = list(ar = 0.75),
    sd = 1
  ))
  seg3 <- as.numeric(stats::arima.sim(
    n = n_per_segment,
    model = list(ar = c(0.6, -0.25)),
    sd = 1
  ))
  c(seg1, seg2, seg3)
}

# Use EnvCpt non-exported function, as required by the GSoC easy test.
fit_envcpt_nonexported_ar <- function(x, order = c(1L, 2L), with_trend = FALSE, minseglen = 20L) {
  order <- as.integer(order[1])
  stopifnot(is.numeric(x), length(x) > 100)
  stopifnot(order %in% c(1L, 2L))
  stopifnot(is.logical(with_trend), length(with_trend) == 1L)

  cpt_reg <- getFromNamespace("cpt.reg", "EnvCpt")
  n <- length(x)

  if (order == 1L) {
    if (!with_trend) {
      design <- cbind(x[-1], rep(1, n - 1), x[-n])
    } else {
      design <- cbind(x[-1], rep(1, n - 1), seq_len(n - 1), x[-n])
    }
  } else {
    if (!with_trend) {
      design <- cbind(x[-c(1, 2)], rep(1, n - 2), x[2:(n - 1)], x[1:(n - 2)])
    } else {
      design <- cbind(
        x[-c(1, 2)],
        rep(1, n - 2),
        seq_len(n - 2),
        x[2:(n - 1)],
        x[1:(n - 2)]
      )
    }
  }

  cpt_reg(
    data = design,
    method = "PELT",
    minseglen = minseglen,
    dist = "Normal",
    class = TRUE,
    param.estimates = TRUE
  )
}

series <- generate_piecewise_ar()

fit_ar1 <- fit_envcpt_nonexported_ar(series, order = 1L, with_trend = FALSE)
fit_ar2 <- fit_envcpt_nonexported_ar(series, order = 2L, with_trend = FALSE)

png("inst/gsoc/easy_envcpt_ar_plots.png", width = 1600, height = 700, res = 130)
par(mfrow = c(1, 2))
plot(fit_ar1, main = "EnvCpt non-exported AR1 changepoints")
plot(fit_ar2, main = "EnvCpt non-exported AR2 changepoints")
dev.off()

cat("AR1 changepoints:\n")
print(cpts(fit_ar1))
cat("AR2 changepoints:\n")
print(cpts(fit_ar2))

cat("\nSession info:\n")
print(sessionInfo())
