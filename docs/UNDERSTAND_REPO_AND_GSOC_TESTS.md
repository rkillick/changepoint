# changepoint repo quick guide

This repository is the CRAN `changepoint` package. It provides exact and approximate changepoint detection algorithms with R front-end APIs and C back-end compute kernels.

## 1) How this repo is organized

- `R/`:
  - User entry points: `cpt.mean()`, `cpt.var()`, `cpt.meanvar()` in `R/cpt.R`, and `cpt.reg()` in `R/CptReg.R`.
  - Algorithm wrappers: `PELT`, `BINSEG`, `SEGNEIGH` wrappers call into C where needed.
- `src/`:
  - Core high-performance implementations for PELT/BinSeg/regression changepoint routines.
- `tests/testthat/`:
  - Regression tests for mean/variance/meanvar/regression behavior and plots/examples.
- `man/`:
  - `.Rd` documentation, including package-level and function examples.

## 2) Mental model for `changepoint`

At a high level:

1. You call a front-end function (for example `cpt.mean(data, method="PELT")`).
2. Input checks + penalty logic are applied in R.
3. The chosen method dispatches to method-specific R wrappers.
4. Wrappers call C implementations for speed where appropriate.
5. Results are wrapped into S4 classes (`cpt`, `cpt.reg`, `cpt.range`) with plotting/summary methods.

## 3) Where to read first

1. `DESCRIPTION` and `NAMESPACE` for package scope and exports.
2. `R/cpt.R` for top-level method/test-stat dispatch.
3. `R/CptReg.R` for regression-changepoint flow.
4. `R/PELT_one_func_minseglen.R` + `src/PELT_one_func_minseglen.c` to see R-to-C integration.
5. `tests/testthat/` for expected behavior and edge cases.

## 4) Easy test from GSoC spec

Use `inst/gsoc/easy_envcpt_ar_tests.R` in this repo. It:

- simulates a series with changing AR structure,
- uses non-exported EnvCpt internals via `getFromNamespace("cpt.reg", "EnvCpt")`,
- fits AR1 and AR2 changepoint models (without calling `envcpt()`),
- saves a plot to `inst/gsoc/easy_envcpt_ar_plots.png`.

Run:

```bash
Rscript inst/gsoc/easy_envcpt_ar_tests.R
```

## 5) Medium test in this repo

Added: `tests/testthat/test-cptreg-edgecases.R`

Coverage-focused edge cases include:
- invalid method handling,
- invalid `minseglen` handling,
- warning path when `minseglen` is auto-bumped,
- multi-dataset array input path,
- unsupported `CROPS` path in `cpt.reg`.

Run only this file quickly:

```bash
R -q -e "library(changepoint); testthat::test_file('tests/testthat/test-cptreg-edgecases.R')"
```

Note: full local `testthat::test_local('.')` may fail on systems missing local Fortran toolchain libs due native package compilation requirements.

## 6) Hard test package scaffold

A separate package scaffold is created at:

- `/Users/kaikaizhang/Documents/research/gsoc-envcpt-ar-wrapper`

It contains:
- a wrapper function for AR1/AR2 with optional trend,
- input validation,
- unit tests,
- GitHub Actions `R-CMD-check`,
- covr + codecov workflow.
