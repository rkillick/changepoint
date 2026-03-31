# GSoC test results template

Use this as the content you can post back to mentors after pushing to GitHub.

## Easy

- Script: `inst/gsoc/easy_envcpt_ar_tests.R`
- Plot output: `inst/gsoc/easy_envcpt_ar_plots.png`
- Reproducible command:
  - `Rscript inst/gsoc/easy_envcpt_ar_tests.R`
- Latest local result:
  - AR1 changepoints: `200`
  - AR2 changepoints: `199`

## Medium

- Added tests:
  - `tests/testthat/test-cptreg-edgecases.R`
- Reproducible command:
  - `R -q -e "library(changepoint); testthat::test_file('tests/testthat/test-cptreg-edgecases.R')"`
- Latest local result:
  - test file passes locally

## Hard

- Package scaffold path:
  - `/Users/kaikaizhang/Documents/research/gsoc-envcpt-ar-wrapper`
- Key function:
  - `R/fit_ar_changepoint.R`
- Reproducible commands:
  - `R -q -e "testthat::test_local('.', reporter='summary')"`
  - `R -q -e "cov <- covr::package_coverage(path='.'); covr::percent_coverage(cov)"`
- Latest local result:
  - tests pass
  - coverage: `85.71%`

## Replace with your GitHub links

- Medium fork URL: `<ADD_URL>`
- Medium commit/PR URL: `<ADD_URL>`
- Hard package repository URL: `<ADD_URL>`
- Hard CI URL: `<ADD_URL>`
- Hard coverage URL: `<ADD_URL>`
