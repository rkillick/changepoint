# GSoC test results

## Easy

- Script: `inst/gsoc/easy_envcpt_ar_tests.R`
- Plot output: `inst/gsoc/easy_envcpt_ar_plots.png`
- Reproducible command:
  - `Rscript inst/gsoc/easy_envcpt_ar_tests.R`
- Latest local result:
  - AR1 changepoints: `200`
  - AR2 changepoints: `199`
- GitHub artifacts:
  - Script: `https://github.com/KaiKz/changepoint/blob/gsoc-tests-deliverables/inst/gsoc/easy_envcpt_ar_tests.R`
  - Plot: `https://github.com/KaiKz/changepoint/blob/gsoc-tests-deliverables/inst/gsoc/easy_envcpt_ar_plots.png`

## Medium

- Added tests:
  - `tests/testthat/test-cptreg-edgecases.R`
- Reproducible command:
  - `R -q -e "library(changepoint); testthat::test_file('tests/testthat/test-cptreg-edgecases.R')"`
- Latest local result:
  - test file passes locally
- GitHub artifacts:
  - Test file: `https://github.com/KaiKz/changepoint/blob/gsoc-tests-deliverables/tests/testthat/test-cptreg-edgecases.R`
  - Commit: `https://github.com/KaiKz/changepoint/commit/d92ae02`
  - PR: `https://github.com/KaiKz/changepoint/pull/new/gsoc-tests-deliverables`

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
- GitHub artifacts:
  - Repository: `https://github.com/KaiKz/gsoc-envcpt-ar-wrapper`
  - Main function: `https://github.com/KaiKz/gsoc-envcpt-ar-wrapper/blob/main/R/fit_ar_changepoint.R`
  - Commit: `https://github.com/KaiKz/gsoc-envcpt-ar-wrapper/commit/717a815`
  - CI workflow: `https://github.com/KaiKz/gsoc-envcpt-ar-wrapper/actions/workflows/R-CMD-check.yaml`
  - Coverage workflow: `https://github.com/KaiKz/gsoc-envcpt-ar-wrapper/actions/workflows/coverage.yaml`

## Submission links

- Medium fork URL: `https://github.com/KaiKz/changepoint`
- Medium commit URL: `https://github.com/KaiKz/changepoint/commit/d92ae02`
- Medium PR URL: `https://github.com/KaiKz/changepoint/pull/new/gsoc-tests-deliverables`
- Hard package repository URL: `https://github.com/KaiKz/gsoc-envcpt-ar-wrapper`
- Hard CI URL: `https://github.com/KaiKz/gsoc-envcpt-ar-wrapper/actions/workflows/R-CMD-check.yaml`
- Hard coverage URL: `https://github.com/KaiKz/gsoc-envcpt-ar-wrapper/actions/workflows/coverage.yaml`
