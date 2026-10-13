# CRAN readiness — version 0.2.2

**Not yet verified as CRAN-ready.** This revision was assembled and statically checked without a local R runtime. Run `devtools::document()`, `devtools::test()`, and `devtools::check(document = FALSE, args = "--as-cran")` in RStudio. In particular, check the new analytic plot data and fitted intercept recovery with `scale_intercept = TRUE`.

Before submission, review DESCRIPTION metadata, package URL, licensing, example/vignette runtime, platform checks, and all `R CMD check` warnings/notes. The synthetic benchmark under `inst/benchmarks/` is optional and is not executed by the test suite. Keep proprietary datasets out of the repository.

## R CMD check follow-up (2026-10-12)

The last reported Windows check was 0 errors, 1 WARNING, 1 NOTE. The remaining issues were Rd markdown links caused by `E[...]`, unqualified `head` and `tail`, and ggplot aesthetic variables. This archive addresses these at the R source and checked-in manual page level. The updated check has not yet been executed in this environment; rerun `devtools::document(); devtools::test(); devtools::check(document = FALSE, args = "--as-cran")`.

## 0.2.2 targeted numerical regression checks

Check `test-022-numerical-stability.R`: a free-intercept fit cannot have materially worse log likelihood than its nested fit, catastrophic Burr cancellation cannot yield zero log likelihood, and extreme deductible plotting is finite. Verify results on the 85,854-claim synthetic scenario using the standalone `validate_022_external.R`.

The fitter now enforces shape admissibility `[1e-5, 1e5]` for major positive-shape parameters and rejects an extreme conditional log-probability beyond the safe numerical range. These are defensive numerical limits, not assumptions about actuarial tail models. Review if your intended calibration genuinely requires parameters beyond that range.

Failures in a candidate fit should be diagnosed and explicitly recorded with missing AIC/BIC; they should not be silently ranked. Profiling runtime may increase when several starting points or nested fit baselines are required.
