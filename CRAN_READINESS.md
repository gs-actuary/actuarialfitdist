# CRAN readiness — version 0.2.1

**Not yet verified as CRAN-ready.** This revision was assembled and statically checked without a local R runtime. Run `devtools::document()`, `devtools::test()`, and `devtools::check(document = FALSE, args = "--as-cran")` in RStudio. In particular, check the new analytic plot data and fitted intercept recovery with `scale_intercept = TRUE`.

Before submission, review DESCRIPTION metadata, package URL, licensing, example/vignette runtime, platform checks, and all `R CMD check` warnings/notes. The synthetic benchmark under `inst/benchmarks/` is optional and is not executed by the test suite. Keep proprietary datasets out of the repository.

## R CMD check follow-up (2026-10-12)

The last reported Windows check was 0 errors, 1 WARNING, 1 NOTE. The remaining issues were Rd markdown links caused by `E[...]`, unqualified `head` and `tail`, and ggplot aesthetic variables. This archive addresses these at the R source and checked-in manual page level. The updated check has not yet been executed in this environment; rerun `devtools::document(); devtools::test(); devtools::check(document = FALSE, args = "--as-cran")`.
