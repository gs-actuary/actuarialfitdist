# CRAN-readiness review for 0.2.0 source

## Documentation correction (post-check)

The user reported 77 passing tests and an R CMD check with 0 errors,
2 warnings and 1 note. All three messages concerned documentation.
This archive fixes the roxygen2 source blocks and bundled Rd help files;
it still **requires a fresh `devtools::document()` and `devtools::check()`**
to verify that the generated documentation is warning- and note-free.


This ZIP is an editable source-tree snapshot, not a CRAN-checked binary or
`R CMD build` output. The author should run the checks below in R 4.1+ with
current dependencies installed. This environment has no R interpreter, so we
**cannot** claim that `R CMD check --as-cran` passes, that roxygen-generated
Rd files have been validated, or that timing regressions meet their targets.

## On the author's R machine

```r
# install.packages(c("devtools", "roxygen2", "testthat", "scales", "knitr", "rmarkdown"))
# Run in the folder containing DESCRIPTION:
devtools::document()
devtools::test()
devtools::check(document = FALSE, args = "--as-cran")
# Optional platform coverage: rhub::rhub_check() or CI on Windows/Linux/macOS.
```

```sh
R CMD build actuarialfitdist
R CMD check --as-cran actuarialfitdist_0.2.0.tar.gz
```

Do not submit until errors and warnings are resolved. Review any NOTES,
especially examples/vignette runtime, Rd examples, non-ASCII, or method
signatures. Regression testing includes exact formula checks for expected
payments and spliced search behavior. The local machine should repeat the
100 x 100 factor grid timing benchmark against the original version.

## Statistical review before release

* `loss` is a **ground-up** severity and not net payment/ACV. Fitting `limit`
  is a **ground-up censoring threshold**. Factor-generation `limit` is a
  **payment cap after deductible**. Avoid conflating them.
* Above-limit rows are excluded by default (not silently treated as censored).
  Out-of-contract payments require analyst assessment. Censored-at-limit
  observations remain in the likelihood as right-censored data.
* Search thresholds are empirical quantiles of observed/censored losses.
  Pileups at limits yield duplicate thresholds. Min exact-tail count defaults
  to 25; flagged extreme tail scales/shapes still require actuarial judgment.
* Performance optimization uses analytic LEV for four families; all other
  families/splices use numerical quadrature and may remain slower on large
  Cartesian grids. This is a remaining optimization opportunity.
* Automatic exclusion changes sample composition. Audit original positions
  and reasons before accepting a model in production.
