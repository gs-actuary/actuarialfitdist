# Installing actuarialfitdist 0.2.3 (CRAN submission candidate)

This ZIP contains a complete R package source tree, built from the previously
shared 0.2.2 numerical-fix source. **Make a commit or backup** before replacing
existing project files with this source. It does not download or overwrite
anything automatically.

## Local development in RStudio

1. Extract this ZIP. Its top-level folder is `actuarialfitdist/`.
2. Open `actuarialfitdist.Rproj` in RStudio.
3. Install missing development dependencies if necessary:

```r
install.packages(c("devtools", "testthat", "roxygen2",
                   "ggplot2", "scales", "knitr", "rmarkdown"))
```

4. From the package project, run:

```r
devtools::document()
devtools::test()
devtools::check_doc_fields()
urlchecker::url_check() # first install.packages("urlchecker") if needed
devtools::check(document = FALSE, args = "--as-cran",
                remote = TRUE, manual = TRUE)
```

5. Examine the `man/` and `NAMESPACE` diff produced by `document()`.
Review the displayed HTML/PDF help for the fitting, candidate, plotting,
factor, simulation, bootstrap, and S3 methods.
6. Install locally to open vignettes:

```r
devtools::install(build_vignettes = TRUE)
vignette("actuarialfitdist", package = "actuarialfitdist")
vignette("candidate-comparisons", package = "actuarialfitdist")
```

7. Check R-devel via `devtools::check_win_devel()`, and rerun the large
synthetic numerical validation suite that identified 0.2.1 failures.

The documentation now has a roxygen2 source for the shared S3 methods topic;
the Rd files are included so users can read help immediately, but regeneration
with your installed roxygen2 is necessary before certifying CRAN readiness.

**No R runtime was available to execute R CMD check when producing this ZIP.**
Please report any errors, warnings, or notes from the final check.
