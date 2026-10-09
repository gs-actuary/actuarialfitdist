# Installing actuarialfitdist 0.2.0 (development snapshot)

The primary ZIP contains the full source tree. Do not overwrite uncommitted
edits in your existing project; commit or back them up first. The snapshot is
based on the earlier `actuarialfitdist_0.1.0-checkfix.zip` rather than an
unverified live pull from GitHub.

## Recommended for RStudio development

1. Extract the ZIP. The top directory is `actuarialfitdist/`.
2. Open `actuarialfitdist.Rproj` or the containing folder in RStudio.
3. Install dependencies with `install.packages(c("devtools", "testthat", "roxygen2", "ggplot2", "scales", "knitr", "rmarkdown"))`.
4. Run the following in the package project:

```r
devtools::document()
devtools::test()
devtools::check(document = FALSE, args = "--as-cran")
# Optional, only after success:
devtools::install(build_vignettes = TRUE)
vignette("actuarialfitdist", package = "actuarialfitdist")
vignette("candidate-comparisons", package = "actuarialfitdist")
```

## Minimum post-install smoke test

```r
library(actuarialfitdist)
set.seed(8)
d <- data.frame(loss = rlnorm(400, log(15000), 0.85))
fits <- fit_severity_candidates(d, "loss", distributions = c("lognormal", "gamma"), hessian = FALSE)
print(fits$comparison)
print(plot_candidate_fits(fits, nsim = 1))
print(head(severity_factors(fits, deductible = c(500, 1000),
   limit = c(25000, 50000), grid = "paired")))
```

When checking through `R CMD check`, first build with `R CMD build actuarialfitdist`.
This ZIP has pre-generated Rd pages; use `devtools::document()` to regenerate
those pages and the NAMESPACE from the roxygen comments before final check.
See `CRAN_READINESS.md` for the remaining verification and statistical caveats.
