# actuarialfitdist 0.2.1 — focused performance and plot API update

## Changes

* Automatically choose an `optim()` `parscale` for estimated additive severity-scale intercepts: `max(1000, 0.10 * median(valid observed severity))`. Applies to single and spliced fits and respects an explicit user override. Remaining encoded optimizer coordinates keep `parscale = 1`.
* Increase finite-difference Hessian steps for rescaled coordinates to avoid the previous tiny dollar-intercept numerical perturbations; keep all fitted coefficients in original units.
* Reuse deductible survival evaluations for repeated (deductible, scaling-variable) combinations during the objective function evaluation.
* Add per-family `elapsed_seconds`, `function_evaluations`, `gradient_evaluations`, and `convergence_code` to `fit_severity_candidates()$comparison`; individual fits contain `optimizer_counts`, `optimizer_control`, and `elapsed_seconds`.
* Default `plot_fit()` and `plot_candidate_fits()` to **analytic** probability-based observed-claims summaries. Model probabilities reflect individual left truncation and censoring. Output is binned probability density per x-coordinate (or CDF/survival), so capped observations appear as probability mass rather than as a fictitious smooth continuous density.
* Keep `method = "simulation"` for stochastic graphical checks and backwards-compatible `output = "raw"` in the data-builder functions.
* Add `output = "plot_ready"` to both `fit_plot_data()` and `candidate_plot_data()`, returning exactly the summary data consumed by the default ggplots, including source, distribution where applicable, x, value, lower, upper. `scaling_plot_data(output = "plot_ready")` exposes the exact long data used for scaling plots.
* Add configurable `x_text_angle`, `x_text_size`, `y_text_size`, and `x_breaks` on plot functions. Faceted candidate plots default to a 45-degree tilt.
* Accelerate inverse-Gaussian observed-claim simulation via exact vectorized rejection sampling, with precise numerical quantile inversion as a fallback for exceptionally severe truncation.
* Add synthetic regression tests and an optional 100,000-claim benchmark script at `inst/benchmarks/benchmark_021.R`.
* Update vignettes, README, NEWS, and help pages.

## Compatibility

* Existing fitting functions, limits/deductibles, anomaly auditing and factor-generation interfaces remain available.
* `candidate_plot_data()` and `fit_plot_data()` continue to return individual simulated claims by default (`output = "raw"`).
* **Plot defaults changed:** `plot_fit()` / `plot_candidate_fits()` now default to deterministic analytic summaries. To reproduce prior plots, specify `method = "simulation"`.
* The optimizer's changed numerical scaling may produce small differences in parameter estimates or convergence, especially when an estimated additive intercept was previously poorly scaled. Compare log-likelihoods and coefficients against prior fits.

## Testing status

This source is **NOT CERTIFIED CRAN-READY**. Static delimiter, function-interface documentation, unique help alias, archive integrity and file-structure checks have passed in the build environment. No R executable is available here. The R test suite, package build, vignette rendering, benchmark and `R CMD check --as-cran` are **not executed here**.

Run in RStudio (from the extracted package's project directory):

```r
devtools::document()
devtools::test()
devtools::check(document = FALSE, args = "--as-cran")
```

Then run the installed/source benchmark script if needed:

```r
source("inst/benchmarks/benchmark_021.R")
```

If results are satisfactory, build official CRAN source tarball with `devtools::build()` or `R CMD build` locally, not from the convenience tarball in this download.

## Source correction (2026-10-12)

* Fixed invalid unquoted reserved-word argument names in `R/fit.R` and `R/splice.R`: `c("function" = 0L, gradient = 0L)`. This previously prevented `devtools::document()` / `load_all()` from parsing package source.
* Added `inst/benchmarks/parse_all_sources.R` for a fast source parsing preflight on a system with R installed.
* These corrections were inspected statically; the build environment has no R runtime and cannot independently execute the preflight or the full R checks.
