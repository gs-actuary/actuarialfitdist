# actuarialfitdist 0.2.2 — candidate fitting numerical stability

The supplied external profiling files (0.2.1) revealed two model-selection defects: (i) the free-intercept Weibull fit could report convergence at a substantially inferior likelihood, and (ii) an extreme Burr parameter vector could produce logLik approximately 0 and therefore a nonsensical AIC. The same profiling exposed analytical plot failures when survival was calculated using `1 - F(d)`.

Changes in 0.2.2:

- A zero-intercept fitted model serves as both the initial anchor and a likelihood baseline for any free additive scale-intercept fit. A candidate at a materially worse log-likelihood is rejected even with convergence code zero. Only when necessary, the intercept optimizer retries a second scaling choice.
- Gamma and Burr optimizers search additional initial shape values. Implausibly extreme shape values and cancellation-prone log-likelihood calculations are rejected; a failed family has no AIC or BIC rather than automatically winning a candidate comparison.
- Burr log-density / survival uses logarithmic stable transformations; plot-ready conditional CDFs use log-survival differences.
- Four focused regression tests were added, with a CRAN check checklist below.

**Compatibility:** No exported signatures changed. Data and optimization results may change if prior fits were degenerate. Internal shape admissibility now restricts shape parameters to [1e-5, 1e5], which may exclude legitimately extreme distribution families and should be reviewed before CRAN submission. Fitting can be slower because validation now takes precedence over speed.

**Check commands:**

```r
devtools::document()
devtools::test()
devtools::check(document = FALSE, args = "--as-cran")
```

This build has undergone static packaging checks only. It has not passed an executable R CMD check in the producing environment; it should be treated as **a candidate for CRAN readiness, not a certified CRAN-ready release**.
