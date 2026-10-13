# actuarialfitdist 0.2.3 (CRAN documentation candidate)

* Shorten the package title and add a standard loss-modeling reference in DESCRIPTION.
* Rewrite help-page return-value descriptions and important argument explanations.
* Clarify ground-up observation censoring versus post-deductible payment caps.
* Document deterministic analytic plotting as the default and simulation as optional.
* Add installation instructions and a reproducible release-check procedure.
* **No changes to likelihood, optimization, plotting, or factor-calculation code.**

# actuarialfitdist 0.2.2 (numerical-stability update)

* Fit a zero-intercept nested anchor before estimating a free scaling intercept. Reject an apparently converged free-intercept fit if its log likelihood is materially worse than the nested solution.
* Retry Gamma/Burr optimizations from multiple shape starts, report `optimizer_attempts`, reject inadmissible final fits, and avoid pathological near-zero/astronomical shape values that cause catastrophic floating-point cancellation.
* Compute Burr log-density and log-survival using stable log1p-exp expressions.
* Construct deterministic fitted severity-plot probabilities from conditional **log-survival ratios** rather than `1 - F(d)` to avoid loss of tail precision.
* Candidate fitting reports failed families as missing AIC/BIC rather than rewarding degenerate fits.
* Add tests for heavy-censoring, large-scale covariates, nested intercept likelihood, Burr degeneracy, and extreme-tail plot survival.
* These changes prioritize fit validity over the fastest possible fitting time. Source is distributed for independent R CMD check; CRAN checks still required on a machine with R.

# actuarialfitdist 0.2.1

* Corrected roxygen2 help text so mathematical expressions are not interpreted as cross-reference links. Qualified utils::head/tail and made ggplot data-mask references explicit for clean R CMD check.

* Corrected invalid reserved-word argument names in all-fixed-parameter fitting branches in `fit_severity()` and `fit_spliced_severity()`.

* Automatic numerical `parscale` for estimated scale intercepts; explicit optimizer overrides preserved, including in spliced models.
* Candidate comparison now includes individual runtimes, optimizer evaluation counts and convergence codes.
* Reuse repeated deductible/scaling term probabilities within each ordinary likelihood evaluation.
* Candidate and single-fit plots default to deterministic censoring/truncation-aware bin/CDF summaries rather than simulating large portfolios. `method = "simulation"` retains the previous option.
* `fit_plot_data()` and `candidate_plot_data()` support `output = "plot_ready"`; `scaling_plot_data()` exposes the exact long plot data.
* User-facing plot axis-label sizes, angles and explicit tick breaks; 45-degree tick rotation for faceted candidates.
* Exact rejection simulation for inverse Gaussian draws conditional on a deductible, avoiding most per-row inverse-CDF root searches.
* New regression tests and documented benchmark script; updated README/vignette/examples.

# actuarialfitdist 0.2.0 (development)

* Documentation-only CRAN check cleanup: unique S3 aliases, correct plotting
  helper documentation association, and wrapped candidate usage lines.

* Optional deductible and censoring-limit columns for single, candidate and spliced fits.
* One-pass anomaly screening, warnings, original-position `excluded_rows`, and reason table `excluded_details`; `invalid_rows = "error"` available.
* Splice threshold search reports eligible body/exact-tail counts and explicit rejected-candidate reasons; configurable minimum uncensored tail observations.
* Vectorized analytic limited expected values for exponential, Weibull, gamma and lognormal families; batched severity factor grids, plus paired scenario mode.
* Log10 severity axes and xlim controls; candidate overlays/facets using observation-aware simulated claim data.
* Expanded quick-start, worked candidate vignette, and API examples.
* The package source is provided for local R checks. Its CRAN status is not certified in an environment without R.

# actuarialfitdist 0.1.0

* Numerical warnings from invalid optimizer trial points are now handled internally; such points receive a penalized likelihood while invalid final fits still fail explicitly.

* Initial release.
* Fits common actuarial severity distributions to ground-up losses with deductible truncation and limit censoring.
* Supports optional linear severity scaling, likelihood weights, fixed parameters, candidate comparison, and spliced body-tail models.
* Generates limited expected severities, LERs, ILFs, deductible factors, combined rating factors, and valuation-adjusted expected payments.
* Includes simulation-based goodness-of-fit diagnostics, scaling diagnostics, layer diagnostics, Hessian inference, and basic bootstrap support.

* 0.2.1 source maintenance: resolved candidate plotting default argument
  handling within `lapply()` and consolidated roxygen2 source-of-truth
  maintenance for help pages, NAMESPACE, package docs, and S3 print registration.
