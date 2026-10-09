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
