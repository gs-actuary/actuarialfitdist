# actuarialfitdist

`actuarialfitdist` is an R package for actuarial severity distribution fitting and the limit/deductible rating analyses that follow from those fits.

The package is deliberately opinionated about one important data assumption:

> **The fitting functions expect ground-up claim severity.**
>
> The loss column is not an insurer payment, an amount net of deductible, an excess-of-deductible amount, or an ACV/depreciated payment unless the analyst has first transformed the data onto the intended common ground-up basis.

This is not a hidden implementation detail. It determines the likelihood. Deductibles are treated as left-truncation thresholds: claims below them are assumed absent from the fitted sample. Finite limits are treated as right-censoring thresholds: a claim recorded at the limit is known only to have reached or exceeded that threshold.

## Installation

Until the first CRAN release, install from the public GitHub repository:

```r
install.packages("remotes") # only needed once
remotes::install_github("gs-actuary/actuarialfitdist")
```

After CRAN acceptance, install the released version with
`install.packages("actuarialfitdist")`. The package works on standard R
installations; it does not compile C or C++ source code.

## Quick start: three common workflows

The fitters require **ground-up claim severity**, not net insurer payment.
Policy terms are optional: absent `deductible` means zero truncation and absent
`limit` means no censoring. A column such as Coverage A is a **scaling variable**,
not automatically a ground-up censoring limit. With finite ground-up limits,
observations exactly at the limit are treated as right-censored.

```r
library(actuarialfitdist)
set.seed(104)
claims <- data.frame(loss = rlnorm(400, log(15000), 0.9))

# 1. Single-family fit, no deductible or censoring limit
f <- fit_severity(claims, loss = "loss", distribution = "lognormal")
summary(f)
plot_fit(f, x_scale = "log10", xlim = c(500, 200000))

# 2. Candidate comparison: inspect the fit objects AND the comparison table
candidates <- fit_severity_candidates(claims, loss = "loss",
  distributions = c("lognormal", "gamma", "weibull"), hessian = FALSE)
candidates$comparison[order(candidates$comparison$AIC), ]
plot_candidate_fits(candidates, type = "histogram")
head(candidate_plot_data(candidates, output = "plot_ready"))

# 3. Translate fits to deductible/payment-limit factors
severity_factors(candidates, deductible = c(500, 1000),
  limit = c(25000, 50000), base_deductible = 500,
  base_limit = 25000)

# When inputs describe matched scenarios, avoid a Cartesian product:
severity_factors(f, deductible = c(500, 1000),
  limit = c(25000, 50000), grid = "paired")
```

**Anomalies:** by default the fitter warns and excludes rows with invalid claim
information (e.g., ground-up loss at/below deductible or above a finite censoring
limit). Inspect `fit$excluded_rows` (original positional row numbers) and
`fit$excluded_details` (reasons). Use `invalid_rows = "error"` to stop instead.
For candidate fits the audit is on both the collection and each successful fit.

**Splices:** empirical observed-loss percentiles can be tied at a policy limit.
The threshold-search table contains body counts, uncensored tail counts,
convergence, and rejection reasons. Consider the coverage structure before
lowering `min_tail_exact`; convergence does not establish tail stability.

**Next steps:** consult `vignette("actuarialfitdist")` and
`vignette("candidate-comparisons")`, then `?fit_severity_candidates`,
`?plot_candidate_fits` and `?severity_factors` for runnable examples.

## Main features

* Maximum-likelihood severity fitting with deductible truncation and limit censoring.
* Weibull, gamma, lognormal, Pareto II/Lomax, Burr XII, normal, exponential, log-logistic, inverse Gaussian, and generalized Pareto families.
* Optional severity scaling by Coverage A, TIV, vehicle value, or any analyst-supplied numeric scaling variable.
* Exact zero scale intercept when `scale_intercept = FALSE`; optional estimated intercept otherwise.
* Likelihood/frequency weights and fixed parameters.
* Candidate-distribution comparison using log-likelihood, AIC, BIC, convergence, diagnostics, and downstream factor stability.
* Spliced body-tail models with fixed or searched thresholds and no required density continuity.
* Limited expected severities, LERs, deductible factors, ILFs, combined rating factors, and valuation/depreciation adjustments.
* Deterministic analytic and optional simulation-based goodness-of-fit diagnostics that reproduce the actual deductible, limit, and scaling structure.
* Diagnostics of the assumed severity scaling relationship and of fit within selected loss layers.
* Numerical Hessian covariance estimates and basic case/parametric bootstrap support.
* `ggplot2` and `scales` graphics plus public data-building functions for fully customized charts and Plotly workflows.

## Ground-up observation model

For an exact observed ground-up loss `x`, deductible `d`, and fitted severity distribution with density `f()` and survival function `S()`, the row log-likelihood is

```text
log(f(x)) - log(S(d))
```

For a claim censored at a finite ground-up limit `u`, it is

```text
log(S(u)) - log(S(d))
```

If a row has likelihood weight `w`, its contribution is

```text
w * row_log_likelihood
```

Thus a weight of 2 is equivalent to two identical observations. Equivalently, the row likelihood is raised to the power `w` before likelihoods are multiplied.

## Severity scaling

If `scale_by` is supplied, the package uses a positive severity scale

```text
s_i = scale_coef * scale_by_i + scale_intercept
```

with `scale_intercept = 0` exactly when `scale_intercept = FALSE`.

Supported families are parameterized so that their finite mean severity is proportional to `s_i`. For example:

* Weibull: native scale is `s_i`, so the mean is `s_i * Gamma(1 + 1 / shape)`.
* Lognormal: `meanlog = log(s_i)`, so the mean is `s_i * exp(sdlog^2 / 2)`.
* Gamma: native scale is `s_i`, so the mean is `shape * s_i`.

The scaling variable is intentionally generic. In homeowners it may be Coverage A; elsewhere it may be TIV, vehicle value, or a user-created transformation. The package does not assign business meaning to the column.

### Distribution parameterization under scaling

The user does not need to choose a statistical parameterization. The package documents and applies one internally so the fitted mean is linear in the supplied scale variable whenever the family mean exists.

| Family | Internal scaling convention | Mean relationship |
|---|---|---|
| Weibull | native `scale = s_i` | `s_i * Gamma(1 + 1/shape)` |
| Gamma | native `scale = s_i` | `shape * s_i` |
| Lognormal | `meanlog = log(s_i)` | `s_i * exp(sdlog^2 / 2)` |
| Pareto II / Lomax | native `scale = s_i` | `s_i / (shape - 1)` when `shape > 1` |
| Burr XII | native `scale = s_i` | proportional to `s_i` when the mean exists |
| Exponential | mean/scale `= s_i` | `s_i` |
| Log-logistic | native `scale = s_i` | proportional to `s_i` when `shape > 1` |
| Inverse Gaussian | `mean = s_i`, `lambda = shape * s_i` | `s_i` |
| Generalized Pareto | native `scale = s_i` | `s_i / (1 - xi)` when `xi < 1` |
| Normal | `mean = s_i`, `sd = cv * s_i` | `s_i` before conditioning on positive severity |

The normal family is mainly a comparator for positive claim data. The generalized Pareto is often most natural as a tail family in a spliced model, but whole-loss fitting is permitted.

## Quick example

```r
library(actuarialfitdist)

set.seed(1)
n <- 2000
latent <- rlnorm(n, log(12000), 0.9)
ded <- sample(c(500, 1000, 2500), n, TRUE)
lim <- rep(50000, n)
keep <- latent > ded
claims <- data.frame(
  loss = pmin(latent[keep], lim[keep]),
  deductible = ded[keep],
  limit = lim[keep]
)

fit <- fit_severity(
  claims,
  loss = "loss",
  deductible = "deductible",
  limit = "limit",
  distribution = "lognormal"
)

summary(fit)

severity_factors(
  fit,
  deductible = c(500, 1000, 2500),
  limit = c(25000, 50000, 100000),
  base_deductible = 500,
  base_limit = 50000
)
```

## Candidate comparison

```r
fits <- fit_severity_candidates(
  claims,
  loss = "loss",
  deductible = "deductible",
  limit = "limit",
  distributions = c("lognormal", "weibull", "gamma", "burr", "loglogistic")
)

fits$comparison

severity_factors(
  fits,
  deductible = c(500, 1000, 2500),
  limit = c(25000, 50000, 100000),
  base_deductible = 500,
  base_limit = 50000
)
```

The package does not automatically declare a statistical winner. Tail behavior, fit within the business-relevant layer, and sensitivity of the resulting rating factors all matter.

## Spliced body-tail models

```r
splice_fit <- fit_spliced_severity(
  claims,
  loss = "loss",
  deductible = "deductible",
  limit = "limit",
  body = "lognormal",
  tail = "pareto2",
  threshold_method = "search"
)

splice_fit$threshold
splice_fit$threshold_search
```

A spliced model normalizes the body and tail families on their respective sides of the threshold and estimates the body probability mass. Density continuity is not imposed. A searched threshold is selected by profile AIC over the candidate grid and is counted as an estimated model degree of freedom in the reported information criteria.

## Valuation and depreciation

Depreciation is a payment transformation, not a fitting-basis detector. Historical losses should already be on the analyst's selected ground-up basis.

For factor generation, `valuation_factor = 1` represents full replacement-cost treatment. A value such as 0.40 means that the ground-up replacement-cost severity is multiplied by 40 percent before applying the deductible and payment limit.

```r
severity_factors(
  fit,
  deductible = 2500,
  limit = 25000,
  valuation_factor = c(1, 0.8, 0.6, 0.4, 0.2),
  base_deductible = 2500,
  base_limit = 25000,
  base_valuation_factor = 1
)
```

## Diagnostics and plotting data

```r
plot_fit(fit, type = "density")

plot_dat <- fit_plot_data(fit, nsim = 20)
head(plot_dat)
```

`fit_plot_data()` is public specifically so analysts can build their own `ggplot2`, Plotly, or reporting layer without reimplementing the observation logic.

See the vignette and `inst/examples/` for property-damage limits, collision deductibles, homeowners scaling, spliced models, weights, parameter fixing, depreciation, bootstrap inference, and Plotly examples.


## Faster candidate fitting and plot data (0.2.2)

With `scale_intercept = TRUE`, BFGS now automatically scales the additive
intercept coordinate by `max(1000, 0.10 * median(loss))`. That affects only
numerical optimization, not the fitted severity relationship. Supply your own
`control = list(parscale = c(...))` to override the heuristic; the order is
reported in a fitted model's `optim_par` vector. A fitted model also includes
`optimizer_control`, `optimizer_counts`, and `elapsed_seconds`. Candidate
comparisons include separate timing and optimizer-evaluation columns for each
family.

The default plots are **analytic**, comparing the actual portfolio to the
expected distribution of observed claim amounts under the fitted model,
conditional on deductibles and capped at censoring limits. This avoids
unnecessary simulation and costly inverse-Gaussian numerical quantiles.
For alternative simulation-based diagnostics, set `method = "simulation"`.
Analytic density summaries are **bin-based probability densities** (with mass
at finite censoring limits included in the appropriate bins), not smoothed
continuous PDFs. For logarithmic axes the densities are per log10 severity,
so observed and modeled lines/bars use matching units.

```r
set.seed(101)
d <- data.frame(coverage_amount = rep(seq(200000, 600000, by = 50000), each = 100))
d$deductible <- sample(c(1000, 2500), nrow(d), TRUE)
d$loss <- rweibull(nrow(d), shape = 0.8, scale = 0.15 * d$coverage_amount)
d <- subset(d, loss > deductible)

fits <- fit_severity_candidates(
  d, loss = "loss", deductible = "deductible",
  scale_by = "coverage_amount", scale_intercept = TRUE,
  distributions = c("weibull", "gamma", "lognormal"), hessian = FALSE)
fits$comparison[, c("distribution", "AIC", "elapsed_seconds",
                    "function_evaluations", "convergence_code")]

# Default: fast and deterministic, readable axis labels in facets.
plot_candidate_fits(fits, type = "density", x_text_angle = 45,
                    x_text_size = 8, xlim = c(1000, 250000))

# The *exact* plotted summaries are public and editable:
plot_df <- candidate_plot_data(fits, output = "plot_ready",
  type = "density", xlim = c(1000, 250000))
head(plot_df)
# Build a custom ggplot or Plotly chart from plot_df$x and plot_df$value.

# Backwards-compatible individual records, for simulation/diagnostics:
raw <- candidate_plot_data(fits, nsim = 1, output = "raw", seed = 4)

# For the scaling diagnostic:
scale_df <- scaling_plot_data(fits$fits$weibull,
  bins = 5, nsim = 1, output = "plot_ready")
```

The public builders `fit_plot_data()`, `candidate_plot_data()` and
`scaling_plot_data()` return the same data consumed by their corresponding
plot functions. `plot_fit()`, `plot_candidate_fits()` and `plot_scaling_fit()`
expose axis-label angle, font sizes and optional x tick positions. Plot
objects remain ggplots, so arbitrary additional ggplot2 customization is
supported.
