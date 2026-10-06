# actuarialfitdist

`actuarialfitdist` is an R package for actuarial severity distribution fitting and the limit/deductible rating analyses that follow from those fits.

The package is deliberately opinionated about one important data assumption:

> **The fitting functions expect ground-up claim severity.**
>
> The loss column is not an insurer payment, an amount net of deductible, an excess-of-deductible amount, or an ACV/depreciated payment unless the analyst has first transformed the data onto the intended common ground-up basis.

This is not a hidden implementation detail. It determines the likelihood. Deductibles are treated as left-truncation thresholds: claims below them are assumed absent from the fitted sample. Finite limits are treated as right-censoring thresholds: a claim recorded at the limit is known only to have reached or exceeded that threshold.

## Main features

* Maximum-likelihood severity fitting with deductible truncation and limit censoring.
* Weibull, gamma, lognormal, Pareto II/Lomax, Burr XII, normal, exponential, log-logistic, inverse Gaussian, and generalized Pareto families.
* Optional severity scaling by Coverage A, TIV, vehicle value, or any analyst-supplied numeric scaling variable.
* Exact zero scale intercept when `scale_intercept = FALSE`; optional estimated intercept otherwise.
* Likelihood/frequency weights and fixed parameters.
* Candidate-distribution comparison using log-likelihood, AIC, BIC, convergence, diagnostics, and downstream factor stability.
* Spliced body-tail models with fixed or searched thresholds and no required density continuity.
* Limited expected severities, LERs, deductible factors, ILFs, combined rating factors, and valuation/depreciation adjustments.
* Simulation-based goodness-of-fit diagnostics that reproduce the actual deductible, limit, and scaling structure.
* Diagnostics of the assumed severity scaling relationship and of fit within selected loss layers.
* Numerical Hessian covariance estimates and basic case/parametric bootstrap support.
* `ggplot2` graphics plus public data-building functions for fully customized charts and Plotly workflows.

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
plot_fit(fit, type = "density", nsim = 20)

plot_dat <- fit_plot_data(fit, nsim = 20)
head(plot_dat)
```

`fit_plot_data()` is public specifically so analysts can build their own `ggplot2`, Plotly, or reporting layer without reimplementing the observation logic.

See the vignette and `inst/examples/` for property-damage limits, collision deductibles, homeowners scaling, spliced models, weights, parameter fixing, depreciation, bootstrap inference, and Plotly examples.
