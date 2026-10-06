# 09_real_world_danish_fire.R
# Real-world severity example: Danish fire insurance claims.
#
# The data were collected at Copenhagen Reinsurance and contain 2,167 fire
# losses from 1980-1990, inflation-adjusted to 1985 values and expressed in
# millions of Danish Krone. The commonly distributed univariate series contains
# losses above 1 million DKK, so this script models the data as ground-up losses
# left-truncated at 1 mDKK.
#
# Data source: fitdistrplus::danishuni (Suggested package).

library(actuarialfitdist)

if (!requireNamespace("fitdistrplus", quietly = TRUE)) {
  stop("Install the suggested package 'fitdistrplus' to run this real-world example.")
}

data("danishuni", package = "fitdistrplus", envir = environment())
dan <- danishuni

claims <- data.frame(
  loss = dan$Loss,
  deductible = 1,
  limit = Inf
)

# Single-family fit.
lognormal_fit <- fit_severity(
  claims, "loss", "deductible", "limit", "lognormal"
)
print(summary(lognormal_fit))

# Spliced body-tail fit with a searched threshold.
spliced_fit <- fit_spliced_severity(
  claims, "loss", "deductible", "limit",
  body = "lognormal",
  tail = "pareto2",
  threshold_method = "search",
  threshold_probs = c(0.65, 0.95),
  threshold_grid = 15
)
print(spliced_fit)
print(spliced_fit$threshold_search)

# Compare actuarial limits output from the two plausible models.
limits <- c(2, 5, 10, 20, 50, 100)
print(severity_factors(
  lognormal_fit,
  deductible = 1,
  limit = limits,
  base_deductible = 1,
  base_limit = 5
))
print(severity_factors(
  spliced_fit,
  deductible = 1,
  limit = limits,
  base_deductible = 1,
  base_limit = 5
))

# Censoring-aware diagnostics. Here there is no policy-limit censoring, but the
# simulation still respects the dataset's 1 mDKK truncation threshold.
plot_fit(lognormal_fit, type = "survival", nsim = 20, seed = 1)
plot_fit(spliced_fit, type = "survival", nsim = 20, seed = 1)
