# 02_candidate_horserace.R
# Statistical comparison plus business-factor sensitivity.

library(actuarialfitdist)

set.seed(102)
n <- 5000
latent <- rweibull(n, shape = 1.45, scale = 15000)
ded <- sample(c(0, 500, 1000, 2500), n, TRUE)
lim <- sample(c(25000, 50000, 100000, Inf), n, TRUE)
keep <- latent > ded
claims <- data.frame(loss = pmin(latent[keep], lim[keep]), deductible = ded[keep], limit = lim[keep])

fits <- fit_severity_candidates(
  claims, "loss", "deductible", "limit",
  distributions = c("weibull", "gamma", "lognormal", "burr", "loglogistic", "invgauss")
)

print(fits)

# Long-format endpoint across all successful candidates.
factors <- severity_factors(
  fits,
  deductible = c(0, 1000, 2500),
  limit = c(25000, 50000, 100000, 250000),
  base_deductible = 0,
  base_limit = 50000
)
print(factors)

# Side-by-side comparison of the final rating factor.
compare_factors(
  fits,
  metric = "rating_factor",
  deductible = c(0, 1000, 2500),
  limit = c(25000, 50000, 100000, 250000),
  base_deductible = 0,
  base_limit = 50000
)
