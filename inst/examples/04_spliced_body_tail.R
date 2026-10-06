# 04_spliced_body_tail.R
# Fit body-tail models using fixed and searched thresholds.

library(actuarialfitdist)

set.seed(104)
n <- 6000
t <- 30000
is_body <- runif(n) < 0.9

Fb <- plnorm(t, log(9000), 0.75)
body <- qlnorm(runif(n, 0, Fb), log(9000), 0.75)

alpha <- 2.0
s <- 18000
Ft <- 1 - (1 + t / s)^(-alpha)
tail <- s * ((1 - runif(n, Ft, 1))^(-1 / alpha) - 1)

latent <- ifelse(is_body, body, tail)
ded <- sample(c(0, 1000, 2500), n, TRUE)
lim <- sample(c(50000, 100000, 250000, Inf), n, TRUE)
keep <- latent > ded
claims <- data.frame(loss = pmin(latent[keep], lim[keep]), deductible = ded[keep], limit = lim[keep])

fixed_threshold_fit <- fit_spliced_severity(
  claims, "loss", "deductible", "limit",
  body = "lognormal", tail = "pareto2",
  threshold = 30000, threshold_method = "fixed"
)

searched_fit <- fit_spliced_severity(
  claims, "loss", "deductible", "limit",
  body = "lognormal", tail = "pareto2",
  threshold_method = "search",
  threshold_probs = c(0.60, 0.95),
  threshold_grid = 15
)

print(searched_fit)
print(searched_fit$threshold_search)

severity_factors(
  searched_fit,
  deductible = c(0, 1000, 5000),
  limit = c(25000, 50000, 100000, 250000),
  base_deductible = 0,
  base_limit = 50000
)
