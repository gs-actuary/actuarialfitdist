# 01_single_distribution_fit.R
# Basic ground-up severity fitting with deductible truncation and limit censoring.

library(actuarialfitdist)

set.seed(101)
n <- 5000
latent <- rlnorm(n, log(10000), 0.85)
deductible <- sample(c(500, 1000, 2500), n, TRUE)
limit <- sample(c(25000, 50000, 100000), n, TRUE)
keep <- latent > deductible

claims <- data.frame(
  loss = pmin(latent[keep], limit[keep]),
  deductible = deductible[keep],
  limit = limit[keep]
)

# IMPORTANT: `loss` is ground-up severity, capped only because the claim is
# censored at the policy limit. It is not payment after deductible.
fit <- fit_severity(
  claims,
  loss = "loss",
  deductible = "deductible",
  limit = "limit",
  distribution = "lognormal"
)

print(fit)
summary(fit)
coef(fit)
vcov(fit)
confint(fit)

severity_factors(
  fit,
  deductible = c(500, 1000, 2500, 5000),
  limit = c(25000, 50000, 100000),
  base_deductible = 500,
  base_limit = 50000
)
