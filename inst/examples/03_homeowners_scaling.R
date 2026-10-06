# 03_homeowners_scaling.R
# Homeowners severity scaling by Coverage A.

library(actuarialfitdist)

set.seed(103)
n <- 8000
coverage_a <- runif(n, 150000, 900000)
shape <- 1.3
scale_coef <- 0.08
latent <- rweibull(n, shape = shape, scale = scale_coef * coverage_a)
ded <- sample(c(1000, 2500, 5000, 10000), n, TRUE)
lim <- coverage_a
keep <- latent > ded

home <- data.frame(
  loss = pmin(latent[keep], lim[keep]),
  deductible = ded[keep],
  limit = lim[keep],
  coverage_a = coverage_a[keep]
)

fit <- fit_severity(
  home, "loss", "deductible", "limit", "weibull",
  scale_by = "coverage_a",
  scale_intercept = FALSE
)

coef(fit)
# scale_intercept is absent and therefore exactly zero.

plot_fit(fit, type = "density", scale_range = c(300000, 450000), nsim = 20, seed = 1)
plot_scaling_fit(fit, bins = 12, nsim = 30, seed = 1)

severity_factors(
  fit,
  scale_value = c(200000, 400000, 600000, 800000),
  deductible = c(1000, 2500, 5000),
  limit = c(100000, 250000, 500000),
  base_deductible = 1000,
  base_limit = 250000
)
