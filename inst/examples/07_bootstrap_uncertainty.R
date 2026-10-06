# 07_bootstrap_uncertainty.R
# Hessian/Wald inference plus lightweight parametric bootstrap.

library(actuarialfitdist)

set.seed(107)
x <- rlnorm(2000, log(12000), 0.8)
dat <- data.frame(loss = x, deductible = 0, limit = Inf)
fit <- fit_severity(dat, "loss", "deductible", "limit", "lognormal")

vcov(fit)
confint(fit)

# Reduce R while developing; use a larger number for final inference.
boot <- bootstrap_fit(fit, R = 100, type = "parametric", seed = 42)
print(boot$success_rate)
print(head(boot$estimates))

# Example percentile interval for a coefficient:
quantile(boot$estimates$scale[boot$estimates$converged], c(0.025, 0.975), na.rm = TRUE)
