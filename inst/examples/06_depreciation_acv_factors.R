# 06_depreciation_acv_factors.R
# Fit replacement-cost ground-up severity, then apply ACV/depreciation assumptions
# at the factor-generation stage.

library(actuarialfitdist)

set.seed(106)
n <- 5000
roof_rcv <- rlnorm(n, log(18000), 0.45)
ded <- sample(c(1000, 2500, 5000), n, TRUE)
keep <- roof_rcv > ded
roof <- data.frame(loss = roof_rcv[keep], deductible = ded[keep], limit = Inf)

# Historical losses are already on a common replacement-cost ground-up basis.
fit <- fit_severity(roof, "loss", "deductible", "limit", "lognormal")

acv <- severity_factors(
  fit,
  deductible = 2500,
  limit = 25000,
  valuation_factor = c(1.00, 0.80, 0.60, 0.40, 0.20),
  base_deductible = 2500,
  base_limit = 25000,
  base_valuation_factor = 1
)

print(acv)

# The package does not infer depreciation from historical claims. If historical
# data are a mixture of ACV and RCV, the analyst first places them on a common
# ground-up basis suitable for fitting.
