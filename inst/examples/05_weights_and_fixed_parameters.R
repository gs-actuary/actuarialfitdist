# 05_weights_and_fixed_parameters.R
# Frequency-style likelihood weights and judgmentally fixed parameters.

library(actuarialfitdist)

set.seed(105)
x <- rgamma(2000, shape = 2.5, scale = 4000)
claims <- data.frame(
  loss = x,
  deductible = 0,
  limit = Inf,
  frequency_weight = sample(1:4, length(x), replace = TRUE)
)

fit <- fit_severity(
  claims, "loss", "deductible", "limit", "gamma",
  weights = "frequency_weight",
  fixed = list(shape = 2.5)
)

coef(fit)

# Mathematics of the weights:
# weighted log-likelihood = sum_i w_i * log(L_i)
#                          = log(prod_i L_i ^ w_i)
# Therefore w_i = 2 has the same likelihood effect as duplicating row i.
