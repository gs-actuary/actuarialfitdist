library(actuarialfitdist)
set.seed(11)
dat <- data.frame(loss = rlnorm(500, log(12000), .9))
candidates <- fit_severity_candidates(dat, "loss",
  distributions = c("gamma", "weibull", "lognormal"), hessian = FALSE)
print(candidates$comparison)
print(plot_candidate_fits(candidates, type = "histogram", nsim = 2,
  x_scale = "log10", xlim = c(500, 200000)))
print(head(candidate_plot_data(candidates, nsim = 1)))
print(severity_factors(candidates, deductible = c(500, 1000),
  limit = c(25000, 50000), grid = "paired"))
