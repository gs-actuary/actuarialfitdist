test_that("diagnostic data preserve scale filtering and simulated observation process", {
  set.seed(60)
  n <- 1000
  z <- runif(n, 100, 500)
  x <- rexp(n, rate = 1 / (50 * z))
  d <- rep(100, n)
  u <- rep(20000, n)
  keep <- x > d
  dat <- data.frame(loss = pmin(x[keep], u[keep]), deductible = d[keep], limit = u[keep], z = z[keep])
  fit <- fit_severity(dat, "loss", "deductible", "limit", "exponential", scale_by = "z")
  pd <- fit_plot_data(fit, scale_range = c(200, 300), nsim = 2, seed = 1)
  expect_true(all(pd$scale_value[!is.na(pd$scale_value)] >= 200 & pd$scale_value[!is.na(pd$scale_value)] <= 300))
  expect_true(all(c("actual", "simulated") %in% pd$source))
  sd <- scaling_plot_data(fit, bins = 4, nsim = 2, seed = 1)
  expect_true(all(c("actual_observed_mean", "modeled_observed_mean", "fitted_ground_up_mean") %in% names(sd)))
})
