test_that("fitting uses deductible truncation and limit censoring", {
  set.seed(20)
  n <- 8000
  latent <- rexp(n, rate = 1 / 7000)
  d <- sample(c(500, 1000, 2500), n, TRUE)
  u <- sample(c(10000, 25000, Inf), n, TRUE)
  keep <- latent > d
  dat <- data.frame(loss = pmin(latent[keep], u[keep]), deductible = d[keep], limit = u[keep])
  fit <- fit_severity(dat, "loss", "deductible", "limit", "exponential")
  expect_true(fit$n_censored > 0)
  expect_equal(unname(coef(fit)["scale"]), 7000, tolerance = 0.10)
})
