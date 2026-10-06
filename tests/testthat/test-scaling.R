test_that("scale_by recovers proportional Weibull scaling with zero intercept", {
  set.seed(30)
  n <- 9000
  cov <- runif(n, 150000, 800000)
  a <- 0.06
  shape <- 1.4
  latent <- rweibull(n, shape = shape, scale = a * cov)
  d <- sample(c(1000, 2500, 5000), n, TRUE)
  u <- cov
  keep <- latent > d
  dat <- data.frame(loss = pmin(latent[keep], u[keep]), deductible = d[keep], limit = u[keep], cov = cov[keep])
  fit <- fit_severity(dat, "loss", "deductible", "limit", "weibull", scale_by = "cov", scale_intercept = FALSE)
  expect_false("scale_intercept" %in% names(coef(fit)))
  expect_equal(unname(coef(fit)["scale_coef"]), a, tolerance = 0.15)
  expect_equal(unname(coef(fit)["shape"]), shape, tolerance = 0.15)
})

test_that("estimated scale intercept is exposed when requested", {
  set.seed(31)
  n <- 3000
  z <- runif(n, 10, 100)
  latent <- rexp(n, rate = 1 / (20 * z + 100))
  dat <- data.frame(loss = latent, deductible = 0, limit = Inf, z = z)
  fit <- fit_severity(dat, "loss", "deductible", "limit", "exponential", scale_by = "z", scale_intercept = TRUE)
  expect_true("scale_intercept" %in% names(coef(fit)))
})
