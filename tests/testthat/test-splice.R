test_that("spliced model fits with fixed and searched thresholds", {
  set.seed(50)
  n <- 2500
  t <- 20000
  is_body <- runif(n) < 0.9
  Fb <- plnorm(t, log(7000), 0.7)
  body <- qlnorm(runif(n, 0, Fb), log(7000), 0.7)
  alpha <- 2.2; s <- 12000
  Ft <- 1 - (1 + t / s)^(-alpha)
  tail <- s * ((1 - runif(n, Ft, 1))^(-1 / alpha) - 1)
  x <- ifelse(is_body, body, tail)
  dat <- data.frame(loss = x, deductible = 0, limit = Inf)

  fixed_fit <- fit_spliced_severity(dat, "loss", "deductible", "limit",
                                    body = "lognormal", tail = "pareto2",
                                    threshold = t, threshold_method = "fixed")
  expect_s3_class(fixed_fit, "actuarialfitdist_fit")
  expect_equal(fixed_fit$threshold, t)

  search_fit <- fit_spliced_severity(dat, "loss", "deductible", "limit",
                                     body = "lognormal", tail = "pareto2",
                                     threshold_method = "search", threshold_grid = 5)
  expect_true(nrow(search_fit$threshold_search) >= 2)
  expect_true(is.finite(search_fit$threshold))
})
