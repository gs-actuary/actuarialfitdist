test_that("fixed parameters remain fixed", {
  set.seed(40)
  x <- rgamma(3000, shape = 2.5, scale = 4000)
  dat <- data.frame(loss = x, deductible = 0, limit = Inf)
  fit <- fit_severity(dat, "loss", "deductible", "limit", "gamma", fixed = list(shape = 2.5))
  expect_equal(unname(coef(fit)["shape"]), 2.5)
})
