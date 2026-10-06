test_that("exponential fit and factors recover simple formulas", {
  set.seed(1)
  x <- rexp(5000, rate = 1 / 10000)
  dat <- data.frame(loss = x, deductible = 0, limit = Inf)
  fit <- fit_severity(dat, "loss", "deductible", "limit", "exponential")
  s <- unname(coef(fit)["scale"])

  lev <- limited_expected_value(fit, 5000)
  expected_lev <- s * (1 - exp(-5000 / s))
  expect_equal(lev, expected_lev, tolerance = 1e-4)

  ler <- loss_elimination_ratio(fit, 1000)
  expected_ler <- 1 - exp(-1000 / s)
  expect_equal(ler, expected_ler, tolerance = 1e-4)
})

test_that("valuation factor is applied before deductible", {
  set.seed(2)
  x <- rexp(3000, rate = 1 / 10000)
  dat <- data.frame(loss = x, deductible = 0, limit = Inf)
  fit <- fit_severity(dat, "loss", "deductible", "limit", "exponential")
  s <- unname(coef(fit)["scale"])

  out <- severity_factors(
    fit, deductible = 1000, limit = Inf,
    valuation_factor = 0.5,
    base_deductible = 0, base_limit = Inf,
    base_valuation_factor = 1
  )
  # For X ~ Exp(scale=s), E[(0.5X - 1000)+] = 0.5*s*exp(-2000/s).
  expected <- 0.5 * s * exp(-2000 / s)
  expect_equal(out$expected_payment, expected, tolerance = 1e-4)
})
