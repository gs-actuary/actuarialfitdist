test_that("below-deductible rows are excluded and audited", {
  set.seed(6)
  dat <- data.frame(loss = c(rexp(100, 1/1000) + 500, 10), deductible = 500)
  expect_warning(fit <- fit_severity(dat, "loss", deductible = "deductible",
                                     distribution = "exponential", hessian = FALSE), "excluded")
  expect_equal(fit$excluded_rows, 101L)
  expect_equal(fit$n, 100L)
  expect_equal(fit$excluded_details$row, 101L)
  expect_error(fit_severity(dat, "loss", "deductible", distribution = "exponential",
                            invalid_rows = "error"), "101")
})

test_that("negative likelihood weights are excluded", {
  set.seed(2)
  dat <- data.frame(loss = rexp(100, 1/1000), w = c(rep(1, 99), -1))
  expect_warning(fit <- fit_severity(dat, "loss", weights = "w", hessian = FALSE), "excluded")
  expect_equal(fit$excluded_rows, 100L)
})
