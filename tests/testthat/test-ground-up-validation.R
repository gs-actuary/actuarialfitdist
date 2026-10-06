test_that("ground-up validation rejects net-looking observations below deductible", {
  dat <- data.frame(loss = c(100, 200), deductible = c(500, 500), limit = c(10000, 10000))
  expect_error(
    fit_severity(dat, "loss", "deductible", "limit", "lognormal"),
    "ground-up"
  )
})

test_that("negative likelihood weights are rejected", {
  dat <- data.frame(loss = c(1000, 2000), deductible = 0, limit = Inf, w = c(1, -1))
  expect_error(
    fit_severity(dat, "loss", "deductible", "limit", "exponential", weights = "w"),
    "nonnegative"
  )
})
