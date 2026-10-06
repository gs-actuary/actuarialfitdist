test_that("candidate fits can be taken through factor generation", {
  set.seed(70)
  x <- rlnorm(1500, log(8000), 0.7)
  dat <- data.frame(loss = x, deductible = 0, limit = Inf)
  expect_no_warning({
    fits <- fit_severity_candidates(dat, "loss", "deductible", "limit",
                                    distributions = c("lognormal", "weibull"), hessian = FALSE)
  })
  expect_s3_class(fits, "actuarialfitdist_candidates")
  out <- severity_factors(fits, deductible = c(0, 1000), limit = c(10000, 50000),
                          base_deductible = 0, base_limit = 10000)
  expect_true(all(c("lognormal", "weibull") %in% out$distribution))
})
