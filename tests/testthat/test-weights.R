test_that("frequency weight two matches duplicated observation likelihood", {
  set.seed(10)
  x <- rexp(300, rate = 1 / 5000)
  dat <- data.frame(loss = x, deductible = 0, limit = Inf, w = 1)
  dat$w[1] <- 2
  fit_w <- fit_severity(dat, "loss", "deductible", "limit", "exponential", weights = "w")

  dat2 <- rbind(dat[, c("loss", "deductible", "limit")], dat[1, c("loss", "deductible", "limit")])
  fit_dup <- fit_severity(dat2, "loss", "deductible", "limit", "exponential")

  expect_equal(unname(coef(fit_w)["scale"]), unname(coef(fit_dup)["scale"]), tolerance = 1e-5)
  expect_equal(as.numeric(logLik(fit_w)), as.numeric(logLik(fit_dup)), tolerance = 1e-5)
})
