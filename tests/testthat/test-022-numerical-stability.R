test_that("a free scale intercept cannot fit worse than a nested zero intercept", {
  set.seed(2201)
  n <- 1800L
  cov <- rep(c(150000, 250000, 350000, 450000, 600000), length.out = n)
  ded <- rep(c(1000, 2500, 5000), length.out = n)
  ground_up <- stats::rweibull(n, shape = 0.6, scale = .3 * cov)
  dat <- data.frame(loss = ground_up, cov = cov, deductible = ded)
  dat <- dat[dat$loss > dat$deductible, , drop = FALSE]
  base <- fit_severity(dat, "loss", deductible = "deductible",
                       distribution = "weibull", scale_by = "cov",
                       hessian = FALSE)
  free <- fit_severity(dat, "loss", deductible = "deductible",
                       distribution = "weibull", scale_by = "cov",
                       scale_intercept = TRUE, hessian = FALSE)
  expect_equal(free$convergence, 0L)
  expect_gte(free$logLik + 0.1, base$logLik)
  expect_gt(unname(free$coefficients[["scale_coef"]]), .05)
  expect_true(all(is.finite(free$loglik_contributions)))
})

test_that("extreme Burr shapes cannot yield a spurious zero log likelihood", {
  set.seed(2202)
  dat <- data.frame(cov = rep(c(200000, 300000, 500000), each = 200))
  dat$loss <- stats::rweibull(nrow(dat), .7, .3 * dat$cov)
  dat$ded <- 1500
  dat <- dat[dat$loss > dat$ded, , drop = FALSE]
  params <- list(scale_coef = 1.730258e179, shape1 = 4.679811e-27,
                 shape2 = 2.565549e118)
  internal <- actuarialfitdist:::.afd_single_components(
    "burr", params, dat$loss, dat$ded, rep(Inf, nrow(dat)), dat$cov)
  expect_null(internal)
  cand <- fit_severity_candidates(dat, "loss", deductible = "ded",
    scale_by = "cov", distributions = c("weibull", "burr"), hessian = FALSE)
  expect_true(is.finite(cand$comparison$logLik[1]))
  if (is.finite(cand$comparison$logLik[2])) {
    expect_lt(cand$comparison$logLik[2], -100)
    expect_gt(cand$comparison$AIC[2], 100)
  } else {
    expect_false(cand$comparison$converged[2])
    expect_true(is.na(cand$comparison$AIC[2]))
  }
})

test_that("analytic plot grid is stable at extreme truncation probabilities", {
  dat <- data.frame(loss = rep(1.1e9, 40), ded = rep(1e9, 40))
  fit <- fit_severity(dat, "loss", deductible = "ded",
    distribution = "lognormal", fixed = list(scale = 10000, sdlog = 1),
    hessian = FALSE)
  ready <- fit_plot_data(fit, output = "plot_ready", type = "cdf",
                         x_scale = "linear", xlim = c(1e9, 1.4e9), points = 10)
  modeled <- ready[ready$source == "modeled", ]
  expect_true(all(is.finite(modeled$value)))
  expect_true(all(modeled$value >= 0 & modeled$value <= 1))
  expect_true(all(diff(modeled$value) >= -1e-8))
  expect_gt(max(modeled$value), 0)
})

test_that("invalid fixed shape values are not reported as fitted models", {
  dat <- data.frame(loss = seq(5000, 14000, length.out = 50), ded = 1000)
  expect_error(
    fit_severity(dat, "loss", "ded", distribution = "burr",
      fixed = list(shape1 = 1e-27, shape2 = 1e118), hessian = FALSE),
    "invalid|admissible|converged"
  )
})
