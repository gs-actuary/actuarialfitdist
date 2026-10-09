test_that("optional terms and anomaly audit work for all three fit entry points", {
  set.seed(905)
  dat <- data.frame(loss = rlnorm(300, log(12000), .9))
  f <- fit_severity(dat, "loss", distribution = "lognormal", hessian = FALSE)
  expect_s3_class(f, "actuarialfitdist_fit")
  expect_length(f$excluded_rows, 0)
  expect_true(all(is.infinite(f$data[[f$columns$limit]])))
  expect_true(all(f$data[[f$columns$deductible]] == 0))
  sim <- simulate(f, nsim = 1, seed = 1, type = "observed", newdata = dat)
  expect_equal(nrow(sim), nrow(dat))

  bad <- dat
  bad$ded <- 0
  bad$lim <- 1e9
  bad$ded[2] <- bad$loss[2] + 1
  bad$loss[4] <- 2e9
  expect_warning(g <- fit_severity(bad, "loss", deductible = "ded", limit = "lim",
    distribution = "lognormal", hessian = FALSE), "excluded")
  expect_equal(g$excluded_rows, c(2L, 4L))
  expect_equal(g$n, nrow(bad) - 2L)
  expect_true(all(c("row", "reason") %in% names(g$excluded_details)))
  expect_error(fit_severity(bad, "loss", deductible = "ded", limit = "lim",
    invalid_rows = "error"), "positions")
  expect_warning(candidates <- fit_severity_candidates(bad, "loss", "ded", "lim",
    distributions = c("lognormal", "weibull"), hessian = FALSE), "excluded")
  expect_equal(candidates$excluded_rows, g$excluded_rows)
  expect_equal(candidates$fits$lognormal$excluded_rows, g$excluded_rows)
  expect_warning(sp <- fit_spliced_severity(bad, "loss", "ded", "lim",
    threshold = 15000, min_tail_exact = 5, hessian = FALSE), "excluded")
  expect_equal(sp$excluded_rows, g$excluded_rows)
})

test_that("analytical expected payments match independent quadrature", {
  set.seed(7)
  d <- data.frame(loss = rlnorm(400, log(14000), .95))
  for (family in c("lognormal", "gamma", "weibull", "exponential")) {
    f <- fit_severity(d, "loss", distribution = family, hessian = FALSE)
    for (dd in c(0, 1000, 10000)) for (ll in c(25000, 50000)) {
      target <- expected_payment(f, deductible = dd, limit = ll)
      check <- actuarialfitdist:::.afd_expected_payment_one(f, dd, ll, NULL, 1)
      expect_equal(target, check, tolerance = 1e-5,
        info = paste(family, "ded", dd, "lim", ll))
    }
  }
})

test_that("factor scenario pairing is positional and preserves factors", {
  set.seed(10)
  dat <- data.frame(loss = rexp(400, 1/10000))
  f <- fit_severity(dat, "loss", distribution = "exponential", hessian = FALSE)
  a <- severity_factors(f, deductible = c(500, 1000), limit = c(25000, 50000))
  b <- severity_factors(f, deductible = c(500, 1000), limit = c(25000, 50000), grid = "paired")
  expect_equal(nrow(a), 4L)
  expect_equal(nrow(b), 2L)
  for (i in seq_len(nrow(b))) {
    j <- which(a$deductible == b$deductible[i] & a$limit == b$limit[i])
    expect_equal(b$expected_payment[i], a$expected_payment[j], tolerance = 1e-8)
  }
})

test_that("spliced threshold search rejects tied censored maxima with diagnostics", {
  set.seed(11)
  x <- rlnorm(450, log(14000), .85)
  dat <- data.frame(loss = pmin(x, 25000), lim = 25000)
  fit <- fit_spliced_severity(dat, "loss", limit = "lim",
    threshold_method = "search", threshold_probs = c(.4, .9),
    threshold_grid = 6, min_tail_exact = 10, hessian = FALSE)
  expect_true(all(c("n_exact_tail", "converged", "reason", "tail_warning") %in%
    names(fit$threshold_search)))
  expect_true(any(!fit$threshold_search$converged))
})

test_that("candidate plots and log axis work", {
  set.seed(2)
  dat <- data.frame(loss = rlnorm(220, log(12000), .75))
  c <- fit_severity_candidates(dat, "loss", distributions = c("lognormal", "gamma"),
    hessian = FALSE)
  expect_true(nrow(candidate_plot_data(c, nsim = 1)) > nrow(dat))
  expect_s3_class(plot_candidate_fits(c, nsim = 1, x_scale = "log10"), "ggplot")
  expect_s3_class(plot_fit(c$fits$lognormal, nsim = 1, xlim = c(1000, 100000)), "ggplot")
})
