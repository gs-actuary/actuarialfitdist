test_that("optimizer intercept scaling is automatic, ordered and overridable", {
  set.seed(1201)
  n <- 500
  dat <- data.frame(cov = rep(c(150000, 300000, 450000, 600000), length.out = n))
  dat$ded <- rep(c(1000, 2500), length.out = n)
  dat$loss <- stats::rweibull(n, shape = .8, scale = .22 * dat$cov + 3000)
  dat <- dat[dat$loss > dat$ded, , drop = FALSE]
  f <- fit_severity(dat, "loss", deductible = "ded", distribution = "weibull",
    scale_by = "cov", scale_intercept = TRUE, hessian = FALSE)
  expect_true(unname(f$optimizer_control$parscale[2L]) %in%
    c(max(1000, .1 * stats::median(dat$loss)),
      max(50000, stats::median(dat$loss))))
  expect_equal(names(f$optim_par)[2L], "scale_intercept")
  expect_true(is.finite(f$logLik))
  expect_true(f$coefficients[["scale_coef"]] > 0)
  expect_true(is.list(f$optimizer_counts) || is.numeric(f$optimizer_counts))
  custom <- fit_severity(dat, "loss", deductible = "ded", distribution = "weibull",
    scale_by = "cov", scale_intercept = TRUE, hessian = FALSE,
    control = list(parscale = c(1, 50000, 1)))
  expect_equal(as.numeric(custom$optimizer_control$parscale), c(1, 50000, 1))
  expect_true(is.finite(custom$logLik))
  expect_lt(abs(custom$logLik - f$logLik), 10)
})

test_that("candidate timing diagnostics and fast plots expose their exact data", {
  set.seed(1202)
  dat <- data.frame(loss = stats::rlnorm(240, log(14000), .75))
  cand <- fit_severity_candidates(dat, "loss",
    distributions = c("lognormal", "weibull"), hessian = FALSE)
  expect_true(all(c("elapsed_seconds", "function_evaluations",
    "gradient_evaluations", "convergence_code") %in% names(cand$comparison)))
  expect_true(all(is.finite(cand$comparison$elapsed_seconds)))
  ready <- candidate_plot_data(cand, output = "plot_ready", type = "density",
    x_scale = "log10", xlim = c(3000, 70000))
  # Defaults must be resolved at the API boundary, not inside lapply().
  default_ready <- candidate_plot_data(cand, output = "plot_ready",
    xlim = c(3000, 70000))
  expect_identical(unique(default_ready$source), c("actual", "modeled"))
  expect_equal(nrow(default_ready), 2L * 2L * 160L)
  cdf_ready <- candidate_plot_data(cand, output = "plot_ready",
    type = "cdf", x_scale = "linear", xlim = c(3000, 70000))
  expect_true(all(cdf_ready$value >= 0 & cdf_ready$value <= 1))
  expect_s3_class(plot_candidate_fits(cand), "ggplot")
  expect_true(all(c("distribution", "source", "x", "value", "lower", "upper") %in% names(ready)))
  expect_true(all(is.finite(ready$value) & ready$value >= 0))
  p <- plot_candidate_fits(cand, type = "density", xlim = c(3000, 70000),
    x_text_angle = 60, x_text_size = 8)
  expect_s3_class(p, "ggplot")
  expect_equal(p$data$value, ready$value)
  expect_equal(p$theme$axis.text.x$angle, 60)
  expect_equal(p$theme$axis.text.x$size, 8)
  ready_single <- fit_plot_data(cand$fits$lognormal, output = "plot_ready",
    type = "histogram", bins = 20)
  pf <- plot_fit(cand$fits$lognormal, type = "histogram", bins = 20)
  expect_equal(pf$data$value, ready_single$value)
  expect_equal(nrow(ready_single), 40L)
  expect_true(nrow(candidate_plot_data(cand, nsim = 1)) > nrow(dat))
})

test_that("analytic CDF includes full atom at censoring limit and truncation", {
  set.seed(1203)
  dat <- data.frame(loss = pmin(stats::rlnorm(400, log(10000), .8), 13000),
                    lim = 13000, ded = 1000)
  dat <- dat[dat$loss > dat$ded, , drop = FALSE]
  fit <- fit_severity(dat, "loss", limit = "lim", deductible = "ded",
                      hessian = FALSE)
  ready <- fit_plot_data(fit, output = "plot_ready", type = "cdf",
    x_scale = "linear", xlim = c(1000, 15000), points = 20)
  fitted <- ready[ready$source == "modeled", , drop = FALSE]
  expect_true(all(fitted$value[fitted$x >= 13000] == 1))
  expect_true(all(fitted$value[fitted$x <= 1000] == 0))
  expect_true(all(diff(fitted$value) >= -1e-8))
  histogram <- fit_plot_data(fit, output = "plot_ready", type = "histogram",
    x_scale = "linear", xlim = c(500, 15000), bins = 15)
  expect_true(all(histogram$value >= 0))
})

test_that("scaling plot uses publicly available long data", {
  set.seed(1204)
  dat <- data.frame(cov = rep(c(200000, 300000, 400000, 500000), each = 60))
  dat$loss <- stats::rlnorm(nrow(dat), log(.09 * dat$cov), .5)
  fit <- fit_severity(dat, "loss", scale_by = "cov", hessian = FALSE)
  ready <- scaling_plot_data(fit, bins = 4, nsim = 1, seed = 4,
                             output = "plot_ready")
  p <- plot_scaling_fit(fit, bins = 4, nsim = 1, seed = 4, x_text_angle = 45)
  expect_equal(p$data, ready)
  expect_equal(p$theme$axis.text.x$angle, 45)
})
