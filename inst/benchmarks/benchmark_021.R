# Optional (not run during package checks): a synthetic 100,000-claim benchmark.
# Copy this file into an R session with actuarialfitdist installed and run it.
# No employer or other proprietary data are needed.
set.seed(222)
n <- 100000L
coverage <- sample(seq(100000, 650000, by = 25000), n, TRUE)
ded <- sample(c(1000, 2500, 5000, 10000), n, TRUE)
x <- stats::rweibull(n, shape = .6, scale = .3 * coverage)
dat <- data.frame(coverage_amount = coverage, deductible = ded, loss = x)
dat <- dat[dat$loss > dat$ded, , drop = FALSE]

cat("Rows:", nrow(dat), "\n")
fit_sec <- system.time({
  fits <- actuarialfitdist::fit_severity_candidates(dat, "loss", "deductible",
    distributions = c("weibull", "gamma", "lognormal", "pareto2", "burr",
      "exponential", "loglogistic"), scale_by = "coverage_amount",
    scale_intercept = TRUE, hessian = FALSE)
})[["elapsed"]]
print(fits$comparison[, c("distribution", "elapsed_seconds",
                          "function_evaluations", "convergence_code", "AIC")])
plot_sec <- system.time({
  plot <- actuarialfitdist::plot_candidate_fits(fits, type = "density",
    xlim = c(5000, 5000000), x_text_angle = 45, x_text_size = 8)
})[["elapsed"]]
print(c(fitting_seconds = fit_sec, plot_creation_seconds = plot_sec))
# Verify the plotting data are fully accessible to customized graphics.
ready <- actuarialfitdist::candidate_plot_data(fits, output = "plot_ready",
  type = "density", xlim = c(5000, 5000000))
print(head(ready))
