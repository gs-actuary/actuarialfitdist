# 08_plotly_diagnostics.R
# Use public diagnostic data to build custom interactive Plotly graphics.
# plotly is Suggested, not Imported.

library(actuarialfitdist)

if (!requireNamespace("plotly", quietly = TRUE)) {
  stop("Install the suggested package 'plotly' to run this example.")
}

set.seed(108)
n <- 4000
coverage <- runif(n, 200000, 800000)
latent <- rweibull(n, shape = 1.4, scale = 0.07 * coverage)
ded <- sample(c(1000, 2500, 5000), n, TRUE)
lim <- coverage
keep <- latent > ded
home <- data.frame(
  loss = pmin(latent[keep], lim[keep]),
  deductible = ded[keep], limit = lim[keep], coverage = coverage[keep]
)
fit <- fit_severity(home, "loss", "deductible", "limit", "weibull", scale_by = "coverage")

# 1. Convert the package's standard ggplot directly.
p <- plot_fit(fit, type = "density", scale_range = c(300000, 450000), nsim = 10, seed = 1)
plotly::ggplotly(p)

# 2. Build a Plotly chart directly from the underlying data.
d <- fit_plot_data(fit, scale_range = c(300000, 450000), nsim = 10, seed = 1)
plotly::plot_ly(
  d,
  x = ~observed_loss,
  color = ~source,
  type = "histogram",
  histnorm = "probability density",
  opacity = 0.55
) |> plotly::layout(barmode = "overlay")

# 3. Scaling diagnostic from exposed summary data.
sd <- scaling_plot_data(fit, bins = 10, nsim = 20, seed = 1)
plotly::plot_ly(sd, x = ~scale_value) |>
  plotly::add_lines(y = ~actual_observed_mean, name = "Actual observed mean") |>
  plotly::add_lines(y = ~modeled_observed_mean, name = "Modeled observed mean") |>
  plotly::add_lines(y = ~fitted_ground_up_mean, name = "Fitted ground-up mean")
