\name{plot_fit}
\alias{plot_fit}
\alias{fit_plot_data}
\alias{plot_scaling_fit}
\alias{scaling_plot_data}
\alias{diagnose_fit}
\title{Goodness-of-Fit and Severity-Scaling Diagnostics}
\usage{
plot_fit(object, type = c("histogram", "density", "cdf", "survival"),
  scale_range = NULL, nsim = 20, seed = NULL, bins = 30)

fit_plot_data(object, scale_range = NULL, nsim = 20, seed = NULL)

plot_scaling_fit(object, bins = 10, bin_method = c("quantile", "width"),
  scale_range = NULL, nsim = 50, seed = NULL)

scaling_plot_data(object, bins = 10, bin_method = c("quantile", "width"),
  scale_range = NULL, nsim = 50, seed = NULL)

diagnose_fit(object, lower = 0, upper = Inf, scale_range = NULL,
  nsim = 100, seed = NULL)
}
\arguments{
  \item{object}{A fitted model.}
  \item{type}{Diagnostic plot type.}
  \item{scale_range}{Optional two-element range of the fitted scaling variable.}
  \item{nsim}{Number of simulated observed portfolios.}
  \item{seed}{Optional random seed.}
  \item{bins}{Histogram or scaling-bin count, depending on function.}
  \item{bin_method}{Quantile or equal-width bins for scaling diagnostics.}
  \item{lower, upper}{Observed-severity layer boundaries.}
}
\details{
Severity plots compare actual observed claims with synthetic observed claims generated under the fitted model while preserving each row's deductible, limit, and scaling value. This is intentional: a naive overlay of an unconditional fitted density on deductible-truncated and limit-censored claim data can make a correct fit appear poor and mishandle point masses at limits.

For models with \code{scale_by}, a single fitted distribution does not describe the full portfolio. \code{scale_range} lets the analyst inspect a restricted range while retaining row-level heterogeneity inside that range.

Scaling diagnostics compare actual observed mean severity with modeled observed mean severity by scaling-variable bin. They also expose the fitted underlying ground-up mean relationship. The observed and modeled observed series are on the same censoring/truncation basis; the ground-up series is shown to diagnose the structural scaling assumption.

The data-building functions are public so custom ggplot2 and Plotly graphics can use the correct diagnostic data without reimplementing the observation model.
}
\value{The plot functions return ggplot objects. Data functions and \code{diagnose_fit()} return data frames.}
