\name{scaling_plot_data}
\alias{scaling_plot_data}
\title{Data underlying the scaling relationship diagnostic}
\description{Return data underlying the scaling relationship diagnostic.}
\usage{
scaling_plot_data(object, bins = 10, bin_method = c("quantile", "width"),
                              scale_range = NULL, nsim = 50, seed = NULL)
}
\arguments{
\item{object}{Fitted severity model or candidate-fits object as appropriate.}
\item{bins}{Number of histogram bins or scale buckets as applicable.}
\item{bin_method}{Use `"quantile"` for approximately equal-sized bins or `"width"` for equal-length scale bins.}
\item{scale_range}{Optional inclusive two-number scale_by range for plotting.}
\item{nsim}{Number of simulated observed portfolios.}
\item{seed}{Optional random seed for repeatable simulations.}
}
\details{Input losses must be positive ground-up severities. Deductibles cause left truncation; limits cause right censoring. For fitted payment factors, limit instead refers to a payment cap applied after deductible. Data that violate fitted observation assumptions are excluded with original row indices retained. See also the package vignettes.}
\value{A fitted model, comparison, data frame, numeric result or ggplot as described.}
\examples{
set.seed(2)
d <- data.frame(cov_a = rep(c(200000, 400000, 600000), each = 80))
d$loss <- rlnorm(nrow(d), log(.07*d$cov_a), .8)
f <- fit_severity(d, "loss", scale_by = "cov_a", hessian = FALSE)
scaling_plot_data(f, bins = 3, nsim = 1)

}
