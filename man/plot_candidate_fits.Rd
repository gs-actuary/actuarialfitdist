\name{plot_candidate_fits}
\alias{plot_candidate_fits}
\title{Compare observed and fitted severity across candidate distributions}
\description{Facet or overlay the empirical observed severity distribution and simulations under each successfully converged candidate.}
\usage{
plot_candidate_fits(object, type = c("histogram", "density", "cdf", "survival"),
                                scale_range = NULL, nsim = 3, seed = NULL,
                                bins = 40, x_scale = c("log10", "linear"),
                                xlim = NULL, facet = TRUE)
}
\arguments{
\item{object}{Fitted severity model or candidate-fits object as appropriate.}
\item{type}{For plots, choose histogram, density, CDF or survival; for bootstrap, choose parametric or case.}
\item{scale_range}{Optional inclusive two-number scale_by range for plotting.}
\item{nsim}{Number of simulated observed portfolios.}
\item{seed}{Optional random seed for repeatable simulations.}
\item{bins}{Number of histogram bins or scale buckets as applicable.}
\item{x_scale}{"log10" (default) or "linear".}
\item{xlim}{Optional finite x-axis viewport limits; does not filter observations.}
\item{facet}{Facet candidates into separate charts (TRUE) or overlay all candidates (FALSE).}
}
\details{Input losses must be positive ground-up severities. Deductibles cause left truncation; limits cause right censoring. For fitted payment factors, limit instead refers to a payment cap applied after deductible. Data that violate fitted observation assumptions are excluded with original row indices retained. See also the package vignettes.}
\value{A fitted model, comparison, data frame, numeric result or ggplot as described.}
\examples{
set.seed(123)
d <- data.frame(loss = rlnorm(120, log(10000), 0.8))
candidates <- fit_severity_candidates(d, "loss",
  distributions = c("lognormal", "gamma"), hessian = FALSE)
plot_candidate_fits(candidates, nsim = 1, x_scale = "log10")

}
