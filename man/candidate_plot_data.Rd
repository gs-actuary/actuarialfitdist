\name{candidate_plot_data}
\alias{candidate_plot_data}
\title{Data for comparing several candidate severity distributions}
\description{Return the underlying long observed-versus-simulated data for every successful candidate fit.}
\usage{
candidate_plot_data(object, scale_range = NULL, nsim = 3, seed = NULL)
}
\arguments{
\item{object}{Fitted severity model or candidate-fits object as appropriate.}
\item{scale_range}{Optional inclusive two-number scale_by range for plotting.}
\item{nsim}{Number of simulated observed portfolios.}
\item{seed}{Optional random seed for repeatable simulations.}
}
\details{Input losses must be positive ground-up severities. Deductibles cause left truncation; limits cause right censoring. For fitted payment factors, limit instead refers to a payment cap applied after deductible. Data that violate fitted observation assumptions are excluded with original row indices retained. See also the package vignettes.}
\value{A fitted model, comparison, data frame, numeric result or ggplot as described.}
\examples{
set.seed(123)
d <- data.frame(loss = rlnorm(120, log(10000), 0.8))
candidates <- fit_severity_candidates(d, "loss",
  distributions = c("lognormal", "gamma"), hessian = FALSE)
head(candidate_plot_data(candidates, nsim = 1))

}
