\name{compare_factors}
\alias{compare_factors}
\title{Compare factor outputs side by side}
\description{Compare one selected actuarial factor metric across successfully converged candidate distributions.}
\usage{
compare_factors(object, metric = "rating_factor", ...)
}
\arguments{
\item{object}{Fitted severity model or candidate-fits object as appropriate.}
\item{metric}{One severity_factors output column to pivot across distributions.}
\item{...}{Additional arguments passed to the corresponding method.}
}
\details{Input losses must be positive ground-up severities. Deductibles cause left truncation; limits cause right censoring. For fitted payment factors, limit instead refers to a payment cap applied after deductible. Data that violate fitted observation assumptions are excluded with original row indices retained. See also the package vignettes.}
\value{A fitted model, comparison, data frame, numeric result or ggplot as described.}
\examples{
set.seed(123)
d <- data.frame(loss = rlnorm(120, log(10000), 0.8))
candidates <- fit_severity_candidates(d, "loss",
  distributions = c("lognormal", "gamma"), hessian = FALSE)
compare_factors(candidates, deductible = 1000, limit = c(25000, 50000))

}
