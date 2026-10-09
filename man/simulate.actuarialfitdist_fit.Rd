\name{simulate.actuarialfitdist_fit}
\alias{simulate.actuarialfitdist_fit}
\title{Simulate from a fitted severity model}
\description{Simulates either positive ground-up severities or deductible-truncated,
limit-censored observed claims using the fitted model.}
\usage{
\method{simulate}{actuarialfitdist_fit}(
  object, nsim = 1, seed = NULL,
  type = c("observed", "ground_up"),
  newdata = object$data, n = NULL, scale_value = NULL, ...
)
}
\arguments{
\item{object}{Fitted \code{actuarialfitdist_fit} model.}
\item{nsim}{Number of simulated replicates.}
\item{seed}{Optional random seed.}
\item{type}{\code{"observed"} reproduces deductible truncation and limit censoring; \code{"ground_up"} generates positive ground-up severity.}
\item{newdata}{Optional data frame; defaults to fitting observations.}
\item{n}{Number of ground-up draws when \code{type = "ground_up"}. Defaults to the number of rows in \code{newdata}.}
\item{scale_value}{Optional scaling values for ground-up simulation.}
\item{...}{Unused.}
}
\value{Data frame of simulated observed or ground-up severities.}
\examples{
set.seed(1)
d <- data.frame(loss = rlnorm(120, log(10000), 0.8))
fit <- fit_severity(d, "loss", hessian = FALSE)
head(simulate(fit, nsim = 1, seed = 123))
head(simulate(fit, nsim = 1, seed = 123, type = "ground_up", n = 5))
}
