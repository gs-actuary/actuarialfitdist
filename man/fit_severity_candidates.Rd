\name{fit_severity_candidates}
\alias{fit_severity_candidates}
\title{Fit and Compare Candidate Severity Distributions}
\usage{
fit_severity_candidates(data, loss, deductible, limit,
  distributions = c("weibull", "gamma", "lognormal", "pareto2", "burr",
    "normal", "exponential", "loglogistic", "invgauss", "gpd"),
  scale_by = NULL, scale_intercept = FALSE, weights = NULL,
  fixed = list(), control = list(), hessian = TRUE)
}
\arguments{
  \item{data, loss, deductible, limit, scale_by, scale_intercept, weights, control, hessian}{See \code{\link{fit_severity}}. The same ground-up-loss assumption applies.}
  \item{distributions}{Character vector of candidate families.}
  \item{fixed}{Optional named list of per-family fixed-parameter lists, for example \code{list(gamma = list(shape = 2))}.}
}
\description{
Fits multiple supported severity families to the same claim data and returns both fitted models and a comparison table.
}
\details{
The comparison reports log-likelihood, AIC, BIC, convergence, and parameter counts. No winner is selected automatically. Actuarial model selection may also consider tail behavior, layer-specific diagnostics, and the stability of downstream deductible and limit factors across plausible families.
}
\value{An object of class \code{actuarialfitdist_candidates} with components \code{fits} and \code{comparison}.}
