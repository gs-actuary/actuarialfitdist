\name{bootstrap_fit}
\alias{bootstrap_fit}
\title{Bootstrap a Fitted Severity Model}
\usage{
bootstrap_fit(object, R = 200, type = c("parametric", "case"),
  seed = NULL, refit_threshold = FALSE)
}
\arguments{
  \item{object}{Fitted actuarialfitdist model.}
  \item{R}{Number of bootstrap replicates.}
  \item{type}{\code{"parametric"} or \code{"case"}.}
  \item{seed}{Optional random seed.}
  \item{refit_threshold}{For a spliced model originally fit by threshold search, whether each bootstrap replicate should repeat threshold search. The default reuses the selected threshold.}
}
\details{
The parametric bootstrap preserves the fitted portfolio's row-level deductible, limit, scale variable, and weight structure. It simulates from each row's fitted severity distribution conditional on exceeding that row's deductible, applies right censoring at the row's limit, and refits the model.

The case bootstrap resamples observed claim rows with replacement. This function is intentionally lightweight; Hessian-based \code{vcov()} and \code{confint()} remain the fast default inference tools.
}
\value{An \code{actuarialfitdist_bootstrap} object containing the replicate estimates and success rate.}
