\name{actuarialfitdist-package}
\alias{actuarialfitdist}
\alias{actuarialfitdist-package}
\docType{package}
\title{Actuarial Severity Distribution Fitting for Limits and Deductibles}
\description{Fit parametric ground-up claim severity under deductible truncation and limit censoring; compare single and spliced distributions and generate actuarial payment factors.}
\details{Main entry points: \code{fit_severity()}, \code{fit_severity_candidates()}, \code{fit_spliced_severity()}, \code{plot_fit()}, \code{plot_candidate_fits()}, and \code{severity_factors()}. Claims must be on a ground-up basis. See \code{vignette("actuarialfitdist")} and \code{vignette("candidate-comparisons")}.}
\examples{
set.seed(1)
d <- data.frame(loss = rlnorm(150, 9, 0.9))
fit <- fit_severity(d, "loss", distribution = "lognormal", hessian = FALSE)
summary(fit)
}
