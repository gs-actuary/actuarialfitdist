\name{severity_quantile}
\alias{severity_quantile}
\title{Severity quantiles from a fitted model}
\description{Calculate ground-up severity quantiles implied by a fitted distribution.}
\usage{
severity_quantile(object, p, scale_value = NULL)
}
\arguments{
\item{object}{Fitted severity model or candidate-fits object as appropriate.}
\item{p}{Probability values between zero and one.}
\item{scale_value}{Scale of the insured value for a fit with `scale_by`.}
}
\details{Input losses must be positive ground-up severities. Deductibles cause left truncation; limits cause right censoring. For fitted payment factors, limit instead refers to a payment cap applied after deductible. Data that violate fitted observation assumptions are excluded with original row indices retained. See also the package vignettes.}
\value{A fitted model, comparison, data frame, numeric result or ggplot as described.}
\examples{
set.seed(123)
d <- data.frame(loss = rlnorm(120, log(10000), 0.8))
fit <- fit_severity(d, "loss", distribution = "lognormal", hessian = FALSE)
severity_quantile(fit, c(.5, .9, .99))

}
