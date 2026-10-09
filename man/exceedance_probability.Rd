\name{exceedance_probability}
\alias{exceedance_probability}
\title{Exceedance probabilities from a fitted model}
\description{Evaluate probability of ground-up claim severity exceeding a specified threshold.}
\usage{
exceedance_probability(object, threshold, scale_value = NULL)
}
\arguments{
\item{object}{Fitted severity model or candidate-fits object as appropriate.}
\item{threshold}{Positive ground-up severity splice threshold; when searching, a vector may define candidate values.}
\item{scale_value}{Scale of the insured value for a fit with `scale_by`.}
}
\details{Input losses must be positive ground-up severities. Deductibles cause left truncation; limits cause right censoring. For fitted payment factors, limit instead refers to a payment cap applied after deductible. Data that violate fitted observation assumptions are excluded with original row indices retained. See also the package vignettes.}
\value{A fitted model, comparison, data frame, numeric result or ggplot as described.}
\examples{
set.seed(123)
d <- data.frame(loss = rlnorm(120, log(10000), 0.8))
fit <- fit_severity(d, "loss", distribution = "lognormal", hessian = FALSE)
exceedance_probability(fit, 100000)

}
