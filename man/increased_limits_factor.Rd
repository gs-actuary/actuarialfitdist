\name{increased_limits_factor}
\alias{increased_limits_factor}
\title{Increased limits factor}
\description{Calculate expected-payment ratio between target and base payment caps after deductible.}
\usage{
increased_limits_factor(object, limit, base_limit, scale_value = NULL,
                                    deductible = 0, valuation_factor = 1)
}
\arguments{
\item{object}{Fitted severity model or candidate-fits object as appropriate.}
\item{limit}{Limit; in FIT functions this is a ground-up censoring threshold column name; in FACTOR functions this is a post-deductible payment cap.}
\item{base_limit}{Base payment cap after deductible.}
\item{scale_value}{Scale of the insured value for a fit with `scale_by`.}
\item{deductible}{Optional column name for left-truncation deductible; NULL means zero.}
\item{valuation_factor}{Multiplicative severity valuation adjustment applied before deductible.}
}
\details{Input losses must be positive ground-up severities. Deductibles cause left truncation; limits cause right censoring. For fitted payment factors, limit instead refers to a payment cap applied after deductible. Data that violate fitted observation assumptions are excluded with original row indices retained. See also the package vignettes.}
\value{A fitted model, comparison, data frame, numeric result or ggplot as described.}
\examples{
set.seed(123)
d <- data.frame(loss = rlnorm(120, log(10000), 0.8))
fit <- fit_severity(d, "loss", distribution = "lognormal", hessian = FALSE)
increased_limits_factor(fit, limit = 50000, base_limit = 25000)

}
