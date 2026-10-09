\name{loss_elimination_ratio}
\alias{loss_elimination_ratio}
\title{Loss elimination ratio}
\description{Calculate the proportion of the expected positive severity eliminated by a deductible.}
\usage{
loss_elimination_ratio(object, deductible, scale_value = NULL, valuation_factor = 1)
}
\arguments{
\item{object}{Fitted severity model or candidate-fits object as appropriate.}
\item{deductible}{Optional column name for left-truncation deductible; NULL means zero.}
\item{scale_value}{Scale of the insured value for a fit with `scale_by`.}
\item{valuation_factor}{Multiplicative severity valuation adjustment applied before deductible.}
}
\details{Input losses must be positive ground-up severities. Deductibles cause left truncation; limits cause right censoring. For fitted payment factors, limit instead refers to a payment cap applied after deductible. Data that violate fitted observation assumptions are excluded with original row indices retained. See also the package vignettes.}
\value{A fitted model, comparison, data frame, numeric result or ggplot as described.}
\examples{
set.seed(123)
d <- data.frame(loss = rlnorm(120, log(10000), 0.8))
fit <- fit_severity(d, "loss", distribution = "lognormal", hessian = FALSE)
loss_elimination_ratio(fit, deductible = 1000)

}
