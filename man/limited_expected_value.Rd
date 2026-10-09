\name{limited_expected_value}
\alias{limited_expected_value}
\title{Limited expected severity}
\description{Compute limited expected ground-up severity for a fitted model and chosen severity threshold.}
\usage{
limited_expected_value(object, limit, scale_value = NULL)
}
\arguments{
\item{object}{Fitted severity model or candidate-fits object as appropriate.}
\item{limit}{Limit; in FIT functions this is a ground-up censoring threshold column name; in FACTOR functions this is a post-deductible payment cap.}
\item{scale_value}{Scale of the insured value for a fit with `scale_by`.}
}
\details{Input losses must be positive ground-up severities. Deductibles cause left truncation; limits cause right censoring. For fitted payment factors, limit instead refers to a payment cap applied after deductible. Data that violate fitted observation assumptions are excluded with original row indices retained. See also the package vignettes.}
\value{A fitted model, comparison, data frame, numeric result or ggplot as described.}
\examples{
set.seed(123)
d <- data.frame(loss = rlnorm(120, log(10000), 0.8))
fit <- fit_severity(d, "loss", distribution = "lognormal", hessian = FALSE)
limited_expected_value(fit, 25000)

}
