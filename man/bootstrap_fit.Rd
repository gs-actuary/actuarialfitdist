\name{bootstrap_fit}
\alias{bootstrap_fit}
\title{Bootstrap a fitted severity model}
\description{Bootstrap fitted severity coefficients by parametric simulation or case resampling.}
\usage{
bootstrap_fit(object, R = 200, type = c("parametric", "case"),
                          seed = NULL, refit_threshold = FALSE)
}
\arguments{
\item{object}{Fitted severity model or candidate-fits object as appropriate.}
\item{R}{Number of bootstrap replicates.}
\item{type}{For plots, choose histogram, density, CDF or survival; for bootstrap, choose parametric or case.}
\item{seed}{Optional random seed for repeatable simulations.}
\item{refit_threshold}{Whether bootstrap iterations rerun the original threshold search.}
}
\details{Input losses must be positive ground-up severities. Deductibles cause left truncation; limits cause right censoring. For fitted payment factors, limit instead refers to a payment cap applied after deductible. Data that violate fitted observation assumptions are excluded with original row indices retained. See also the package vignettes.}
\value{A fitted model, comparison, data frame, numeric result or ggplot as described.}
\examples{
set.seed(123)
d <- data.frame(loss = rlnorm(120, log(10000), 0.8))
fit <- fit_severity(d, "loss", distribution = "lognormal", hessian = FALSE)
boot <- bootstrap_fit(fit, R = 3, type = "case", seed = 1)
boot$success_rate

}
