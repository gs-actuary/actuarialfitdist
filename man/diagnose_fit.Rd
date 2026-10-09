\name{diagnose_fit}
\alias{diagnose_fit}
\title{Diagnose fit within a severity layer}
\description{Compare empirical versus simulated observed claim behavior in selected severity layers.}
\usage{
diagnose_fit(object, lower = 0, upper = Inf, scale_range = NULL,
                         nsim = 100, seed = NULL)
}
\arguments{
\item{object}{Fitted severity model or candidate-fits object as appropriate.}
\item{lower}{Lower observed severity bound for layer comparisons.}
\item{upper}{Upper observed severity bound for layer comparisons.}
\item{scale_range}{Optional inclusive two-number scale_by range for plotting.}
\item{nsim}{Number of simulated observed portfolios.}
\item{seed}{Optional random seed for repeatable simulations.}
}
\details{Input losses must be positive ground-up severities. Deductibles cause left truncation; limits cause right censoring. For fitted payment factors, limit instead refers to a payment cap applied after deductible. Data that violate fitted observation assumptions are excluded with original row indices retained. See also the package vignettes.}
\value{A fitted model, comparison, data frame, numeric result or ggplot as described.}
\examples{
set.seed(123)
d <- data.frame(loss = rlnorm(120, log(10000), 0.8))
fit <- fit_severity(d, "loss", distribution = "lognormal", hessian = FALSE)
diagnose_fit(fit, lower = 1000, upper = 30000, nsim = 1)

}
