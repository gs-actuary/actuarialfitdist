\name{fit_severity}
\alias{fit_severity}
\title{Fit an actuarial severity distribution}
\description{Maximum-likelihood fit of a parametric ground-up severity model. Optional deductible and limit columns specify observation truncation and censoring.}
\usage{
fit_severity(data, loss, deductible = NULL, limit = NULL, distribution = "lognormal",
                         scale_by = NULL, scale_intercept = FALSE,
                         weights = NULL, fixed = list(), control = list(),
                         hessian = TRUE, invalid_rows = c("warn_drop", "error"))
}
\arguments{
\item{data}{Claim-level data frame.}
\item{loss}{Name of the positive ground-up severity column.}
\item{deductible}{Optional column name for left-truncation deductible; NULL means zero.}
\item{limit}{Optional column name for ground-up right censoring; NULL means no finite censoring limit.}
\item{distribution}{One of the supported severity families (e.g., lognormal, gamma, weibull, burr).}
\item{scale_by}{Optional column name supplying a numeric severity-scaling exposure.}
\item{scale_intercept}{TRUE estimates a free intercept; FALSE fixes it at zero.}
\item{weights}{Optional nonnegative likelihood weight column name.}
\item{fixed}{Named list of fixed parameters; candidate fits use per-family sublists.}
\item{control}{List forwarded to stats::optim.}
\item{hessian}{Compute numerical Hessian.}
\item{invalid_rows}{"warn_drop" (default) excludes and warns; "error" stops on invalid observations.}
}
\details{Input losses must be positive ground-up severities. Deductibles cause left truncation; limits cause right censoring. For fitted payment factors, limit instead refers to a payment cap applied after deductible. Data that violate fitted observation assumptions are excluded with original row indices retained. See also the package vignettes.}
\value{A fitted model, comparison, data frame, numeric result or ggplot as described.}
\examples{
set.seed(123)
d <- data.frame(loss = rlnorm(120, log(10000), 0.8))
fit <- fit_severity(d, "loss", distribution = "lognormal")
summary(fit)

}
