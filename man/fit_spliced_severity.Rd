\name{fit_spliced_severity}
\alias{fit_spliced_severity}
\title{Fit a spliced body-tail severity model}
\description{Fit a two-part severity distribution, with separate normalized body and tail components at a fixed or searched threshold.}
\usage{
fit_spliced_severity(data, loss, deductible = NULL, limit = NULL,
                                 body = "lognormal", tail = "pareto2",
                                 threshold = NULL,
                                 threshold_method = c("fixed", "search"),
                                 threshold_probs = c(0.65, 0.95),
                                 threshold_grid = 15L,
                                 scale_by = NULL, scale_intercept = FALSE,
                                 weights = NULL, fixed = list(), control = list(),
                                 hessian = TRUE, invalid_rows = c("warn_drop", "error"),
                                 min_tail_exact = 25L)
}
\arguments{
\item{data}{Claim-level data frame.}
\item{loss}{Name of the positive ground-up severity column.}
\item{deductible}{Optional column name for left-truncation deductible; NULL means zero.}
\item{limit}{Optional column name for ground-up right censoring; NULL means no finite censoring limit.}
\item{body}{Severity family to fit below the splice threshold.}
\item{tail}{Severity family to fit above the splice threshold.}
\item{threshold}{Positive ground-up severity splice threshold; when searching, a vector may define candidate values.}
\item{threshold_method}{`"fixed"` (one threshold) or `"search"` (compare candidate thresholds).}
\item{threshold_probs}{Lower and upper empirical observed-loss quantiles for candidate thresholds.}
\item{threshold_grid}{Number of threshold quantile grid points before deduplication.}
\item{scale_by}{Optional column name supplying a numeric severity-scaling exposure.}
\item{scale_intercept}{TRUE estimates a free intercept; FALSE fixes it at zero.}
\item{weights}{Optional nonnegative likelihood weight column name.}
\item{fixed}{Named list of fixed parameters; candidate fits use per-family sublists.}
\item{control}{List forwarded to stats::optim.}
\item{hessian}{Compute numerical Hessian.}
\item{invalid_rows}{"warn_drop" (default) excludes and warns; "error" stops on invalid observations.}
\item{min_tail_exact}{Minimum exact uncensored observations above a candidate threshold (default 25).}
}
\details{Input losses must be positive ground-up severities. Deductibles cause left truncation; limits cause right censoring. For fitted payment factors, limit instead refers to a payment cap applied after deductible. Data that violate fitted observation assumptions are excluded with original row indices retained. See also the package vignettes.}
\value{A fitted model, comparison, data frame, numeric result or ggplot as described.}
\examples{
set.seed(123)
d <- data.frame(loss = rlnorm(120, log(10000), 0.8))
fit <- fit_spliced_severity(d, "loss", body = "lognormal", tail = "pareto2",
  threshold = as.numeric(stats::quantile(d$loss, 0.75)), hessian = FALSE,
  min_tail_exact = 5)
fit$threshold

}
