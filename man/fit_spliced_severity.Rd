\name{fit_spliced_severity}
\alias{fit_spliced_severity}
\title{Fit a Spliced Body-Tail Severity Model}
\usage{
fit_spliced_severity(data, loss, deductible, limit,
  body = "lognormal", tail = "pareto2", threshold = NULL,
  threshold_method = c("fixed", "search"), threshold_probs = c(0.65, 0.95),
  threshold_grid = 15L, scale_by = NULL, scale_intercept = FALSE,
  weights = NULL, fixed = list(), control = list(), hessian = TRUE)
}
\arguments{
  \item{data, loss, deductible, limit, scale_by, scale_intercept, weights, control, hessian}{See \code{\link{fit_severity}}. The loss column must be ground-up severity.}
  \item{body}{Distribution family below the threshold.}
  \item{tail}{Distribution family above the threshold.}
  \item{threshold}{For fixed mode, one positive numeric threshold. For search mode, an optional numeric vector of candidate thresholds.}
  \item{threshold_method}{\code{"fixed"} or \code{"search"}.}
  \item{threshold_probs}{When search thresholds are not supplied directly, two empirical-loss probabilities defining the search range.}
  \item{threshold_grid}{Number of searched thresholds.}
  \item{fixed}{Optional list containing \code{body = list(...)}, \code{tail = list(...)}, and optionally \code{splice_prob}.}
}
\details{
The body density is normalized over positive severities up to the threshold. The tail density is normalized above the threshold. A splice probability allocates probability mass to the body. Density continuity is not imposed.

Search mode fits the model across candidate thresholds and chooses the minimum profile AIC. The selected threshold is counted as an additional model degree of freedom in the final reported AIC and BIC. Threshold-search output is retained in \code{threshold_search}.

When \code{scale_by} is used in a spliced model, body and tail component scales each vary linearly with the supplied scaling variable. Because the splice threshold is an absolute loss amount, the overall spliced mean is not constrained to be exactly linear in the scaling variable; use \code{\link{plot_scaling_fit}} to inspect the resulting relationship.
}
\value{An object of class \code{actuarialfitdist_fit}.}
