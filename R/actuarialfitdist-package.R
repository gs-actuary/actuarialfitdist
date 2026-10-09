#' actuarialfitdist: Actuarial Severity Distribution Fitting and Rating Factors
#'
#' Fits severity distributions to ground-up insurance losses with deductible
#' truncation and limit censoring, then carries fitted models through to limited
#' expected severity, LER, ILF, deductible, and combined rating-factor analyses.
#'
#' @section Ground-up losses are required:
#' The core fitting functions assume the `loss` column is ground-up severity on
#' a consistent valuation basis. Do not supply insurer payments, net-of-
#' deductible amounts, excess losses, or depreciated ACV payments unless the
#' analyst has first transformed them to the intended ground-up basis.
#'
#' @section Supported families:
#' Weibull, gamma, lognormal, Pareto II/Lomax, Burr XII, normal, exponential,
#' log-logistic, inverse Gaussian, and generalized Pareto. Spliced body-tail
#' combinations may use any supported family on either side of the threshold.
#'
#' @importFrom stats AIC BIC coef confint logLik simulate vcov
#' @docType package
#' @name actuarialfitdist-package
NULL

# Variables referenced through ggplot2's non-standard evaluation. Declaring
# them here prevents R CMD check from treating aesthetic column names as
# undefined R globals.
utils::globalVariables(c(
  "observed_loss", "density", "weight", "source", "simulation",
  "x", "y", "scale_value", "mean", "series", "distribution"
))
