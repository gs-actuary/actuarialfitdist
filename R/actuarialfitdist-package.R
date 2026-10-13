#' actuarialfitdist: Actuarial Severity Fitting with Limits and Deductibles
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
#' @section Documentation and first steps:
#' Start with `vignette("actuarialfitdist")` for a worked example and
#' `vignette("candidate-comparisons")` for model comparison, plotting,
#' and conversion to rating factors. See `?fit_severity` and
#' `?severity_factors` for the different meanings of a policy-limit input.
#'
#' @section Supported families:
#' Weibull, gamma, lognormal, Pareto II/Lomax, Burr XII, normal, exponential,
#' log-logistic, inverse Gaussian, and generalized Pareto. Spliced body-tail
#' combinations may use any supported family on either side of the threshold.
#'
#' @importFrom stats AIC BIC coef confint logLik simulate vcov
#' @importFrom stats dgamma dlnorm dnorm dweibull integrate optim pnorm
#' @importFrom stats qgamma qlnorm qnorm qweibull rgamma rlnorm rnorm
#' @importFrom stats runif rweibull uniroot
#' @importFrom ggplot2 aes geom_histogram geom_line geom_point ggplot
#' @importFrom ggplot2 labs theme_minimal
#' @keywords internal
"_PACKAGE"

# Variables referenced through ggplot2's non-standard evaluation. Declaring
# them here prevents R CMD check from treating aesthetic column names as
# undefined R globals.
utils::globalVariables(c(
  "observed_loss", "density", "weight", "source", "simulation",
  "x", "y", "scale_value", "mean", "series", "distribution", ".data"
))
