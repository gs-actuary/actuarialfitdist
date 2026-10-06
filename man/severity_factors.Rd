\name{severity_factors}
\alias{severity_factors}
\alias{limited_expected_value}
\alias{expected_payment}
\alias{loss_elimination_ratio}
\alias{increased_limits_factor}
\alias{compare_factors}
\title{Limited Expected Severity and Actuarial Rating Factors}
\usage{
severity_factors(object, deductible, limit, scale_value = NULL,
  valuation_factor = 1, base_deductible = 0, base_limit = Inf,
  base_valuation_factor = 1)

limited_expected_value(object, limit, scale_value = NULL)

expected_payment(object, deductible = 0, limit = Inf, scale_value = NULL,
  valuation_factor = 1)

loss_elimination_ratio(object, deductible, scale_value = NULL,
  valuation_factor = 1)

increased_limits_factor(object, limit, base_limit, scale_value = NULL,
  deductible = 0, valuation_factor = 1)

compare_factors(object, metric = "rating_factor", ...)
}
\arguments{
  \item{object}{A fitted model, or for \code{severity_factors()} and \code{compare_factors()}, a candidate-comparison object as appropriate.}
  \item{deductible}{Target deductible(s).}
  \item{limit}{Target payment limit(s); \code{Inf} is allowed.}
  \item{scale_value}{For a model fitted with \code{scale_by}, the scaling value(s) at which to calculate outputs.}
  \item{valuation_factor}{Nonnegative multiplicative valuation factor applied to ground-up severity before deductible and limit. A value of 1 represents no depreciation adjustment.}
  \item{base_deductible}{Base deductible for combined rating factors.}
  \item{base_limit}{Base limit for ILFs and combined rating factors.}
  \item{base_valuation_factor}{Base valuation factor for combined rating factors.}
  \item{metric}{Output column to compare side by side.}
  \item{...}{Arguments passed to \code{severity_factors()}.}
}
\details{
For ground-up positive severity \eqn{X}, valuation factor \eqn{v}, deductible \eqn{d}, and payment limit \eqn{L}, expected payment is
\deqn{E[\min(\max(vX-d,0),L)].}
The implementation evaluates the equivalent survival integral, which naturally supports all fitted single-family and spliced models.

Depreciation or ACV treatment belongs here, at the payment-transformation stage. The fitting layer remains agnostic to depreciation and expects historical claims already expressed on the analyst's chosen ground-up valuation basis.

\code{severity_factors()} returns a long data frame. When supplied a candidate-comparison object, it evaluates every successful candidate through the same business scenarios. This makes it easy to judge whether selected LERs or ILFs are materially sensitive to the fitted family.
}
\value{Numeric vectors for the scalar helpers; a data frame for \code{severity_factors()} and \code{compare_factors()}.}
