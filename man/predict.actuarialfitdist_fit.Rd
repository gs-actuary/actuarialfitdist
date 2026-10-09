\name{predict.actuarialfitdist_fit}
\alias{predict.actuarialfitdist_fit}
\title{Predict severity quantities}
\description{Evaluates fitted ground-up means, quantiles, survival probabilities,
cumulative probabilities, or densities from a fitted severity model.}
\usage{
\method{predict}{actuarialfitdist_fit}(
  object, newdata = NULL, scale_value = NULL,
  type = c("mean", "quantile", "survival", "cdf", "density"),
  p = 0.5, x = NULL, ...
)
}
\arguments{
\item{object}{Fitted \code{actuarialfitdist_fit} model.}
\item{newdata}{Optional data frame containing the original scaling column.}
\item{scale_value}{Optional numeric scaling value(s), used instead of \code{newdata}.}
\item{type}{Quantity to predict: \code{"mean"}, \code{"quantile"}, \code{"survival"}, \code{"cdf"}, or \code{"density"}.}
\item{p}{Quantile probability for \code{type = "quantile"}.}
\item{x}{Ground-up severity value for CDF, survival, or density predictions.}
\item{...}{Unused.}
}
\value{Numeric vector. Means and quantiles are in severity units; CDF and
survival values are probabilities, and density values are densities.}
\examples{
set.seed(1)
d <- data.frame(loss = rlnorm(120, log(10000), 0.8))
fit <- fit_severity(d, "loss", hessian = FALSE)
predict(fit, type = "mean")
predict(fit, type = "quantile", p = 0.95)
predict(fit, type = "survival", x = 25000)
}
