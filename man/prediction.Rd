\name{predict.actuarialfitdist_fit}
\alias{predict.actuarialfitdist_fit}
\alias{severity_quantile}
\alias{exceedance_probability}
\title{Predict from Fitted Severity Models}
\usage{
\method{predict}{actuarialfitdist_fit}(object, newdata = NULL,
  scale_value = NULL, type = c("mean", "quantile", "survival", "cdf", "density"),
  p = 0.5, x = NULL, ...)

severity_quantile(object, p, scale_value = NULL)

exceedance_probability(object, threshold, scale_value = NULL)
}
\arguments{
  \item{object}{Fitted model.}
  \item{newdata}{Optional data frame containing the original scaling column.}
  \item{scale_value}{Optional scaling value(s).}
  \item{type}{Prediction type.}
  \item{p}{Quantile probability.}
  \item{x}{Severity value for density/CDF/survival predictions.}
  \item{threshold}{Severity threshold for exceedance probability.}
  \item{...}{Unused.}
}
\value{Numeric predictions.}
