\name{actuarialfitdist_fit_methods}
\alias{print.actuarialfitdist_fit}
\alias{summary.actuarialfitdist_fit}
\alias{print.summary.actuarialfitdist_fit}
\alias{coef.actuarialfitdist_fit}
\alias{vcov.actuarialfitdist_fit}
\alias{logLik.actuarialfitdist_fit}
\alias{AIC.actuarialfitdist_fit}
\alias{BIC.actuarialfitdist_fit}
\alias{confint.actuarialfitdist_fit}
\alias{plot.actuarialfitdist_fit}
\alias{print.actuarialfitdist_candidates}
\title{Methods for actuarial severity fit objects}
\description{Print, summarize, predict, simulate and inspect fitted claim severity objects.}
\details{Objects contain natural-scale coefficients, convergence and information criteria, optional covariance, retained fitting observations, and an anomalous-observation audit. Predictions are on a ground-up basis. For argument descriptions and runnable examples, see
\code{\link{predict.actuarialfitdist_fit}} and
\code{\link{simulate.actuarialfitdist_fit}}.}
\examples{
set.seed(1)
d <- data.frame(loss = rlnorm(150, 9, .8))
f <- fit_severity(d, "loss", hessian = FALSE)
summary(f)
coef(f)
logLik(f)
}
