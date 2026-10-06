\name{actuarialfitdist-methods}
\alias{print.actuarialfitdist_fit}
\alias{summary.actuarialfitdist_fit}
\alias{coef.actuarialfitdist_fit}
\alias{logLik.actuarialfitdist_fit}
\alias{AIC.actuarialfitdist_fit}
\alias{vcov.actuarialfitdist_fit}
\alias{confint.actuarialfitdist_fit}
\alias{plot.actuarialfitdist_fit}
\alias{simulate.actuarialfitdist_fit}
\alias{print.actuarialfitdist_candidates}
\title{Methods for actuarialfitdist Models}
\usage{
\method{print}{actuarialfitdist_fit}(x, ...)
\method{summary}{actuarialfitdist_fit}(object, ...)
\method{coef}{actuarialfitdist_fit}(object, ...)
\method{logLik}{actuarialfitdist_fit}(object, ...)
\method{AIC}{actuarialfitdist_fit}(object, ..., k = 2)
\method{vcov}{actuarialfitdist_fit}(object, ...)
\method{confint}{actuarialfitdist_fit}(object, parm, level = 0.95, ...)
\method{plot}{actuarialfitdist_fit}(x, ...)
\method{simulate}{actuarialfitdist_fit}(object, nsim = 1, seed = NULL,
  type = c("observed", "ground_up"), newdata = object$data,
  n = NULL, scale_value = NULL, ...)
\method{print}{actuarialfitdist_candidates}(x, ...)
}
\arguments{
  \item{x, object}{Fitted object.}
  \item{k}{AIC penalty multiplier.}
  \item{parm}{Parameter subset.}
  \item{level}{Confidence level.}
  \item{nsim}{Number of simulation replicates.}
  \item{seed}{Optional random seed.}
  \item{type}{Simulation or plot type as applicable.}
  \item{newdata}{Portfolio rows for observed simulation.}
  \item{n}{Number of ground-up simulation draws.}
  \item{scale_value}{Optional scale value(s).}
  \item{...}{Additional arguments.}
}
\details{
The covariance matrix is obtained by numerically evaluating the Hessian of the optimized negative log-likelihood and inverting it. Confidence intervals are large-sample Wald intervals based on this covariance matrix; they are not exact analytical intervals.

Observed simulation draws severity conditional on exceeding each row's deductible and then applies the row's limit. Ground-up simulation returns positive severity before insurance coverage modifications.
}
