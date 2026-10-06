\name{fit_severity}
\alias{fit_severity}
\title{Fit an Actuarial Severity Distribution}
\usage{
fit_severity(data, loss, deductible, limit, distribution,
  scale_by = NULL, scale_intercept = FALSE, weights = NULL,
  fixed = list(), control = list(), hessian = TRUE)
}
\arguments{
  \item{data}{Claim-level data frame.}
  \item{loss}{Character scalar naming the \strong{ground-up severity} column. This must not be an insurer payment, net-of-deductible amount, excess loss, or depreciated/ACV payment unless the analyst has already transformed it to the intended common ground-up basis.}
  \item{deductible}{Character scalar naming the deductible column. Claims at or below this value are assumed absent from the observed claim sample.}
  \item{limit}{Character scalar naming the ground-up censoring limit. Use \code{Inf} where no finite limit applies. A loss recorded at or above a finite limit is treated as right-censored at that limit.}
  \item{distribution}{One of \code{"weibull"}, \code{"gamma"}, \code{"lognormal"}, \code{"pareto2"}, \code{"burr"}, \code{"normal"}, \code{"exponential"}, \code{"loglogistic"}, \code{"invgauss"}, or \code{"gpd"}. Aliases such as \code{"lomax"} and \code{"pareto"} map to Pareto II.}
  \item{scale_by}{Optional character scalar naming a numeric severity-scaling variable. In homeowners this may be Coverage A; in other applications it may be TIV, vehicle value, or an analyst-created transformation.}
  \item{scale_intercept}{Logical. If \code{FALSE}, the scale intercept is exactly zero. If \code{TRUE}, an intercept is estimated, subject to positive fitted severity scales.}
  \item{weights}{Optional character scalar naming nonnegative likelihood weights.}
  \item{fixed}{Named list of natural-scale parameters to hold fixed.}
  \item{control}{List passed to \code{stats::optim()}.}
  \item{hessian}{Logical; calculate a numerical Hessian and covariance matrix.}
}
\details{
For an exact ground-up loss \eqn{x} with deductible \eqn{d}, the row log-likelihood is
\deqn{\log f(x) - \log S(d).}
For a loss censored at limit \eqn{u}, it is
\deqn{\log S(u) - \log S(d).}
A likelihood weight \eqn{w} multiplies the row log-likelihood. Thus \eqn{w=2} is equivalent to two identical observations, and the weighted likelihood contribution is \eqn{L_i^w}, not \eqn{wL_i}.

When \code{scale_by} is supplied, supported families are parameterized using a severity scale
\deqn{s_i = a z_i + b,}
where \eqn{z_i} is the supplied scaling variable, \eqn{a} is a positive fitted coefficient, and \eqn{b} is zero unless \code{scale_intercept = TRUE}. When the family mean exists, it is proportional to \eqn{s_i}. For example, Weibull uses native scale \eqn{s_i}; lognormal uses \code{meanlog = log(s_i)}; and gamma uses native scale \eqn{s_i}.

The normal family is fit as a positive claim-severity model through the truncation likelihood and has support below zero before conditioning. It is included primarily as a comparator and may be less natural than positive-support families. The generalized Pareto is often most natural for tail/excess modeling but is available as a whole-loss candidate.
}
\value{An object of class \code{actuarialfitdist_fit}.}
\examples{
set.seed(1)
x <- rlnorm(1000, log(10000), 0.8)
d <- sample(c(500, 1000), 1000, TRUE)
u <- rep(50000, 1000)
keep <- x > d
dat <- data.frame(loss = pmin(x[keep], u[keep]),
                  deductible = d[keep], limit = u[keep])
fit <- fit_severity(dat, "loss", "deductible", "limit", "lognormal")
coef(fit)
}
