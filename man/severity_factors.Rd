\name{severity_factors}
\alias{severity_factors}
\title{Generate actuarial severity rating factors}
\description{Generate expected insurance payments, limited expected severities, LERs, ILFs, and base-relative rate factors from one or several fits.}
\usage{
severity_factors(object, deductible, limit, scale_value = NULL,
                             valuation_factor = 1, base_deductible = 0,
                             base_limit = Inf, base_valuation_factor = 1,
                             grid = c("cross", "paired"))
}
\arguments{
\item{object}{Fitted severity model or candidate-fits object as appropriate.}
\item{deductible}{Optional column name for left-truncation deductible; NULL means zero.}
\item{limit}{Limit; in FIT functions this is a ground-up censoring threshold column name; in FACTOR functions this is a post-deductible payment cap.}
\item{scale_value}{Scale of the insured value for a fit with `scale_by`.}
\item{valuation_factor}{Multiplicative severity valuation adjustment applied before deductible.}
\item{base_deductible}{Reference deductible used to calculate combined rating factors.}
\item{base_limit}{Base payment cap after deductible.}
\item{base_valuation_factor}{Reference pre-deductible valuation multiplier.}
\item{grid}{"cross" (Cartesian) or "paired" (positional recycling of scenarios).}
}
\details{Input losses must be positive ground-up severities. Deductibles cause left truncation; limits cause right censoring. For fitted payment factors, limit instead refers to a payment cap applied after deductible. Data that violate fitted observation assumptions are excluded with original row indices retained. See also the package vignettes.}
\value{A fitted model, comparison, data frame, numeric result or ggplot as described.}
\examples{
set.seed(123)
d <- data.frame(loss = rlnorm(120, log(10000), 0.8))
fit <- fit_severity(d, "loss", distribution = "lognormal", hessian = FALSE)
severity_factors(fit, deductible = c(500, 1000),
  limit = c(25000, 50000), grid = "paired")

}
