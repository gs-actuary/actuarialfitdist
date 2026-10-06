\name{actuarialfitdist-package}
\alias{actuarialfitdist-package}
\alias{actuarialfitdist}
\docType{package}
\title{Actuarial Severity Distribution Fitting and Rating Factors}
\description{
Fits severity distributions to ground-up insurance losses with deductible truncation and policy-limit censoring, and carries fitted models through to actuarial limit and deductible rating factors.
}
\details{
The package supports Weibull, gamma, lognormal, Pareto II/Lomax, Burr XII, normal, exponential, log-logistic, inverse Gaussian, and generalized Pareto families; optional severity scaling; likelihood weights; fixed parameters; spliced body-tail models; simulation-based diagnostics; Hessian and bootstrap uncertainty; limited expected severity; LERs; ILFs; combined factors; and valuation/depreciation transformations.

\strong{Critical assumption:} fitting functions require ground-up claim severity on a consistent valuation basis. The loss column must not be an insurer payment, an excess-of-deductible amount, a loss net of deductible, or an ACV/depreciated payment unless the analyst has first transformed it to the intended ground-up basis. Deductibles are modeled as left-truncation thresholds and finite limits as right-censoring thresholds.
}
\seealso{\code{\link{fit_severity}}, \code{\link{fit_spliced_severity}}, \code{\link{severity_factors}}}
