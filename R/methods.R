# S3 methods --------------------------------------------------------------

#' @export
print.actuarialfitdist_fit <- function(x, ...) {
  cat("Actuarial severity fit\n")
  cat("Model:        ", x$model, "\n", sep = "")
  cat("Distribution: ", x$distribution, "\n", sep = "")
  if (x$model == "spliced") cat("Threshold:     ", format(x$threshold), " (", x$threshold_method, ")\n", sep = "")
  cat("Ground-up observations: ", x$n, "\n", sep = "")
  cat("Right-censored:          ", x$n_censored, "\n", sep = "")
  cat("Log-likelihood:          ", format(x$logLik, digits = 7), "\n", sep = "")
  cat("AIC:                     ", format(x$AIC, digits = 7), "\n", sep = "")
  cat("Converged:               ", if (x$convergence == 0L) "yes" else "no", "\n", sep = "")
  cat("\nCoefficients:\n")
  print(x$coefficients)
  if (nzchar(x$notes %||% "")) cat("\nNote: ", x$notes, "\n", sep = "")
  invisible(x)
}

#' @export
summary.actuarialfitdist_fit <- function(object, ...) {
  se <- rep(NA_real_, length(object$coefficients)); names(se) <- names(object$coefficients)
  if (!is.null(object$vcov)) {
    vv <- diag(object$vcov)
    se[names(vv)] <- sqrt(pmax(vv, 0))
  }
  tab <- data.frame(
    estimate = object$coefficients,
    std.error = se,
    row.names = names(object$coefficients),
    check.names = FALSE
  )
  out <- list(
    call = object$call, model = object$model, distribution = object$distribution,
    coefficients = tab, logLik = object$logLik, AIC = object$AIC, BIC = object$BIC,
    n = object$n, n_eff = object$n_eff, n_censored = object$n_censored,
    convergence = object$convergence, message = object$message,
    threshold = object$threshold %||% NULL, notes = object$notes
  )
  class(out) <- "summary.actuarialfitdist_fit"
  out
}

print.summary.actuarialfitdist_fit <- function(x, ...) {
  cat("Actuarial severity model summary\n\n")
  cat("Model: ", x$model, "\nDistribution: ", x$distribution, "\n", sep = "")
  if (!is.null(x$threshold)) cat("Splice threshold: ", format(x$threshold), "\n", sep = "")
  cat("Observations: ", x$n, " (effective weighted count: ", format(x$n_eff), ")\n", sep = "")
  cat("Right-censored: ", x$n_censored, "\n", sep = "")
  cat("LogLik: ", format(x$logLik, digits = 7), "  AIC: ", format(x$AIC, digits = 7),
      "  BIC: ", format(x$BIC, digits = 7), "\n\n", sep = "")
  print(x$coefficients)
  if (nzchar(x$notes %||% "")) cat("\n", x$notes, "\n", sep = "")
  invisible(x)
}

#' @export
coef.actuarialfitdist_fit <- function(object, ...) object$coefficients

#' @export
logLik.actuarialfitdist_fit <- function(object, ...) {
  out <- object$logLik
  attr(out, "df") <- object$npar %||% length(object$optim_par)
  attr(out, "nobs") <- object$n_eff
  class(out) <- "logLik"
  out
}

#' @export
AIC.actuarialfitdist_fit <- function(object, ..., k = 2) {
  -2 * object$logLik + k * (object$npar %||% length(object$optim_par))
}

#' @export
vcov.actuarialfitdist_fit <- function(object, ...) {
  if (is.null(object$vcov)) .afd_stop("A covariance matrix is unavailable. Refit with `hessian = TRUE` and check convergence.")
  object$vcov
}

#' @export
confint.actuarialfitdist_fit <- function(object, parm, level = 0.95, ...) {
  est <- object$coefficients
  if (!missing(parm)) est <- est[parm]
  V <- stats::vcov(object)
  V <- V[names(est), names(est), drop = FALSE]
  z <- stats::qnorm(1 - (1 - level) / 2)
  se <- sqrt(pmax(diag(V), 0))
  cbind(lower = est - z * se, upper = est + z * se)
}

#' @export
print.actuarialfitdist_candidates <- function(x, ...) {
  cat("Candidate actuarial severity fits\n\n")
  print(x$comparison, row.names = FALSE)
  cat("\nNo distribution is selected automatically. Compare fit statistics, diagnostics, tail behavior, and downstream factors.\n")
  invisible(x)
}

#' Predict severity quantities
#'
#' Evaluates fitted means, quantiles, survival probabilities, CDF values, or
#' densities for single-family and spliced severity models.
#'
#' @param object Fitted model.
#' @param newdata Optional data frame containing the original scaling column.
#' @param scale_value Optional numeric scaling value(s). Used instead of `newdata`.
#' @param type One of `mean`, `quantile`, `survival`, `cdf`, or `density`.
#' @param p Probability for quantiles.
#' @param x Severity value for CDF, survival, or density predictions.
#' @param ... Unused.
#' @export
predict.actuarialfitdist_fit <- function(object, newdata = NULL, scale_value = NULL,
                                         type = c("mean", "quantile", "survival", "cdf", "density"),
                                         p = 0.5, x = NULL, ...) {
  type <- match.arg(type)
  if (!is.null(newdata)) {
    sc <- object$columns$scale_by
    if (is.null(sc)) {
      scale_value <- NULL
    } else {
      if (!is.data.frame(newdata) || !sc %in% names(newdata)) .afd_stop("`newdata` must contain the original scaling column `", sc, "`.")
      scale_value <- newdata[[sc]]
    }
  }
  switch(
    type,
    mean = .afd_fit_mean(object, scale_value),
    quantile = severity_quantile(object, p, scale_value),
    survival = {
      if (is.null(x)) .afd_stop("Supply `x` for survival predictions.")
      .afd_fit_cdf(object, x, scale_value, lower.tail = FALSE)
    },
    cdf = {
      if (is.null(x)) .afd_stop("Supply `x` for CDF predictions.")
      .afd_fit_cdf(object, x, scale_value, lower.tail = TRUE)
    },
    density = {
      if (is.null(x)) .afd_stop("Supply `x` for density predictions.")
      .afd_fit_density(object, x, scale_value)
    }
  )
}

#' @export
plot.actuarialfitdist_fit <- function(x, ...) plot_fit(x, ...)

#' @export
BIC.actuarialfitdist_fit <- function(object, ...) {
  -2 * object$logLik + log(max(object$n_eff, 1)) * (object$npar %||% length(object$optim_par))
}
