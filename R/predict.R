# Fitted distribution evaluation -----------------------------------------

.afd_require_scale_value <- function(fit, scale_value) {
  has_scale <- !is.null(fit$columns$scale_by)
  if (has_scale && is.null(scale_value)) {
    .afd_stop("This model was fitted with `scale_by`; supply `scale_value` for prediction or factor generation.")
  }
  if (!has_scale && is.null(scale_value)) return(NA_real_)
  if (!has_scale && !is.null(scale_value)) return(scale_value)
  if (any(!is.finite(scale_value))) .afd_stop("`scale_value` must be finite.")
  scale_value
}

.afd_single_eval_parts <- function(fit, scale_value) {
  p <- as.list(fit$coefficients)
  if (is.null(fit$columns$scale_by)) {
    s <- p$scale
  } else {
    b <- p$scale_intercept %||% 0
    s <- p$scale_coef * scale_value + b
  }
  if (any(!is.finite(s) | s <= 0)) .afd_stop("Requested scale value produces a nonpositive severity scale.")
  shape <- p[setdiff(names(p), c("scale", "scale_coef", "scale_intercept"))]
  list(s = s, shape = shape)
}

.afd_fit_cdf <- function(fit, x, scale_value = NULL, lower.tail = TRUE) {
  sv <- .afd_require_scale_value(fit, scale_value)
  n <- max(length(x), if (all(is.na(sv))) 1L else length(sv))
  x <- rep(x, length.out = n)
  if (fit$model == "single") {
    parts <- .afd_single_eval_parts(fit, if (all(is.na(sv))) NULL else rep(sv, length.out = n))
    s <- rep(parts$s, length.out = n)
    ans <- .afd_positive_cdf(fit$distribution, x, s, parts$shape)
    if (!lower.tail) ans <- 1 - ans
    return(ans)
  }
  z <- if (is.null(fit$columns$scale_by)) NULL else rep(sv, length.out = n)
  .afd_splice_cdf(x, fit$threshold, fit$body, fit$tail,
                  as.list(fit$coefficients), z, lower.tail = lower.tail)
}

.afd_fit_density <- function(fit, x, scale_value = NULL) {
  sv <- .afd_require_scale_value(fit, scale_value)
  n <- max(length(x), if (all(is.na(sv))) 1L else length(sv))
  x <- rep(x, length.out = n)
  if (fit$model == "single") {
    parts <- .afd_single_eval_parts(fit, if (all(is.na(sv))) NULL else rep(sv, length.out = n))
    return(.afd_positive_density(fit$distribution, x, rep(parts$s, length.out = n), parts$shape))
  }
  z <- if (is.null(fit$columns$scale_by)) NULL else rep(sv, length.out = n)
  .afd_splice_density(x, fit$threshold, fit$body, fit$tail,
                      as.list(fit$coefficients), z)
}

.afd_fit_quantile <- function(fit, prob, scale_value = NULL) {
  sv <- .afd_require_scale_value(fit, scale_value)
  n <- max(length(prob), if (all(is.na(sv))) 1L else length(sv))
  prob <- rep(prob, length.out = n)
  if (any(prob < 0 | prob > 1)) .afd_stop("Probabilities must lie in [0, 1].")
  if (fit$model == "single") {
    parts <- .afd_single_eval_parts(fit, if (all(is.na(sv))) NULL else rep(sv, length.out = n))
    s <- rep(parts$s, length.out = n)
    F0 <- .afd_p(fit$distribution, 0, s, parts$shape)
    target <- F0 + prob * (1 - F0)
    return(.afd_q(fit$distribution, target, s, parts$shape))
  }
  z <- if (is.null(fit$columns$scale_by)) NULL else rep(sv, length.out = n)
  .afd_splice_quantile(prob, fit$threshold, fit$body, fit$tail,
                       as.list(fit$coefficients), z)
}

.afd_fit_mean <- function(fit, scale_value = NULL) {
  sv <- .afd_require_scale_value(fit, scale_value)
  if (fit$model == "single") {
    parts <- .afd_single_eval_parts(fit, if (all(is.na(sv))) NULL else sv)
    s <- parts$s
    if (fit$distribution == "normal") {
      cv <- parts$shape$cv
      a <- -1 / cv
      # Mean of Normal(mu=s, sd=cv*s) conditional on X > 0.
      return(s + cv * s * stats::dnorm(a) / (1 - stats::pnorm(a)))
    }
    return(.afd_mean(fit$distribution, s, parts$shape))
  }
  # For spliced models the absolute threshold can break exact linearity of the
  # overall mean even when each component scale is linear in scale_by.
  svals <- if (all(is.na(sv))) NA_real_ else sv
  vapply(seq_along(svals), function(i) {
    svi <- if (is.na(svals[i])) NULL else svals[i]
    val <- tryCatch(stats::integrate(
      function(x) .afd_fit_cdf(fit, x, svi, lower.tail = FALSE),
      lower = 0, upper = Inf, rel.tol = 1e-7, subdivisions = 500L
    )$value, error = function(e) Inf)
    val
  }, numeric(1))
}

#' Severity quantiles from a fitted model
#'
#' Returns ground-up severity quantiles implied by a fitted single-family or
#' spliced model, including scale-dependent fits when `scale_value` is supplied.
#'
#' @param object A fitted actuarialfitdist model.
#' @param p Probability or vector of probabilities.
#' @param scale_value Optional scaling value for a model fitted with `scale_by`.
#' @return Numeric vector of ground-up severity quantiles.
#' @examples
#' set.seed(123)
#' d <- data.frame(loss = rlnorm(120, log(10000), 0.8))
#' fit <- fit_severity(d, "loss", distribution = "lognormal", hessian = FALSE)
#' severity_quantile(fit, c(.5, .9, .99))
#' @export
severity_quantile <- function(object, p, scale_value = NULL) {
  if (!inherits(object, "actuarialfitdist_fit")) .afd_stop("`object` must be a fitted actuarialfitdist model.")
  .afd_fit_quantile(object, p, scale_value)
}

#' Exceedance probabilities from a fitted model
#'
#' Returns fitted ground-up probabilities of exceeding specified severity
#' thresholds, optionally at supplied values of the scaling variable.
#'
#' @param object A fitted actuarialfitdist model.
#' @param threshold Ground-up severity threshold.
#' @param scale_value Optional scaling value for a model fitted with `scale_by`.
#' @return Probability that severity exceeds the threshold.
#' @examples
#' set.seed(123)
#' d <- data.frame(loss = rlnorm(120, log(10000), 0.8))
#' fit <- fit_severity(d, "loss", distribution = "lognormal", hessian = FALSE)
#' exceedance_probability(fit, 100000)
#' @export
exceedance_probability <- function(object, threshold, scale_value = NULL) {
  if (!inherits(object, "actuarialfitdist_fit")) .afd_stop("`object` must be a fitted actuarialfitdist model.")
  .afd_fit_cdf(object, threshold, scale_value, lower.tail = FALSE)
}
