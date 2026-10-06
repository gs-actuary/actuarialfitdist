# Limited expected severity and rating factors ----------------------------

.afd_expected_payment_one <- function(fit, deductible, limit, scale_value, valuation_factor) {
  if (!is.finite(deductible) || deductible < 0) .afd_stop("Deductibles must be finite and nonnegative.")
  if (!(is.finite(limit) || is.infinite(limit)) || limit <= 0) .afd_stop("Limits must be positive; `Inf` is allowed.")
  if (!is.finite(valuation_factor) || valuation_factor < 0) .afd_stop("Valuation factors must be finite and nonnegative.")
  if (valuation_factor == 0) return(0)
  if (is.infinite(limit)) {
    mu <- .afd_fit_mean(fit, scale_value)
    if (length(mu) == 1L && is.infinite(mu)) return(Inf)
  }
  lower <- deductible / valuation_factor
  upper <- if (is.infinite(limit)) Inf else (deductible + limit) / valuation_factor
  val <- tryCatch(stats::integrate(
    function(x) valuation_factor * .afd_fit_cdf(fit, x, scale_value, lower.tail = FALSE),
    lower = lower, upper = upper, rel.tol = 1e-7, subdivisions = 500L
  )$value, error = function(e) NA_real_)
  val
}

#' Limited expected severity
#'
#' Computes E[min(X, limit)] for positive ground-up severity X. For models
#' fitted with a scaling variable, supply the corresponding `scale_value`.
#'
#' @param object A fitted model.
#' @param limit Positive severity limit.
#' @param scale_value Optional scaling value.
#' @return Numeric limited expected severity.
#' @export
limited_expected_value <- function(object, limit, scale_value = NULL) {
  if (!inherits(object, "actuarialfitdist_fit")) .afd_stop("`object` must be a fitted actuarialfitdist model.")
  n <- max(length(limit), if (is.null(scale_value)) 1L else length(scale_value))
  lim <- .afd_recycle(limit, n, "limit")
  sv <- if (is.null(scale_value)) rep(NA_real_, n) else .afd_recycle(scale_value, n, "scale_value")
  vapply(seq_len(n), function(i) {
    svi <- if (is.na(sv[i])) NULL else sv[i]
    .afd_expected_payment_one(object, 0, lim[i], svi, 1)
  }, numeric(1))
}

#' Loss elimination ratio
#'
#' Calculates one minus the expected payment after deductible divided by the
#' expected positive ground-up severity. Infinite-mean models return `NA` when
#' the denominator is not finite.
#'
#' @param object A fitted model.
#' @param deductible Deductible amount.
#' @param scale_value Optional scaling value.
#' @param valuation_factor Multiplicative valuation factor applied before the deductible.
#' @return Numeric LER.
#' @export
loss_elimination_ratio <- function(object, deductible, scale_value = NULL, valuation_factor = 1) {
  n <- max(length(deductible), length(valuation_factor), if (is.null(scale_value)) 1L else length(scale_value))
  d <- .afd_recycle(deductible, n, "deductible")
  v <- .afd_recycle(valuation_factor, n, "valuation_factor")
  sv <- if (is.null(scale_value)) rep(NA_real_, n) else .afd_recycle(scale_value, n, "scale_value")
  vapply(seq_len(n), function(i) {
    svi <- if (is.na(sv[i])) NULL else sv[i]
    base <- .afd_expected_payment_one(object, 0, Inf, svi, v[i])
    pay <- .afd_expected_payment_one(object, d[i], Inf, svi, v[i])
    if (!is.finite(base) || base <= 0) return(NA_real_)
    1 - pay / base
  }, numeric(1))
}

#' Increased limits factor
#'
#' Computes the ratio of expected payments at a target limit to expected
#' payments at a base limit, holding the deductible and valuation basis fixed.
#'
#' @param object A fitted model.
#' @param limit Target payment limit.
#' @param base_limit Base payment limit.
#' @param scale_value Optional scaling value.
#' @param deductible Deductible applied to both target and base coverage.
#' @param valuation_factor Multiplicative valuation factor applied before deductible.
#' @return Numeric ILF.
#' @export
increased_limits_factor <- function(object, limit, base_limit, scale_value = NULL,
                                    deductible = 0, valuation_factor = 1) {
  n <- max(length(limit), length(base_limit), length(deductible), length(valuation_factor),
           if (is.null(scale_value)) 1L else length(scale_value))
  lim <- .afd_recycle(limit, n, "limit"); bl <- .afd_recycle(base_limit, n, "base_limit")
  d <- .afd_recycle(deductible, n, "deductible"); v <- .afd_recycle(valuation_factor, n, "valuation_factor")
  sv <- if (is.null(scale_value)) rep(NA_real_, n) else .afd_recycle(scale_value, n, "scale_value")
  vapply(seq_len(n), function(i) {
    svi <- if (is.na(sv[i])) NULL else sv[i]
    num <- .afd_expected_payment_one(object, d[i], lim[i], svi, v[i])
    den <- .afd_expected_payment_one(object, d[i], bl[i], svi, v[i])
    if (!is.finite(den) || den <= 0) return(NA_real_)
    num / den
  }, numeric(1))
}

.afd_factor_grid_single <- function(fit, deductible, limit, scale_value,
                                    valuation_factor, base_deductible,
                                    base_limit, base_valuation_factor) {
  scaling <- !is.null(fit$columns$scale_by)
  if (scaling && is.null(scale_value)) .afd_stop("Supply `scale_value` because the model was fitted with `scale_by`.")
  if (!scaling) scale_value <- NA_real_
  g <- expand.grid(
    scale_value = scale_value,
    deductible = deductible,
    limit = limit,
    valuation_factor = valuation_factor,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  g$expected_payment <- NA_real_
  g$limited_expected_severity <- NA_real_
  g$unlimited_mean <- NA_real_
  g$LER <- NA_real_
  g$ILF <- NA_real_
  g$rating_factor <- NA_real_
  for (i in seq_len(nrow(g))) {
    sv <- if (is.na(g$scale_value[i])) NULL else g$scale_value[i]
    g$expected_payment[i] <- .afd_expected_payment_one(fit, g$deductible[i], g$limit[i], sv, g$valuation_factor[i])
    g$limited_expected_severity[i] <- limited_expected_value(fit, g$limit[i], sv)
    g$unlimited_mean[i] <- .afd_fit_mean(fit, sv)
    g$LER[i] <- loss_elimination_ratio(fit, g$deductible[i], sv, g$valuation_factor[i])
    g$ILF[i] <- increased_limits_factor(fit, g$limit[i], base_limit, sv,
                                        deductible = g$deductible[i], valuation_factor = g$valuation_factor[i])
    base_pay <- .afd_expected_payment_one(fit, base_deductible, base_limit, sv, base_valuation_factor)
    g$rating_factor[i] <- if (is.finite(base_pay) && base_pay > 0) g$expected_payment[i] / base_pay else NA_real_
  }
  g$distribution <- fit$distribution
  g$model <- fit$model
  g$base_deductible <- base_deductible
  g$base_limit <- base_limit
  g$base_valuation_factor <- base_valuation_factor
  g[, c("distribution", "model", "scale_value", "deductible", "limit", "valuation_factor",
        "expected_payment", "limited_expected_severity", "unlimited_mean", "LER", "ILF",
        "rating_factor", "base_deductible", "base_limit", "base_valuation_factor")]
}

#' Generate actuarial severity rating factors
#'
#' Takes either one fitted severity model or a candidate-comparison object to
#' the business endpoint. Outputs expected payments, limited expected severity,
#' LERs, ILFs, and base-relative combined rating factors. If a candidate object
#' is supplied, every successful candidate is evaluated using the same coverage
#' scenarios so factor sensitivity can be compared directly.
#'
#' A `valuation_factor` below one applies a multiplicative ACV/depreciation
#' adjustment to ground-up replacement-cost severity before the deductible and
#' limit. Fitting remains agnostic to depreciation: historical losses must have
#' already been placed on the analyst's chosen ground-up basis.
#'
#' @param object A fitted model or candidate-comparison object.
#' @param deductible Vector of target deductibles.
#' @param limit Vector of target payment limits; `Inf` is allowed.
#' @param scale_value Optional vector of scaling values.
#' @param valuation_factor Vector of multiplicative valuation factors; 1 means no depreciation adjustment.
#' @param base_deductible Base deductible for `rating_factor`.
#' @param base_limit Base payment limit for `rating_factor` and ILF calculations.
#' @param base_valuation_factor Base valuation factor.
#' @return Long-format data frame.
#' @export
severity_factors <- function(object, deductible, limit, scale_value = NULL,
                             valuation_factor = 1, base_deductible = 0,
                             base_limit = Inf, base_valuation_factor = 1) {
  if (inherits(object, "actuarialfitdist_candidates")) {
    ok <- vapply(object$fits, inherits, logical(1), what = "actuarialfitdist_fit")
    if (!any(ok)) .afd_stop("No successful candidate fits are available.")
    ans <- lapply(object$fits[ok], .afd_factor_grid_single,
                  deductible = deductible, limit = limit, scale_value = scale_value,
                  valuation_factor = valuation_factor, base_deductible = base_deductible,
                  base_limit = base_limit, base_valuation_factor = base_valuation_factor)
    return(do.call(rbind, ans))
  }
  if (!inherits(object, "actuarialfitdist_fit")) .afd_stop("`object` must be a fitted model or candidate-comparison object.")
  .afd_factor_grid_single(object, deductible, limit, scale_value, valuation_factor,
                          base_deductible, base_limit, base_valuation_factor)
}

#' Compare factor outputs side by side
#'
#' Evaluates the same rating scenarios for each successful candidate fit and
#' reshapes one selected factor metric into a distribution-by-distribution table.
#'
#' @param object A candidate-comparison object.
#' @param metric One output column from `severity_factors()` to compare.
#' @param ... Arguments passed to `severity_factors()`.
#' @return Wide data frame with one column per candidate distribution.
#' @export
compare_factors <- function(object, metric = "rating_factor", ...) {
  if (!inherits(object, "actuarialfitdist_candidates")) .afd_stop("`object` must come from `fit_severity_candidates()`.")
  dat <- severity_factors(object, ...)
  allowed <- c("expected_payment", "limited_expected_severity", "unlimited_mean", "LER", "ILF", "rating_factor")
  if (!metric %in% allowed) .afd_stop("`metric` must be one of: ", paste(allowed, collapse = ", "), ".")
  id <- c("scale_value", "deductible", "limit", "valuation_factor", "base_deductible", "base_limit", "base_valuation_factor")
  tmp <- dat[, c(id, "distribution", metric)]
  names(tmp)[names(tmp) == metric] <- "value"
  stats::reshape(tmp, idvar = id, timevar = "distribution", direction = "wide")
}

#' Expected payment under deductible, limit, and valuation terms
#'
#' Computes E[min(max(v * X - d, 0), L)] where X is positive ground-up
#' severity, v is `valuation_factor`, d is `deductible`, and L is `limit`.
#'
#' @param object A fitted model.
#' @param deductible Deductible amount(s).
#' @param limit Payment limit(s); `Inf` is allowed.
#' @param scale_value Optional scaling value(s).
#' @param valuation_factor Nonnegative multiplicative valuation factor applied
#'   to ground-up severity before deductible and limit.
#' @return Numeric expected payment(s).
#' @export
expected_payment <- function(object, deductible = 0, limit = Inf,
                             scale_value = NULL, valuation_factor = 1) {
  if (!inherits(object, "actuarialfitdist_fit")) .afd_stop("`object` must be a fitted actuarialfitdist model.")
  n <- max(length(deductible), length(limit), length(valuation_factor),
           if (is.null(scale_value)) 1L else length(scale_value))
  d <- .afd_recycle(deductible, n, "deductible")
  lim <- .afd_recycle(limit, n, "limit")
  v <- .afd_recycle(valuation_factor, n, "valuation_factor")
  sv <- if (is.null(scale_value)) rep(NA_real_, n) else .afd_recycle(scale_value, n, "scale_value")
  vapply(seq_len(n), function(i) {
    svi <- if (is.na(sv[i])) NULL else sv[i]
    .afd_expected_payment_one(object, d[i], lim[i], svi, v[i])
  }, numeric(1))
}
