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
#' @examples
#' set.seed(123)
#' d <- data.frame(loss = rlnorm(120, log(10000), 0.8))
#' fit <- fit_severity(d, "loss", distribution = "lognormal", hessian = FALSE)
#' limited_expected_value(fit, 25000)
#' @export
limited_expected_value <- function(object, limit, scale_value = NULL) {
  if (!inherits(object, "actuarialfitdist_fit")) .afd_stop("`object` must be a fitted actuarialfitdist model.")
  n <- max(length(limit), if (is.null(scale_value)) 1L else length(scale_value))
  lim <- .afd_recycle(limit, n, "limit")
  sv <- if (is.null(scale_value)) rep(NA_real_, n) else .afd_recycle(scale_value, n, "scale_value")
  .afd_payments_bulk(object, rep(0, n), lim, sv, rep(1, n))
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
#' @examples
#' set.seed(123)
#' d <- data.frame(loss = rlnorm(120, log(10000), 0.8))
#' fit <- fit_severity(d, "loss", distribution = "lognormal", hessian = FALSE)
#' loss_elimination_ratio(fit, deductible = 1000)
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
#' @examples
#' set.seed(123)
#' d <- data.frame(loss = rlnorm(120, log(10000), 0.8))
#' fit <- fit_severity(d, "loss", distribution = "lognormal", hessian = FALSE)
#' increased_limits_factor(fit, limit = 50000, base_limit = 25000)
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

# Vectorized limited expected values for common positive severity families.
# For other families (and splices), use the established numerical integral.
# Note: policy LIMIT in expected_payment is a PAYMENT cap after deductible.
# Consequently E[min((v*X-d)+, L)] = v*(A((d+L)/v)-A(d/v)),
# not A(L)-A(d). This distinction is material for ILFs and rating factors.
.afd_lev_fast <- function(fit, t, scale_value = NULL) {
  if (length(t) == 0L) return(numeric())
  if (fit$model == "single" && fit$distribution %in%
      c("exponential", "weibull", "gamma", "lognormal")) {
    parts <- .afd_single_eval_parts(fit, scale_value)
    s <- parts$s; p <- parts$shape
    ans <- switch(fit$distribution,
      exponential = -s * expm1(-t / s),
      weibull = s * gamma(1 + 1 / p$shape) *
        stats::pgamma((t / s)^p$shape, shape = 1 + 1 / p$shape) +
        t * exp(-(t / s)^p$shape),
      gamma = s * p$shape * stats::pgamma(t, shape = p$shape + 1, scale = s) +
        t * stats::pgamma(t, shape = p$shape, scale = s, lower.tail = FALSE),
      lognormal = {
        q <- (log(t / s) - p$sdlog^2) / p$sdlog
        s * exp(p$sdlog^2 / 2) * stats::pnorm(q) +
          t * stats::plnorm(t, meanlog = log(s), sdlog = p$sdlog, lower.tail = FALSE)
      })
    # 0*Inf is NaN at infinite limits. Replace using the analytic mean.
    infinite <- is.infinite(t)
    if (any(infinite)) ans[infinite] <- .afd_fit_mean(fit, scale_value)
    ans[t == 0] <- 0
    return(ans)
  }
  vapply(t, function(x) {
    if (x == 0) return(0)
    if (is.infinite(x)) return(.afd_fit_mean(fit, scale_value))
    tryCatch(stats::integrate(function(y)
      .afd_fit_cdf(fit, y, scale_value, lower.tail = FALSE),
      lower = 0, upper = x, rel.tol = 1e-7, subdivisions = 500L)$value,
      error = function(e) NA_real_)
  }, numeric(1))
}

.afd_payments_bulk <- function(fit, deductible, limit, scale_value, valuation_factor) {
  n <- length(deductible)
  if (!is.null(fit$columns$scale_by) && anyNA(scale_value))
    .afd_stop("`scale_value` is required for a model fitted with `scale_by`.")
  out <- numeric(n)
  groups <- split(seq_len(n), ifelse(is.na(scale_value), "default", sprintf("%.17g", scale_value)))
  for (ind in groups) {
    sv <- if (is.na(scale_value[ind[1L]])) NULL else scale_value[ind[1L]]
    v <- valuation_factor[ind]; d <- deductible[ind]; lim <- limit[ind]
    if (any(!is.finite(d) | d < 0) || any(is.na(lim) | lim <= 0) ||
        any(!is.finite(v) | v < 0)) .afd_stop("Invalid deductible, limit or valuation factor.")
    active <- v > 0
    if (!any(active)) next
    bounds <- c(d[active] / v[active], (d[active] + lim[active]) / v[active])
    unique_bounds <- unique(bounds)
    levs <- .afd_lev_fast(fit, unique_bounds, sv)
    lookup <- match(bounds, unique_bounds)
    na <- sum(active)
    lower <- levs[lookup[seq_len(na)]]
    upper <- levs[lookup[na + seq_len(na)]]
    val <- v[active] * (upper - lower)
    # When both expected values diverge, the limited-payment result is not
    # determined by naive subtraction. Finite payment caps remain finite.
    # Avoid catastrophic cancellation in the distant tail, where both
    # limited expectations can round to the same nearly-unlimited mean.
    bad <- is.nan(val) | (is.finite(val) & val <= 0 & d[active] > 0)
    if (any(bad)) val[bad] <- v[active][bad] * vapply(which(bad), function(j) {
      lo <- d[active][j] / v[active][j]
      hi <- (d[active][j] + lim[active][j]) / v[active][j]
      tryCatch(stats::integrate(function(y) .afd_fit_cdf(fit, y, sv, lower.tail = FALSE),
        lo, hi, rel.tol = 1e-7, subdivisions = 500L)$value,
        error = function(e) NA_real_)
    }, numeric(1))
    out[ind[active]] <- pmax(0, val)
  }
  out
}

.afd_factor_grid_single <- function(fit, deductible, limit, scale_value,
                                    valuation_factor, base_deductible,
                                    base_limit, base_valuation_factor,
                                    grid = "cross") {
  scaling <- !is.null(fit$columns$scale_by)
  if (scaling && is.null(scale_value)) .afd_stop("Supply `scale_value` because the model was fitted with `scale_by`.")
  if (!scaling) scale_value <- NA_real_
  if (grid == "cross") {
    g <- expand.grid(scale_value = scale_value, deductible = deductible,
       limit = limit, valuation_factor = valuation_factor,
       KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  } else {
    n <- max(length(scale_value), length(deductible), length(limit), length(valuation_factor))
    g <- data.frame(scale_value = .afd_recycle(scale_value, n, "scale_value"),
      deductible = .afd_recycle(deductible, n, "deductible"),
      limit = .afd_recycle(limit, n, "limit"),
      valuation_factor = .afd_recycle(valuation_factor, n, "valuation_factor"))
  }
  n <- nrow(g); sv <- g$scale_value
  g$expected_payment <- .afd_payments_bulk(fit, g$deductible, g$limit, sv, g$valuation_factor)
  g$limited_expected_severity <- .afd_payments_bulk(fit, rep(0, n), g$limit, sv, rep(1, n))
  sv_unique <- unique(sv)
  sv_means <- vapply(sv_unique, function(x) .afd_fit_mean(fit,
    if (is.na(x)) NULL else x), numeric(1))
  g$unlimited_mean <- sv_means[match(sv, sv_unique)]
  zero <- rep(0, n); inf <- rep(Inf, n)
  base_mean <- .afd_payments_bulk(fit, zero, inf, sv, g$valuation_factor)
  ded_pay <- .afd_payments_bulk(fit, g$deductible, inf, sv, g$valuation_factor)
  g$LER <- ifelse(is.finite(base_mean) & base_mean > 0, 1 - ded_pay/base_mean, NA_real_)
  base_limit_pay <- .afd_payments_bulk(fit, g$deductible,
    rep(base_limit, n), sv, g$valuation_factor)
  g$ILF <- ifelse(is.finite(base_limit_pay) & base_limit_pay > 0,
    g$expected_payment / base_limit_pay, NA_real_)
  base_pay <- .afd_payments_bulk(fit, rep(base_deductible, n),
    rep(base_limit, n), sv, rep(base_valuation_factor, n))
  g$rating_factor <- ifelse(is.finite(base_pay) & base_pay > 0,
    g$expected_payment / base_pay, NA_real_)
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
#' @param grid `"cross"` (default) forms the Cartesian product of inputs;
#'   `"paired"` recycles vectors to matching scenario rows.
#' @return Long-format data frame.
#' @examples
#' set.seed(123)
#' d <- data.frame(loss = rlnorm(120, log(10000), 0.8))
#' fit <- fit_severity(d, "loss", distribution = "lognormal", hessian = FALSE)
#' severity_factors(fit, deductible = c(500, 1000),
#'   limit = c(25000, 50000), grid = "paired")
#' @export
severity_factors <- function(object, deductible, limit, scale_value = NULL,
                             valuation_factor = 1, base_deductible = 0,
                             base_limit = Inf, base_valuation_factor = 1,
                             grid = c("cross", "paired")) {
  grid <- match.arg(grid)
  if (inherits(object, "actuarialfitdist_candidates")) {
    ok <- vapply(object$fits, function(f) inherits(f, "actuarialfitdist_fit") &&
                 f$convergence == 0L, logical(1))
    if (!any(ok)) .afd_stop("No successful candidate fits are available.")
    ans <- lapply(object$fits[ok], .afd_factor_grid_single,
                  deductible = deductible, limit = limit, scale_value = scale_value,
                  valuation_factor = valuation_factor, base_deductible = base_deductible,
                  base_limit = base_limit, base_valuation_factor = base_valuation_factor,
                  grid = grid)
    return(do.call(rbind, ans))
  }
  if (!inherits(object, "actuarialfitdist_fit")) .afd_stop("`object` must be a fitted model or candidate-comparison object.")
  .afd_factor_grid_single(object, deductible, limit, scale_value, valuation_factor,
                          base_deductible, base_limit, base_valuation_factor, grid)
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
#' @examples
#' set.seed(123)
#' d <- data.frame(loss = rlnorm(120, log(10000), 0.8))
#' candidates <- fit_severity_candidates(d, "loss",
#'   distributions = c("lognormal", "gamma"), hessian = FALSE)
#' compare_factors(candidates, deductible = 1000, limit = c(25000, 50000))
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
#' @examples
#' set.seed(123)
#' d <- data.frame(loss = rlnorm(120, log(10000), 0.8))
#' fit <- fit_severity(d, "loss", distribution = "lognormal", hessian = FALSE)
#' expected_payment(fit, deductible = 1000, limit = 25000)
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
  .afd_payments_bulk(object, d, lim, sv, v)
}
