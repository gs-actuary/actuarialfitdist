# Likelihood calculations -------------------------------------------------

.afd_single_components <- function(dist, params, x, deductible, limit, scale_by = NULL) {
  s <- if (is.null(scale_by)) rep(params$scale, length(x)) else .afd_scale_value(params, scale_by)
  if (length(s) == 1L) s <- rep(s, length(x))
  if (any(!is.finite(s)) || any(s <= 0)) return(NULL)

  shape_params <- params[setdiff(names(params), c("scale", "scale_coef", "scale_intercept"))]
  if (!.afd_valid_distribution_params(dist, shape_params)) return(NULL)
  log_surv_d <- .afd_p(dist, deductible, s, shape_params, lower.tail = FALSE, log.p = TRUE)
  censored <- is.finite(limit) & x >= limit
  ll <- numeric(length(x))
  exact <- !censored
  if (any(exact)) {
    ll[exact] <- .afd_d(dist, x[exact], s[exact], shape_params, log = TRUE) - log_surv_d[exact]
  }
  if (any(censored)) {
    ll[censored] <- .afd_p(dist, limit[censored], s[censored], shape_params, lower.tail = FALSE, log.p = TRUE) - log_surv_d[censored]
  }
  list(loglik = ll, scale = s, censored = censored, shape_params = shape_params)
}

.afd_single_loglik <- function(dist, params, x, deductible, limit, weights, scale_by = NULL) {
  z <- .afd_single_components(dist, params, x, deductible, limit, scale_by)
  if (is.null(z) || any(!is.finite(z$loglik[weights > 0]))) return(-Inf)
  sum(weights[weights > 0] * z$loglik[weights > 0])
}

.afd_positive_cdf <- function(dist, x, s, p) {
  # CDF conditional on X > 0. For positive-support distributions F(0)=0.
  Fx <- .afd_p(dist, x, s, p)
  F0 <- .afd_p(dist, 0, s, p)
  den <- 1 - F0
  out <- (Fx - F0) / den
  out[x <= 0] <- 0
  pmin(pmax(out, 0), 1)
}

.afd_positive_density <- function(dist, x, s, p, log = FALSE) {
  f <- .afd_d(dist, x, s, p, log = FALSE)
  den <- 1 - .afd_p(dist, 0, s, p)
  out <- ifelse(x > 0, f / den, 0)
  if (log) log(out) else out
}

.afd_splice_component_params <- function(params, prefix) {
  keep <- startsWith(names(params), paste0(prefix, "_"))
  ans <- params[keep]
  names(ans) <- sub(paste0("^", prefix, "_"), "", names(ans))
  ans
}

.afd_splice_component_scale <- function(params, prefix, scale_by = NULL) {
  p <- .afd_splice_component_params(params, prefix)
  if (is.null(scale_by)) return(rep(p$scale, 1L))
  intercept <- if (!is.null(p$scale_intercept)) p$scale_intercept else 0
  p$scale_coef * scale_by + intercept
}

.afd_splice_density <- function(x, threshold, body, tail, params, scale_by = NULL, log = FALSE) {
  n <- max(length(x), if (is.null(scale_by)) 1L else length(scale_by))
  x <- rep(x, length.out = n)
  sb <- .afd_splice_component_scale(params, "body", scale_by); sb <- rep(sb, length.out = n)
  st <- .afd_splice_component_scale(params, "tail", scale_by); st <- rep(st, length.out = n)
  if (any(sb <= 0 | st <= 0 | !is.finite(sb) | !is.finite(st))) {
    return(rep(if (log) -Inf else 0, n))
  }
  bp <- .afd_splice_component_params(params, "body")
  tp <- .afd_splice_component_params(params, "tail")
  bp <- bp[setdiff(names(bp), c("scale", "scale_coef", "scale_intercept"))]
  tp <- tp[setdiff(names(tp), c("scale", "scale_coef", "scale_intercept"))]
  if (!.afd_valid_distribution_params(body, bp) ||
      !.afd_valid_distribution_params(tail, tp)) {
    return(rep(if (log) -Inf else 0, n))
  }
  pi_body <- params$splice_prob
  out <- numeric(n)
  lo <- x <= threshold & x > 0
  hi <- x > threshold
  if (any(lo)) {
    F0 <- .afd_p(body, 0, sb[lo], bp)
    Ft <- .afd_p(body, threshold, sb[lo], bp)
    den <- Ft - F0
    out[lo] <- pi_body * .afd_d(body, x[lo], sb[lo], bp) / den
  }
  if (any(hi)) {
    St <- .afd_p(tail, threshold, st[hi], tp, lower.tail = FALSE)
    out[hi] <- (1 - pi_body) * .afd_d(tail, x[hi], st[hi], tp) / St
  }
  out[!is.finite(out) | out < 0] <- 0
  if (log) log(out) else out
}

.afd_splice_cdf <- function(x, threshold, body, tail, params, scale_by = NULL, lower.tail = TRUE, log.p = FALSE) {
  n <- max(length(x), if (is.null(scale_by)) 1L else length(scale_by))
  x <- rep(x, length.out = n)
  sb <- .afd_splice_component_scale(params, "body", scale_by); sb <- rep(sb, length.out = n)
  st <- .afd_splice_component_scale(params, "tail", scale_by); st <- rep(st, length.out = n)
  if (any(sb <= 0 | st <= 0 | !is.finite(sb) | !is.finite(st))) return(rep(NA_real_, n))
  bp <- .afd_splice_component_params(params, "body")
  tp <- .afd_splice_component_params(params, "tail")
  bp <- bp[setdiff(names(bp), c("scale", "scale_coef", "scale_intercept"))]
  tp <- tp[setdiff(names(tp), c("scale", "scale_coef", "scale_intercept"))]
  if (!.afd_valid_distribution_params(body, bp) ||
      !.afd_valid_distribution_params(tail, tp)) return(rep(NA_real_, n))
  pi_body <- params$splice_prob
  out <- numeric(n)
  lo <- x <= threshold & x > 0
  hi <- x > threshold
  out[x <= 0] <- 0
  if (any(lo)) {
    F0 <- .afd_p(body, 0, sb[lo], bp)
    Ft <- .afd_p(body, threshold, sb[lo], bp)
    Fx <- .afd_p(body, x[lo], sb[lo], bp)
    out[lo] <- pi_body * (Fx - F0) / (Ft - F0)
  }
  if (any(hi)) {
    Ft <- .afd_p(tail, threshold, st[hi], tp)
    St <- 1 - Ft
    Fx <- .afd_p(tail, x[hi], st[hi], tp)
    out[hi] <- pi_body + (1 - pi_body) * (Fx - Ft) / St
  }
  out <- pmin(pmax(out, 0), 1)
  if (!lower.tail) out <- 1 - out
  if (log.p) log(out) else out
}

.afd_splice_quantile <- function(prob, threshold, body, tail, params, scale_by = NULL) {
  n <- max(length(prob), if (is.null(scale_by)) 1L else length(scale_by))
  prob <- rep(prob, length.out = n)
  sb <- .afd_splice_component_scale(params, "body", scale_by); sb <- rep(sb, length.out = n)
  st <- .afd_splice_component_scale(params, "tail", scale_by); st <- rep(st, length.out = n)
  bp_all <- .afd_splice_component_params(params, "body")
  tp_all <- .afd_splice_component_params(params, "tail")
  bp <- bp_all[setdiff(names(bp_all), c("scale", "scale_coef", "scale_intercept"))]
  tp <- tp_all[setdiff(names(tp_all), c("scale", "scale_coef", "scale_intercept"))]
  pi_body <- params$splice_prob
  out <- numeric(n)
  lo <- prob <= pi_body
  if (any(lo)) {
    F0 <- .afd_p(body, 0, sb[lo], bp)
    Ft <- .afd_p(body, threshold, sb[lo], bp)
    target <- F0 + (prob[lo] / pi_body) * (Ft - F0)
    out[lo] <- .afd_q(body, target, sb[lo], bp)
  }
  if (any(!lo)) {
    Ft <- .afd_p(tail, threshold, st[!lo], tp)
    target <- Ft + ((prob[!lo] - pi_body) / (1 - pi_body)) * (1 - Ft)
    out[!lo] <- .afd_q(tail, target, st[!lo], tp)
  }
  out
}

.afd_splice_components <- function(body, tail, threshold, params, x, deductible, limit, scale_by = NULL) {
  log_surv_d <- .afd_splice_cdf(deductible, threshold, body, tail, params, scale_by, lower.tail = FALSE, log.p = TRUE)
  censored <- is.finite(limit) & x >= limit
  ll <- numeric(length(x))
  exact <- !censored
  if (any(exact)) {
    sb <- if (is.null(scale_by)) NULL else scale_by[exact]
    ll[exact] <- .afd_splice_density(x[exact], threshold, body, tail, params, sb, log = TRUE) - log_surv_d[exact]
  }
  if (any(censored)) {
    sb <- if (is.null(scale_by)) NULL else scale_by[censored]
    ll[censored] <- .afd_splice_cdf(limit[censored], threshold, body, tail, params, sb, lower.tail = FALSE, log.p = TRUE) - log_surv_d[censored]
  }
  list(loglik = ll, censored = censored)
}

.afd_splice_loglik <- function(body, tail, threshold, params, x, deductible, limit, weights, scale_by = NULL) {
  if (!is.finite(params$splice_prob) || params$splice_prob <= 0 || params$splice_prob >= 1) return(-Inf)
  z <- .afd_splice_components(body, tail, threshold, params, x, deductible, limit, scale_by)
  if (any(!is.finite(z$loglik[weights > 0]))) return(-Inf)
  sum(weights[weights > 0] * z$loglik[weights > 0])
}
