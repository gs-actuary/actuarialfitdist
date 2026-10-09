# Spliced body-tail models -------------------------------------------------

.afd_prefix_spec <- function(spec, prefix) {
  names(spec) <- paste0(prefix, "_", names(spec))
  spec
}

.afd_splice_spec <- function(body, tail, x, scale_by, scale_intercept, threshold) {
  bs <- .afd_prefix_spec(.afd_make_param_spec(body, x[x <= threshold],
                                               if (is.null(scale_by)) NULL else scale_by[x <= threshold],
                                               scale_intercept), "body")
  ts <- .afd_prefix_spec(.afd_make_param_spec(tail, x[x > threshold],
                                               if (is.null(scale_by)) NULL else scale_by[x > threshold],
                                               scale_intercept), "tail")
  p0 <- mean(x <= threshold)
  p0 <- min(max(p0, 0.05), 0.95)
  c(bs, ts, list(splice_prob = list(start = p0, transform = "prob")))
}

.afd_flatten_splice_fixed <- function(fixed) {
  if (length(fixed) == 0L) return(list())
  out <- list()
  if (!is.null(fixed$body)) {
    for (nm in names(fixed$body)) out[[paste0("body_", nm)]] <- fixed$body[[nm]]
  }
  if (!is.null(fixed$tail)) {
    for (nm in names(fixed$tail)) out[[paste0("tail_", nm)]] <- fixed$tail[[nm]]
  }
  if (!is.null(fixed$splice_prob)) out$splice_prob <- fixed$splice_prob
  direct <- setdiff(names(fixed), c("body", "tail", "splice_prob"))
  for (nm in direct) out[[nm]] <- fixed[[nm]]
  out
}

.afd_fit_splice_at_threshold <- function(data, x, d, u, w, z, body, tail,
                                         threshold, scale_intercept, fixed,
                                         control, hessian) {
  if (!is.finite(threshold) || threshold <= 0) .afd_stop("Splice threshold must be a positive finite number.")
  if (sum(x <= threshold) < 5L || sum(x > threshold) < 5L) {
    .afd_stop("A splice threshold must leave at least five observed rows on each side of the threshold.")
  }
  spec <- .afd_splice_spec(body, tail, x, z, scale_intercept, threshold)
  packed <- .afd_pack_parameters(spec, .afd_flatten_splice_fixed(fixed))
  objective <- function(opt) {
    params <- packed$decode(opt)
    ll <- .afd_splice_loglik(body, tail, threshold, params, x, d, u, w, z)
    if (!is.finite(ll)) return(1e100)
    -ll
  }
  if (length(packed$start)) {
    opt <- stats::optim(
      packed$start, objective, method = "BFGS",
      control = utils::modifyList(list(maxit = 3000, reltol = 1e-10), control)
    )
    optim_par <- opt$par; convergence <- opt$convergence; message <- opt$message; value <- opt$value
  } else {
    optim_par <- numeric(); convergence <- 0L
    message <- "All parameters fixed; no optimization performed."; value <- objective(numeric())
  }
  params <- packed$decode(optim_par)
  comp <- .afd_splice_components(body, tail, threshold, params, x, d, u, z)
  if (!is.finite(value) || any(!is.finite(comp$loglik[w > 0]))) .afd_stop("Invalid spliced likelihood at this threshold.")

  H <- Vopt <- Vnat <- NULL
  if (isTRUE(hessian) && length(optim_par) && convergence == 0L) {
    H <- .afd_num_hessian(objective, optim_par)
    Vopt <- .afd_safe_inverse(H)
    if (!is.null(Vopt)) {
      all_names <- names(spec)
      J <- matrix(0, nrow = length(all_names), ncol = length(optim_par), dimnames = list(all_names, packed$unknown))
      for (j in seq_along(packed$unknown)) {
        nm <- packed$unknown[j]
        tr <- spec[[nm]]$transform
        J[nm, j] <- if (tr == "positive") params[[nm]] else if (tr == "prob") params[[nm]] * (1 - params[[nm]]) else 1
      }
      Vnat <- J %*% Vopt %*% t(J)
      dimnames(Vnat) <- list(all_names, all_names)
    }
  }
  k <- length(optim_par)
  n_eff <- .afd_n_eff(w)
  list(
    body = body, tail = tail, threshold = threshold, coefficients = unlist(params),
    logLik = -value, AIC = 2 * k + 2 * value,
    BIC = log(max(n_eff, 1)) * k + 2 * value,
    convergence = convergence, message = message,
    optim_par = optim_par, npar = k, hessian = H, vcov = Vnat,
    loglik_contributions = comp$loglik, n_censored = sum(comp$censored & w > 0)
  )
}

#' Fit a spliced body-tail severity model
#'
#' Fits a two-component severity model with one family below a threshold and a
#' second family above it. Each component is normalized on its side of the
#' threshold and a fitted splice probability controls the probability mass in
#' the body. Density continuity at the threshold is not imposed.
#'
#' The same critical ground-up-loss assumption as `fit_severity()` applies.
#' The `loss` column must contain ground-up severities. Deductibles define
#' left truncation and limits define right censoring.
#'
#' @inheritParams fit_severity
#' @param body Distribution family used below the splice threshold.
#' @param tail Distribution family used above the splice threshold.
#' @param threshold Numeric threshold for `threshold_method = "fixed"`. For
#'   search mode, it may optionally supply candidate thresholds.
#' @param threshold_method Either `"fixed"` or `"search"`.
#' @param threshold_probs For search mode when explicit thresholds are not
#'   supplied, two probabilities defining the range of empirical loss
#'   quantiles searched. Defaults to 0.65 through 0.95.
#' @param threshold_grid Number of candidate thresholds used in search mode.
#' @param min_tail_exact Minimum uncensored observations strictly above a threshold.
#'   Candidates failing this condition are skipped and described in `threshold_search`.
#' @param fixed Optional list. Use `body = list(...)`, `tail = list(...)`, and
#'   optionally `splice_prob = ...` to fix parameters.
#' @return An `actuarialfitdist_fit` object. Search fits also contain a
#'   `threshold_search` data frame.
#' @examples
#' set.seed(123)
#' d <- data.frame(loss = rlnorm(120, log(10000), 0.8))
#' fit <- fit_spliced_severity(d, "loss", body = "lognormal", tail = "pareto2",
#'   threshold = as.numeric(stats::quantile(d$loss, 0.75)), hessian = FALSE,
#'   min_tail_exact = 5)
#' fit$threshold
#' @export
fit_spliced_severity <- function(data, loss, deductible = NULL, limit = NULL,
                                 body = "lognormal", tail = "pareto2",
                                 threshold = NULL,
                                 threshold_method = c("fixed", "search"),
                                 threshold_probs = c(0.65, 0.95),
                                 threshold_grid = 15L,
                                 scale_by = NULL, scale_intercept = FALSE,
                                 weights = NULL, fixed = list(), control = list(),
                                 hessian = TRUE, invalid_rows = c("warn_drop", "error"),
                                 min_tail_exact = 25L) {
  threshold_method <- match.arg(threshold_method)
  prep <- .afd_prepare(data, loss, deductible, limit, weights, scale_by, invalid_rows)
  data <- prep$data; cols <- prep$cols
  loss_col <- cols$loss; ded_col <- cols$deductible; lim_col <- cols$limit
  scale_col <- cols$scale_by; weight_col <- cols$weights
  x <- data[[loss_col]]; d <- data[[ded_col]]; u <- data[[lim_col]]
  w <- if (is.null(weight_col)) rep(1, nrow(data)) else data[[weight_col]]
  z <- if (is.null(scale_col)) NULL else data[[scale_col]]
  if (length(min_tail_exact) != 1L || !is.finite(min_tail_exact) || min_tail_exact < 5)
    .afd_stop("`min_tail_exact` must be at least 5.")
  body <- .afd_normalize_distribution(body); tail <- .afd_normalize_distribution(tail)

  if (threshold_method == "fixed") {
    if (is.null(threshold) || length(threshold) != 1L) .afd_stop("Supply one numeric `threshold` when `threshold_method = \"fixed\"`.")
    candidates <- as.numeric(threshold)
  } else {
    if (!is.null(threshold)) {
      candidates <- sort(unique(as.numeric(threshold)))
    } else {
      if (length(threshold_probs) != 2L || any(threshold_probs <= 0 | threshold_probs >= 1) || threshold_probs[1] >= threshold_probs[2]) {
        .afd_stop("`threshold_probs` must contain two increasing probabilities strictly between 0 and 1.")
      }
      threshold_grid <- as.integer(threshold_grid)
      if (threshold_grid < 2L) .afd_stop("`threshold_grid` must be at least 2 for threshold search.")
      probs <- seq(threshold_probs[1], threshold_probs[2], length.out = threshold_grid)
      candidates <- unique(as.numeric(stats::quantile(x, probs = probs, names = FALSE, type = 8)))
    }
  }

  candidates <- sort(unique(candidates[is.finite(candidates) & candidates > 0]))
  if (!length(candidates)) .afd_stop("No positive finite splice thresholds were supplied.")
  exact_above <- vapply(candidates, function(th) sum(x > th &
    !(is.finite(u) & x >= u) & w > 0), integer(1))
  body_rows <- vapply(candidates, function(th) sum(x <= th & w > 0), integer(1))
  reason <- rep(NA_character_, length(candidates))
  reason[exact_above < min_tail_exact] <- "insufficient uncensored tail observations"
  reason[body_rows < 5L] <- "insufficient body observations"
  results <- lapply(seq_along(candidates), function(i) {
    if (!is.na(reason[i])) return(NULL)
    ans <- tryCatch(.afd_fit_splice_at_threshold(data, x, d, u, w, z,
      body, tail, candidates[i], scale_intercept, fixed, control, hessian = FALSE),
      error = function(e) e)
    if (inherits(ans, "error")) reason[i] <<- conditionMessage(ans)
    else if (ans$convergence != 0L) reason[i] <<- paste0("optimizer code ", ans$convergence)
    ans
  })
  valid <- vapply(results, function(r) is.list(r) &&
                  !inherits(r, "error") && !is.null(r$AIC) &&
                  is.finite(r$AIC) && r$convergence == 0L, logical(1))
  score <- vapply(seq_along(results), function(i)
                  if (valid[i]) results[[i]]$AIC else Inf, numeric(1))
  if (all(!is.finite(score))) {
    .afd_stop("No eligible converged splice threshold. ",
      paste(utils::head(unique(stats::na.omit(reason)), 4L), collapse = " | "),
      ". Lower `threshold_probs` or `min_tail_exact`, or inspect censoring limits.")
  }
  tail_flags <- vapply(seq_along(results), function(i) {
    if (!valid[i]) return(NA_character_)
    cf <- results[[i]]$coefficients
    get_par <- function(nm) if (nm %in% names(cf)) unname(cf[nm]) else NA_real_
    ts <- get_par("tail_scale")
    tshape <- c(get_par("tail_shape"), get_par("tail_shape1"),
                get_par("tail_shape2"))
    if ((is.finite(ts) && ts > 100 * candidates[i]) &&
        any(tshape[is.finite(tshape)] > 100))
      return("Extreme tail scale and shape: possible weak identifiability")
    if (any(tshape[is.finite(tshape)] > 1000))
      return("Extreme tail shape: inspect extrapolation")
    NA_character_
  }, character(1))
  best_i <- which.min(score)
  if (!is.na(tail_flags[best_i])) .afd_warn("Spliced-fit diagnostic: ", tail_flags[best_i])
  best <- .afd_fit_splice_at_threshold(data, x, d, u, w, z, body, tail,
                                       candidates[best_i], scale_intercept, fixed,
                                       control, hessian)
  search_df <- data.frame(
    threshold = candidates,
    n_body = body_rows, n_exact_tail = exact_above,
    n_censored = vapply(candidates, function(th) sum(is.finite(u) & x >= u & u > th & w > 0), integer(1)),
    logLik = vapply(seq_along(results), function(i) if (valid[i]) results[[i]]$logLik else NA_real_, numeric(1)),
    AIC = vapply(seq_along(results), function(i) if (valid[i]) results[[i]]$AIC else NA_real_, numeric(1)),
    BIC = vapply(seq_along(results), function(i) if (valid[i]) results[[i]]$BIC else NA_real_, numeric(1)),
    converged = valid, reason = reason, tail_warning = tail_flags,
    stringsAsFactors = FALSE
  )
  n_eff <- .afd_n_eff(w)
  if (threshold_method == "search") {
    best$npar <- best$npar + 1L
    best$AIC <- -2 * best$logLik + 2 * best$npar
    best$BIC <- -2 * best$logLik + log(max(n_eff, 1)) * best$npar
  }
  out <- c(best, list(
    call = match.call(), model = "spliced", distribution = paste0(body, "+", tail),
    fixed = fixed, threshold_method = threshold_method,
    threshold_search = if (threshold_method == "search") search_df else NULL,
    data = data,
    columns = list(loss = loss_col, deductible = ded_col, limit = lim_col,
                   scale_by = scale_col, weights = weight_col),
    scale_intercept = isTRUE(scale_intercept), n = nrow(data), n_eff = n_eff,
    weights = w, excluded_rows = prep$excluded_rows,
    excluded_details = prep$excluded_details, retained_rows = prep$retained_rows,
    notes = "Spliced densities are normalized on each side of the selected threshold; density continuity at the threshold is not imposed."
  ))
  class(out) <- "actuarialfitdist_fit"
  out
}
