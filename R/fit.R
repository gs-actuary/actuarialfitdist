#' Fit an actuarial severity distribution
#'
#' Fits a parametric severity distribution to ground-up insurance losses using
#' maximum likelihood. Claims below the deductible are treated as unobserved
#' (left truncation) and observations at the policy limit are treated as
#' right-censored.
#'
#' @section Critical ground-up-loss assumption:
#' `loss` must contain ground-up claim severities on a consistent valuation
#' basis. It must not contain insurer payments, losses net of deductible,
#' excess-of-deductible amounts, or ACV/depreciated payments unless the analyst
#' has first converted those observations to the intended ground-up basis.
#' The package deliberately does not guess or reconstruct this basis.
#'
#' @param data A data frame containing claim-level observations.
#' @param loss Character scalar naming the ground-up loss column.
#' @param deductible Optional character scalar naming the deductible column; omitted means no deductible.
#' @param limit Optional character scalar naming the ground-up censoring-limit column.
#'   Omission means no censoring; use `Inf` for individually unlimited claims.
#' @param distribution Distribution family. See `?actuarialfitdist`.
#' @param scale_by Optional character scalar naming a numeric severity-scaling
#'   variable. For homeowners this may be Coverage A; for other lines it may be
#'   TIV, vehicle value, or a user-created transformation. The package treats
#'   it generically. Supported families are parameterized so finite mean
#'   severity is proportional to `scale_coef * scale_by + scale_intercept`.
#' @param scale_intercept Logical. If `FALSE`, the scale intercept is fixed
#'   exactly at zero. If `TRUE`, it is estimated subject to positive fitted
#'   scales for all observations used in fitting.
#' @param weights Optional character scalar naming nonnegative likelihood
#'   weights. A weight of 2 contributes twice the row log-likelihood and is
#'   equivalent to two identical observations. Noninteger weights define a
#'   weighted or pseudo-likelihood.
#' @param fixed Named list of natural-scale parameters to hold fixed.
#' @param control List passed to `stats::optim()`.
#' @param hessian Logical; calculate a numerical Hessian and covariance matrix.
#' @param invalid_rows `"warn_drop"` excludes anomalous rows with a warning (default);
#'   `"error"` stops and identifies affected original positions.
#'   Anomalies include ground-up loss at/below deductible and above a finite
#'   censoring limit; investigate the valuation basis before fitting.
#' @return An object of class `actuarialfitdist_fit`.
#' @examples
#' set.seed(123)
#' d <- data.frame(loss = rlnorm(120, log(10000), 0.8))
#' fit <- fit_severity(d, "loss", distribution = "lognormal")
#' summary(fit)
#' @export
fit_severity <- function(data, loss, deductible = NULL, limit = NULL, distribution = "lognormal",
                         scale_by = NULL, scale_intercept = FALSE,
                         weights = NULL, fixed = list(), control = list(),
                         hessian = TRUE, invalid_rows = c("warn_drop", "error")) {
  prep <- .afd_prepare(data, loss, deductible, limit, weights, scale_by, invalid_rows)
  data <- prep$data; cols <- prep$cols
  loss_col <- cols$loss; ded_col <- cols$deductible; lim_col <- cols$limit
  scale_col <- cols$scale_by; weight_col <- cols$weights
  x <- data[[loss_col]]; d <- data[[ded_col]]; u <- data[[lim_col]]
  w <- if (is.null(weight_col)) rep(1, nrow(data)) else data[[weight_col]]
  z <- if (is.null(scale_col)) NULL else data[[scale_col]]

  dist <- .afd_normalize_distribution(distribution)
  spec <- .afd_make_param_spec(dist, x, z, scale_intercept)
  packed <- .afd_pack_parameters(spec, fixed)

  objective <- function(opt) {
    params <- packed$decode(opt)
    ll <- .afd_single_loglik(dist, params, x, d, u, w, z)
    if (!is.finite(ll)) return(1e100)
    -ll
  }

  if (length(packed$start)) {
    opt <- stats::optim(
      par = packed$start, fn = objective, method = "BFGS",
      control = utils::modifyList(list(maxit = 2000, reltol = 1e-10), control)
    )
    optim_par <- opt$par
    convergence <- opt$convergence
    message <- opt$message
    value <- opt$value
  } else {
    optim_par <- numeric()
    convergence <- 0L
    message <- "All parameters fixed; no optimization performed."
    value <- objective(numeric())
  }
  params <- packed$decode(optim_par)
  components <- .afd_single_components(dist, params, x, d, u, z)
  if (is.null(components) || !is.finite(value) ||
      any(!is.finite(components$loglik[w > 0]))) {
    .afd_stop("The fitted parameter combination is invalid.")
  }

  H <- Vopt <- Vnat <- NULL
  if (isTRUE(hessian) && length(optim_par) && convergence == 0L) {
    H <- .afd_num_hessian(objective, optim_par)
    Vopt <- .afd_safe_inverse(H)
    if (!is.null(Vopt)) {
      # Numerical Jacobian of natural parameters with respect to optimization parameters.
      all_names <- names(spec)
      J <- matrix(0, nrow = length(all_names), ncol = length(optim_par), dimnames = list(all_names, packed$unknown))
      for (j in seq_along(packed$unknown)) {
        nm <- packed$unknown[j]
        if (spec[[nm]]$transform == "positive") J[nm, j] <- params[[nm]] else J[nm, j] <- 1
      }
      Vnat <- J %*% Vopt %*% t(J)
      dimnames(Vnat) <- list(all_names, all_names)
    }
  }

  k <- length(optim_par)
  n_eff <- .afd_n_eff(w)
  out <- list(
    call = match.call(), model = "single", distribution = dist,
    coefficients = unlist(params), fixed = fixed,
    logLik = -value, AIC = 2 * k + 2 * value,
    BIC = log(max(n_eff, 1)) * k + 2 * value,
    convergence = convergence, message = message,
    optim_par = optim_par, npar = k, hessian = H, vcov = Vnat,
    data = data, columns = list(loss = loss_col, deductible = ded_col, limit = lim_col,
                                scale_by = scale_col, weights = weight_col),
    scale_intercept = isTRUE(scale_intercept),
    n = nrow(data), n_eff = n_eff,
    n_censored = sum(components$censored & w > 0),
    loglik_contributions = components$loglik,
    weights = w, excluded_rows = prep$excluded_rows,
    excluded_details = prep$excluded_details, retained_rows = prep$retained_rows,
    notes = .afd_distribution_note(dist)
  )
  class(out) <- "actuarialfitdist_fit"
  out
}

#' Fit and compare candidate severity distributions
#'
#' Fits several supported families to the same ground-up-loss data and returns
#' both the fitted models and a comparison table. The function intentionally
#' does not choose a winner automatically; actuarial tail behavior and rating
#' factor stability can matter even when information criteria are close.
#'
#' @inheritParams fit_severity
#' @param distributions Character vector of candidate families. Defaults to all
#'   supported single-family distributions.
#' @usage fit_severity_candidates(
#'   data, loss, deductible = NULL, limit = NULL,
#'   distributions = c("weibull", "gamma", "lognormal",
#'     "pareto2", "burr", "normal", "exponential",
#'     "loglogistic", "invgauss", "gpd"),
#'   scale_by = NULL, scale_intercept = FALSE,
#'   weights = NULL, fixed = list(), control = list(),
#'   hessian = TRUE, invalid_rows = c("warn_drop", "error")
#' )
#' @return An object of class `actuarialfitdist_candidates`.
#' @examples
#' set.seed(123)
#' d <- data.frame(loss = rlnorm(120, log(10000), 0.8))
#' candidates <- fit_severity_candidates(d, "loss",
#'   distributions = c("lognormal", "gamma"), hessian = FALSE)
#' candidates$comparison
#' candidates$fits$lognormal
#' plot_candidate_fits(candidates, nsim = 1)
#' @export
fit_severity_candidates <- function(data, loss, deductible = NULL, limit = NULL,
                                    distributions = c(
                                      "weibull", "gamma", "lognormal", "pareto2", "burr",
                                      "normal", "exponential", "loglogistic", "invgauss", "gpd"
                                    ),
                                    scale_by = NULL, scale_intercept = FALSE,
                                    weights = NULL, fixed = list(), control = list(),
                                    hessian = TRUE, invalid_rows = c("warn_drop", "error")) {
  prep <- .afd_prepare(data, loss, deductible, limit, weights, scale_by, invalid_rows)
  data <- prep$data
  loss <- prep$cols$loss; deductible <- prep$cols$deductible
  limit <- prep$cols$limit
  fits <- vector("list", length(distributions)); names(fits) <- distributions
  rows <- vector("list", length(distributions))
  for (i in seq_along(distributions)) {
    nm <- distributions[i]
    f <- tryCatch(
      fit_severity(data, loss, deductible, limit, nm, scale_by, scale_intercept,
                   weights, fixed = fixed[[nm]] %||% list(), control = control,
                   hessian = hessian, invalid_rows = "error"),
      error = function(e) e
    )
    if (inherits(f, "actuarialfitdist_fit")) {
      f$excluded_rows <- prep$excluded_rows
      f$excluded_details <- prep$excluded_details
      f$retained_rows <- prep$retained_rows
    }
    fits[[i]] <- f
    if (inherits(f, "actuarialfitdist_fit")) {
      rows[[i]] <- data.frame(
        distribution = f$distribution, converged = f$convergence == 0L,
        logLik = f$logLik, AIC = f$AIC, BIC = f$BIC,
        parameters = .afd_param_count(f), n = f$n, n_censored = f$n_censored,
        error = NA_character_, stringsAsFactors = FALSE
      )
    } else {
      rows[[i]] <- data.frame(
        distribution = nm, converged = FALSE, logLik = NA_real_, AIC = NA_real_, BIC = NA_real_,
        parameters = NA_integer_, n = nrow(data), n_censored = NA_integer_,
        error = conditionMessage(f), stringsAsFactors = FALSE
      )
    }
  }
  out <- list(call = match.call(), fits = fits, comparison = do.call(rbind, rows),
              excluded_rows = prep$excluded_rows, excluded_details = prep$excluded_details)
  class(out) <- "actuarialfitdist_candidates"
  out
}

`%||%` <- function(x, y) if (is.null(x)) y else x
