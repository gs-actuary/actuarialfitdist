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
#' @param deductible Character scalar naming the deductible column.
#' @param limit Character scalar naming the ground-up censoring limit column.
#'   Use `Inf` in the data for claims that are not subject to a finite limit.
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
#' @return An object of class `actuarialfitdist_fit`.
#' @export
fit_severity <- function(data, loss, deductible, limit, distribution,
                         scale_by = NULL, scale_intercept = FALSE,
                         weights = NULL, fixed = list(), control = list(),
                         hessian = TRUE) {
  if (!is.data.frame(data)) .afd_stop("`data` must be a data frame.")
  loss_col <- .afd_match_col(data, loss, "loss")
  ded_col <- .afd_match_col(data, deductible, "deductible")
  lim_col <- .afd_match_col(data, limit, "limit")
  scale_col <- .afd_match_col(data, scale_by, "scale_by", required = FALSE)
  weight_col <- .afd_match_col(data, weights, "weights", required = FALSE)

  x <- data[[loss_col]]
  d <- data[[ded_col]]
  u <- data[[lim_col]]
  w <- if (is.null(weight_col)) rep(1, nrow(data)) else data[[weight_col]]
  z <- if (is.null(scale_col)) NULL else data[[scale_col]]
  .afd_validate_ground_up(x, d, u, w, z)

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
    weights = w,
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
#' @return An object of class `actuarialfitdist_candidates`.
#' @export
fit_severity_candidates <- function(data, loss, deductible, limit,
                                    distributions = c(
                                      "weibull", "gamma", "lognormal", "pareto2", "burr",
                                      "normal", "exponential", "loglogistic", "invgauss", "gpd"
                                    ),
                                    scale_by = NULL, scale_intercept = FALSE,
                                    weights = NULL, fixed = list(), control = list(),
                                    hessian = TRUE) {
  fits <- vector("list", length(distributions)); names(fits) <- distributions
  rows <- vector("list", length(distributions))
  for (i in seq_along(distributions)) {
    nm <- distributions[i]
    f <- tryCatch(
      fit_severity(data, loss, deductible, limit, nm, scale_by, scale_intercept,
                   weights, fixed = fixed[[nm]] %||% list(), control = control,
                   hessian = hessian),
      error = function(e) e
    )
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
  out <- list(call = match.call(), fits = fits, comparison = do.call(rbind, rows))
  class(out) <- "actuarialfitdist_candidates"
  out
}

`%||%` <- function(x, y) if (is.null(x)) y else x
