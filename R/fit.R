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
#' @param control List passed to `stats::optim()`. When an additive scale
#'   intercept is estimated, the package sets the optimizer's `parscale` for
#'   that coordinate to `max(1000, 0.1 * median(loss))`; other free coordinates
#'   use 1. Explicit `control$parscale` overrides this heuristic and must
#'   follow the order in `$optim_par`. This affects numerical optimization,
#'   not the statistical scale model or reported coefficients. The fitter
#'   checks a zero-intercept anchor where applicable and retries numerically
#'   sensitive families. Invalid apparent convergence is rejected.
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
  started <- proc.time()[["elapsed"]]
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
  groups <- .afd_likelihood_groups(d, u, z)
  optim_control <- .afd_optimizer_control(control, packed$unknown, x)

  objective <- function(opt) {
    params <- packed$decode(opt)
    ll <- .afd_single_loglik(dist, params, x, d, u, w, z, groups)
    if (!is.finite(ll)) return(1e100)
    -ll
  }

  # Fit the nested zero-intercept model first when an additive intercept is
  # free. Its maximum is a likelihood floor for the larger model. This also
  # gives a data-informed starting point close to the correct scaling slope.
  # In particular, it prevents a constant-scale local optimum from winning
  # simply because optim() reports BFGS convergence code zero.
  anchor <- NULL
  if (!is.null(z) && isTRUE(scale_intercept) &&
      "scale_intercept" %in% packed$unknown) {
    anchor_control <- control
    anchor_control$parscale <- NULL  # Different parameter dimension/order.
    anchor <- tryCatch(
      fit_severity(data, loss = loss_col, deductible = ded_col, limit = lim_col,
                   distribution = dist, scale_by = scale_col,
                   scale_intercept = FALSE, weights = weight_col,
                   fixed = packed$fixed, control = anchor_control,
                   hessian = FALSE, invalid_rows = "error"),
      error = function(e) NULL
    )
    if (!is.null(anchor) && anchor$convergence != 0L) anchor <- NULL
  }

  if (length(packed$start)) {
    # Internal optimizers use the encoded (log-positive) parameterization.
    # Starts from several well-defined points are needed for flexible families.
    # Only a finite, numerically admissible, converged maximum is accepted.
    replace_start <- function(seed, name, natural) {
      if (name %in% packed$unknown) {
        seed[name] <- if (spec[[name]]$transform == "positive") log(natural) else natural
      }
      seed
    }
    starts <- list()
    if (!is.null(anchor)) {
      warm <- packed$start
      for (nm in intersect(names(anchor$coefficients), packed$unknown)) {
        warm <- replace_start(warm, nm, anchor$coefficients[[nm]])
      }
      warm <- replace_start(warm, "scale_intercept", 0)
      starts[[length(starts) + 1L]] <- warm
    }
    starts[[length(starts) + 1L]] <- packed$start
    if (dist == "gamma" && "shape" %in% packed$unknown) {
      for (shape in c(0.75, 4)) {
        starts[[length(starts) + 1L]] <- replace_start(packed$start, "shape", shape)
      }
    } else if (dist == "burr" &&
               any(c("shape1", "shape2") %in% packed$unknown)) {
      for (pair in list(c(0.9, 1.5), c(2.5, 1.5))) {
        seed <- replace_start(packed$start, "shape1", pair[1L])
        starts[[length(starts) + 1L]] <- replace_start(seed, "shape2", pair[2L])
      }
    }

    best <- NULL
    best_control <- optim_control
    total_counts <- c("function" = 0, "gradient" = 0)
    attempts <- 0L
    run_start <- function(seed, ctl) {
      tryCatch(suppressWarnings(stats::optim(
        par = seed, fn = objective, method = "BFGS", control = ctl
      )), error = function(e) NULL)
    }
    for (seed in starts) {
      trial <- run_start(seed, optim_control)
      attempts <- attempts + 1L
      if (is.null(trial)) next
      total_counts <- total_counts + trial$counts[names(total_counts)]
      if (trial$convergence != 0L || !is.finite(trial$value) ||
          trial$value >= 1e90) next
      trial_components <- .afd_single_components(dist, packed$decode(trial$par),
                                                  x, d, u, z, groups)
      if (is.null(trial_components) ||
          any(!is.finite(trial_components$loglik[w > 0]))) next
      # The unrestricted model must be at least as good as its nested submodel.
      if (!is.null(anchor) && -trial$value < anchor$logLik -
          1e-8 * max(1, abs(anchor$logLik))) next
      if (is.null(best) || trial$value < best$value) best <- trial
      # Retain normal-family speed; search alternative basins for the most
      # numerically delicate distributions even after one converged fit.
      if (!dist %in% c("gamma", "burr") ||
          !any(c("shape", "shape1", "shape2") %in% packed$unknown)) break
    }
    # A user-specified parscale is honored. For the automatic heuristic, retry
    # the anchor at an alternative scale if the first attempts were rejected.
    if (is.null(best) && !is.null(anchor) && is.null(control$parscale)) {
      ctl <- optim_control
      j <- match("scale_intercept", packed$unknown)
      ctl$parscale[j] <- max(50000, stats::median(x))
      trial <- run_start(starts[[1L]], ctl)
      attempts <- attempts + 1L
      if (!is.null(trial)) {
        total_counts <- total_counts + trial$counts[names(total_counts)]
        if (trial$convergence == 0L && is.finite(trial$value) && trial$value < 1e90 &&
            -trial$value >= anchor$logLik - 1e-8 * max(1, abs(anchor$logLik))) {
          cc <- .afd_single_components(dist, packed$decode(trial$par), x, d, u, z, groups)
          if (!is.null(cc) && all(is.finite(cc$loglik[w > 0]))) {
            best <- trial
            best_control <- ctl
          }
        }
      }
    }
    if (is.null(best)) {
      .afd_stop("No valid converged maximum was found for `", dist,
                "`. The optimizer reached invalid or inferior parameter values; ",
                "do not use this family's information criteria. Try a different ",
                "starting configuration or a simpler model.")
    }
    optim_par <- best$par
    convergence <- best$convergence
    message <- best$message
    value <- best$value
    optim_counts <- total_counts
    optim_control <- best_control
  } else {
    optim_par <- numeric()
    convergence <- 0L
    message <- "All parameters fixed; no optimization performed."
    value <- objective(numeric())
    optim_counts <- c("function" = 0L, gradient = 0L)
  }
  params <- packed$decode(optim_par)
  components <- .afd_single_components(dist, params, x, d, u, z)
  if (is.null(components) || !is.finite(value) || value >= 1e90 ||
      any(!is.finite(components$loglik[w > 0]))) {
    .afd_stop("The fitted parameter combination is invalid.")
  }

  H <- Vopt <- Vnat <- NULL
  if (isTRUE(hessian) && length(optim_par) && convergence == 0L) {
    H <- .afd_num_hessian(objective, optim_par, parscale = optim_control$parscale)
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
    optimizer_counts = optim_counts, optimizer_control = optim_control,
    optimizer_attempts = if (length(packed$start)) attempts else 0L,
    elapsed_seconds = unname(proc.time()[["elapsed"]] - started),
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
#' both the fitted models and a comparison table with per-distribution
#' optimization runtimes, evaluation counts, and convergence codes. The function intentionally
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
    t0 <- proc.time()[["elapsed"]]
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
    seconds <- unname(proc.time()[["elapsed"]] - t0)
    if (inherits(f, "actuarialfitdist_fit")) {
      rows[[i]] <- data.frame(
        distribution = f$distribution, converged = f$convergence == 0L,
        logLik = f$logLik, AIC = f$AIC, BIC = f$BIC,
        parameters = .afd_param_count(f), n = f$n, n_censored = f$n_censored,
        error = NA_character_, elapsed_seconds = seconds,
        function_evaluations = unname(f$optimizer_counts[["function"]]),
        gradient_evaluations = unname(f$optimizer_counts[["gradient"]]),
        convergence_code = f$convergence, stringsAsFactors = FALSE
      )
    } else {
      rows[[i]] <- data.frame(
        distribution = nm, converged = FALSE, logLik = NA_real_, AIC = NA_real_, BIC = NA_real_,
        parameters = NA_integer_, n = nrow(data), n_censored = NA_integer_,
        error = conditionMessage(f), elapsed_seconds = seconds,
        function_evaluations = NA_real_, gradient_evaluations = NA_real_,
        convergence_code = NA_integer_, stringsAsFactors = FALSE
      )
    }
  }
  out <- list(call = match.call(), fits = fits, comparison = do.call(rbind, rows),
              excluded_rows = prep$excluded_rows, excluded_details = prep$excluded_details)
  class(out) <- "actuarialfitdist_candidates"
  out
}

`%||%` <- function(x, y) if (is.null(x)) y else x
