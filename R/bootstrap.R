# Bootstrap inference -----------------------------------------------------

.afd_refit_like <- function(fit, data, refit_threshold = FALSE, hessian = FALSE) {
  c <- fit$columns
  if (fit$model == "single") {
    return(fit_severity(
      data = data, loss = c$loss, deductible = c$deductible, limit = c$limit,
      distribution = fit$distribution, scale_by = c$scale_by,
      scale_intercept = fit$scale_intercept, weights = c$weights,
      fixed = fit$fixed, hessian = hessian
    ))
  }
  fit_spliced_severity(
    data = data, loss = c$loss, deductible = c$deductible, limit = c$limit,
    body = fit$body, tail = fit$tail,
    threshold = if (isTRUE(refit_threshold) && fit$threshold_method == "search") NULL else fit$threshold,
    threshold_method = if (isTRUE(refit_threshold)) fit$threshold_method else "fixed",
    scale_by = c$scale_by, scale_intercept = fit$scale_intercept,
    weights = c$weights, fixed = fit$fixed, hessian = hessian
  )
}

#' Bootstrap a fitted severity model
#'
#' Provides a deliberately lightweight bootstrap. Parametric bootstrap retains
#' the fitted portfolio's deductible, limit, scaling, and weight structure,
#' simulates observed claims conditional on exceeding the deductible, and
#' refits the model. Case bootstrap resamples claim rows with replacement.
#'
#' @param object Fitted model.
#' @param R Number of bootstrap replicates.
#' @param type `"parametric"` or `"case"`.
#' @param seed Optional random seed.
#' @param refit_threshold For a searched spliced model, whether each bootstrap
#'   replicate should repeat threshold search. Default `FALSE` reuses the
#'   selected threshold to keep the bootstrap inexpensive and stable.
#' @return An object of class `actuarialfitdist_bootstrap` containing coefficient
#'   estimates and convergence status by replicate.
#' @examples
#' set.seed(123)
#' d <- data.frame(loss = rlnorm(120, log(10000), 0.8))
#' fit <- fit_severity(d, "loss", distribution = "lognormal", hessian = FALSE)
#' boot <- bootstrap_fit(fit, R = 3, type = "case", seed = 1)
#' boot$success_rate
#' @export
bootstrap_fit <- function(object, R = 200, type = c("parametric", "case"),
                          seed = NULL, refit_threshold = FALSE) {
  if (!inherits(object, "actuarialfitdist_fit")) .afd_stop("`object` must be a fitted model.")
  type <- match.arg(type); R <- as.integer(R)
  if (R < 1L) .afd_stop("`R` must be at least 1.")
  if (!is.null(seed)) set.seed(seed)
  rows <- vector("list", R)
  for (r in seq_len(R)) {
    dat <- if (type == "case") {
      object$data[sample.int(nrow(object$data), replace = TRUE), , drop = FALSE]
    } else {
      sim <- stats::simulate(object, nsim = 1, type = "observed", newdata = object$data)
      d <- object$data
      d[[object$columns$loss]] <- sim$observed_loss
      d
    }
    fr <- tryCatch(.afd_refit_like(object, dat, refit_threshold, hessian = FALSE), error = function(e) e)
    if (inherits(fr, "actuarialfitdist_fit")) {
      co <- as.list(fr$coefficients)
      rows[[r]] <- c(list(replicate = r, converged = fr$convergence == 0L,
                          threshold = fr$threshold %||% NA_real_, error = NA_character_), co)
    } else {
      rows[[r]] <- list(replicate = r, converged = FALSE, threshold = NA_real_, error = conditionMessage(fr))
    }
  }
  all_names <- unique(unlist(lapply(rows, names)))
  mat <- lapply(rows, function(z) {
    miss <- setdiff(all_names, names(z)); z[miss] <- NA
    as.data.frame(z[all_names], stringsAsFactors = FALSE)
  })
  estimates <- do.call(rbind, mat)
  # Coerce coefficient columns back to numeric where possible.
  protected <- c("replicate", "converged", "error")
  for (nm in setdiff(names(estimates), protected)) estimates[[nm]] <- suppressWarnings(as.numeric(estimates[[nm]]))
  out <- list(call = match.call(), type = type, R = R, original = object,
              estimates = estimates, success_rate = mean(estimates$converged))
  class(out) <- "actuarialfitdist_bootstrap"
  out
}
