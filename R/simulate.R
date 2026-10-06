# Simulation --------------------------------------------------------------

.afd_sim_ground_up <- function(fit, scale_values) {
  n <- length(scale_values)
  if (fit$model == "single") {
    if (is.null(fit$columns$scale_by)) {
      parts <- .afd_single_eval_parts(fit, NULL)
      s <- rep(parts$s, n)
    } else {
      parts <- .afd_single_eval_parts(fit, scale_values)
      s <- parts$s
    }
    F0 <- .afd_p(fit$distribution, 0, s, parts$shape)
    target <- F0 + stats::runif(n) * (1 - F0)
    return(.afd_q(fit$distribution, target, s, parts$shape))
  }
  p <- stats::runif(n)
  z <- if (is.null(fit$columns$scale_by)) NULL else scale_values
  .afd_splice_quantile(p, fit$threshold, fit$body, fit$tail, as.list(fit$coefficients), z)
}

.afd_sim_observed_rows <- function(fit, data, reps = 1L) {
  cols <- fit$columns
  n <- nrow(data)
  d <- data[[cols$deductible]]; u <- data[[cols$limit]]
  z <- if (is.null(cols$scale_by)) rep(NA_real_, n) else data[[cols$scale_by]]
  ans <- vector("list", reps)
  for (r in seq_len(reps)) {
    # Simulate from the fitted distribution conditional on exceeding each row's deductible.
    Fd <- .afd_fit_cdf(fit, d, if (all(is.na(z))) NULL else z)
    target <- Fd + stats::runif(n) * (1 - Fd)
    ground <- .afd_fit_quantile(fit, target, if (all(is.na(z))) NULL else z)
    observed <- pmin(ground, u)
    censored <- is.finite(u) & ground >= u
    ans[[r]] <- data.frame(
      row = seq_len(n), simulation = r, ground_up = ground,
      observed_loss = observed, censored = censored,
      deductible = d, limit = u,
      scale_value = z,
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, ans)
}

#' Simulate from a fitted severity model
#'
#' Simulates either positive ground-up severities or the observed claims that
#' result after reproducing row-level deductible truncation and limit censoring.
#'
#' @param object Fitted model.
#' @param nsim Number of simulated replicates.
#' @param seed Optional random seed.
#' @param type `"observed"` reproduces deductible truncation and limit censoring
#'   using rows of `newdata`; `"ground_up"` simulates positive ground-up severity.
#' @param newdata Optional data frame. Defaults to the fitting data.
#' @param n Number of ground-up draws when `type = "ground_up"`. Defaults to the
#'   number of rows in `newdata`.
#' @param scale_value Optional scale values for ground-up simulation.
#' @param ... Unused.
#' @export
simulate.actuarialfitdist_fit <- function(object, nsim = 1, seed = NULL,
                                          type = c("observed", "ground_up"),
                                          newdata = object$data, n = NULL,
                                          scale_value = NULL, ...) {
  type <- match.arg(type)
  if (!is.null(seed)) set.seed(seed)
  nsim <- as.integer(nsim)
  if (nsim < 1L) .afd_stop("`nsim` must be at least 1.")
  if (type == "observed") {
    needed <- unlist(object$columns[c("deductible", "limit", "scale_by")], use.names = FALSE)
    needed <- needed[!is.na(needed) & nzchar(needed)]
    if (!is.data.frame(newdata) || any(!needed %in% names(newdata))) .afd_stop("`newdata` does not contain all columns needed to reproduce the observation mechanism.")
    return(.afd_sim_observed_rows(object, newdata, nsim))
  }
  if (is.null(n)) n <- if (!is.null(newdata)) nrow(newdata) else if (!is.null(scale_value)) length(scale_value) else object$n
  n <- as.integer(n)
  if (n < 1L) .afd_stop("`n` must be at least 1.")
  if (!is.null(object$columns$scale_by)) {
    if (is.null(scale_value)) {
      if (!is.null(newdata) && object$columns$scale_by %in% names(newdata)) {
        scale_value <- rep(newdata[[object$columns$scale_by]], length.out = n)
      } else .afd_stop("Supply `scale_value` or `newdata` containing the scaling column.")
    }
    scale_values <- rep(scale_value, length.out = n)
  } else scale_values <- rep(NA_real_, n)
  out <- lapply(seq_len(nsim), function(r) data.frame(
    simulation = r,
    ground_up = .afd_sim_ground_up(object, scale_values),
    scale_value = scale_values
  ))
  do.call(rbind, out)
}
