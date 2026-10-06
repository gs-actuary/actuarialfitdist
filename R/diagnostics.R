# Goodness-of-fit diagnostics --------------------------------------------

.afd_filter_scale_range <- function(fit, data, scale_range) {
  if (is.null(scale_range)) return(rep(TRUE, nrow(data)))
  sc <- fit$columns$scale_by
  if (is.null(sc)) .afd_stop("`scale_range` is only available for a model fitted with `scale_by`.")
  if (length(scale_range) != 2L || any(!is.finite(scale_range)) || scale_range[1] > scale_range[2]) {
    .afd_stop("`scale_range` must contain two increasing finite values.")
  }
  data[[sc]] >= scale_range[1] & data[[sc]] <= scale_range[2]
}

#' Data underlying observed-versus-modeled severity plots
#'
#' Produces a censoring- and truncation-aware comparison data set. Modeled
#' values are simulated under the fitted model using each selected row's actual
#' deductible, limit, and scaling value. This avoids the misleading practice of
#' overlaying one unconditional fitted density on a heterogeneous portfolio of
#' deductible-truncated and limit-censored claims.
#'
#' @param object Fitted model.
#' @param scale_range Optional two-element range of fitted scaling values.
#' @param nsim Number of simulated observed portfolios.
#' @param seed Optional random seed.
#' @return Long data frame with actual and simulated observed severities.
#' @export
fit_plot_data <- function(object, scale_range = NULL, nsim = 20, seed = NULL) {
  if (!inherits(object, "actuarialfitdist_fit")) .afd_stop("`object` must be a fitted actuarialfitdist model.")
  keep <- .afd_filter_scale_range(object, object$data, scale_range)
  dat <- object$data[keep, , drop = FALSE]
  if (!nrow(dat)) .afd_stop("No observations remain after applying `scale_range`.")
  cols <- object$columns
  row_weight <- if (is.null(cols$weights)) rep(1, nrow(dat)) else dat[[cols$weights]]
  actual <- data.frame(
    row = seq_len(nrow(dat)), source = "actual", simulation = 0L,
    observed_loss = pmin(dat[[cols$loss]], dat[[cols$limit]]),
    censored = is.finite(dat[[cols$limit]]) & dat[[cols$loss]] >= dat[[cols$limit]],
    deductible = dat[[cols$deductible]], limit = dat[[cols$limit]],
    scale_value = if (is.null(cols$scale_by)) NA_real_ else dat[[cols$scale_by]],
    weight = row_weight,
    stringsAsFactors = FALSE
  )
  sim <- stats::simulate(object, nsim = nsim, seed = seed, type = "observed", newdata = dat)
  sim$source <- "simulated"
  sim$weight <- row_weight[sim$row]
  sim <- sim[, names(actual), drop = FALSE]
  rbind(actual, sim)
}

.afd_empirical_curve <- function(dat, survival = FALSE) {
  split_key <- interaction(dat$source, dat$simulation, drop = TRUE)
  pieces <- lapply(split(dat, split_key), function(d) {
    ord <- order(d$observed_loss)
    xs <- d$observed_loss[ord]
    ww <- d$weight[ord]
    cum <- cumsum(ww) / sum(ww)
    y <- cum
    if (survival) y <- 1 - c(0, utils::head(cum, -1))
    data.frame(source = d$source[1], simulation = d$simulation[1], x = xs, y = y)
  })
  do.call(rbind, pieces)
}

#' Plot censoring-aware goodness of fit
#'
#' Plots actual observed claim severities against severities simulated from the
#' fitted model after reproducing the same deductible, limit, and scaling
#' structure. This avoids comparing truncated/censored data with an
#' inappropriate unconditional ground-up density.
#'
#' @param object Fitted model.
#' @param type One of `"histogram"`, `"density"`, `"cdf"`, or `"survival"`.
#' @param scale_range Optional range of scaling values to inspect.
#' @param nsim Number of simulated observed portfolios.
#' @param seed Optional random seed.
#' @param bins Histogram bin count.
#' @return A `ggplot2` object. Use `fit_plot_data()` for the underlying data.
#' @export
plot_fit <- function(object, type = c("histogram", "density", "cdf", "survival"),
                     scale_range = NULL, nsim = 20, seed = NULL, bins = 30) {
  type <- match.arg(type)
  dat <- fit_plot_data(object, scale_range, nsim, seed)
  if (type == "histogram") {
    return(ggplot2::ggplot(dat, ggplot2::aes(x = observed_loss, y = ggplot2::after_stat(density), weight = weight, fill = source)) +
      ggplot2::geom_histogram(position = "identity", alpha = 0.35, bins = bins) +
      ggplot2::labs(x = "Observed claim severity", y = "Density", fill = NULL,
                    title = "Observed vs simulated observed severity") + ggplot2::theme_minimal())
  }
  if (type == "density") {
    return(ggplot2::ggplot(dat, ggplot2::aes(x = observed_loss, weight = weight, group = interaction(source, simulation), colour = source)) +
      ggplot2::geom_density(alpha = 0.25) +
      ggplot2::labs(x = "Observed claim severity", y = "Density", colour = NULL,
                    title = "Observed vs simulated observed severity") + ggplot2::theme_minimal())
  }
  curve <- .afd_empirical_curve(dat, survival = type == "survival")
  ggplot2::ggplot(curve, ggplot2::aes(x = x, y = y, group = interaction(source, simulation), colour = source)) +
    ggplot2::geom_line(alpha = 0.45) +
    ggplot2::labs(x = "Observed claim severity", y = if (type == "survival") "Empirical survival" else "Empirical CDF",
                  colour = NULL, title = paste("Observed vs simulated", toupper(type))) +
    ggplot2::theme_minimal()
}

#' Data underlying the scaling relationship diagnostic
#'
#' Bins the fitted scaling variable and compares actual mean observed severity
#' with the mean of simulated observed severities generated under the same row-
#' level deductibles and limits. It also reports the fitted ground-up mean at a
#' representative scaling value in each bin. The first two series are directly
#' comparable on the observed/censored basis; the ground-up mean is included to
#' display the model's implied severity-scaling relationship.
#'
#' @param object Model fitted with `scale_by`.
#' @param bins Number of bins.
#' @param bin_method `"quantile"` or `"width"`.
#' @param scale_range Optional scaling range.
#' @param nsim Number of simulated portfolios.
#' @param seed Optional random seed.
#' @return Data frame with one row per scale bin.
#' @export
scaling_plot_data <- function(object, bins = 10, bin_method = c("quantile", "width"),
                              scale_range = NULL, nsim = 50, seed = NULL) {
  if (!inherits(object, "actuarialfitdist_fit")) .afd_stop("`object` must be a fitted model.")
  sc <- object$columns$scale_by
  if (is.null(sc)) .afd_stop("Scaling diagnostics require a model fitted with `scale_by`.")
  bin_method <- match.arg(bin_method)
  keep <- .afd_filter_scale_range(object, object$data, scale_range)
  dat <- object$data[keep, , drop = FALSE]
  z <- dat[[sc]]
  bins <- as.integer(bins)
  if (bins < 2L) .afd_stop("`bins` must be at least 2.")
  if (bin_method == "quantile") {
    br <- unique(as.numeric(stats::quantile(z, probs = seq(0, 1, length.out = bins + 1), names = FALSE, type = 8)))
  } else br <- seq(min(z), max(z), length.out = bins + 1)
  if (length(br) < 3L) .afd_stop("The scaling variable has too few distinct values for the requested bins.")
  bin <- cut(z, breaks = br, include.lowest = TRUE, labels = FALSE)
  actual_obs <- pmin(dat[[object$columns$loss]], dat[[object$columns$limit]])
  ww <- if (is.null(object$columns$weights)) rep(1, nrow(dat)) else dat[[object$columns$weights]]
  sim <- stats::simulate(object, nsim = nsim, seed = seed, type = "observed", newdata = dat)
  sim$bin <- rep(bin, times = nsim)
  sim$weight <- ww[sim$row]
  wmean <- function(x, w) sum(x * w) / sum(w)
  sim_by_bin <- vapply(sort(unique(sim$bin)), function(b) {
    ii <- sim$bin == b
    wmean(sim$observed_loss[ii], sim$weight[ii])
  }, numeric(1))
  names(sim_by_bin) <- sort(unique(sim$bin))
  ids <- sort(unique(bin))
  out <- lapply(ids, function(b) {
    idx <- bin == b
    zv <- stats::median(z[idx])
    data.frame(
      bin = b,
      scale_value = zv,
      n = sum(ww[idx]),
      actual_observed_mean = sum(actual_obs[idx] * ww[idx]) / sum(ww[idx]),
      modeled_observed_mean = unname(sim_by_bin[as.character(b)]),
      fitted_ground_up_mean = .afd_fit_mean(object, zv),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, out)
}

#' Plot the fitted scaling relationship
#'
#' Compares empirical observed mean severity by scale bin with simulated
#' observed means and the fitted ground-up mean relationship.
#'
#' @inheritParams scaling_plot_data
#' @return A ggplot showing actual and modeled observed means by scale bin, plus
#'   the fitted ground-up mean relationship.
#' @export
plot_scaling_fit <- function(object, bins = 10, bin_method = c("quantile", "width"),
                             scale_range = NULL, nsim = 50, seed = NULL) {
  dat <- scaling_plot_data(object, bins, bin_method, scale_range, nsim, seed)
  long <- rbind(
    data.frame(scale_value = dat$scale_value, mean = dat$actual_observed_mean, series = "Actual observed mean"),
    data.frame(scale_value = dat$scale_value, mean = dat$modeled_observed_mean, series = "Modeled observed mean"),
    data.frame(scale_value = dat$scale_value, mean = dat$fitted_ground_up_mean, series = "Fitted ground-up mean")
  )
  ggplot2::ggplot(long, ggplot2::aes(x = scale_value, y = mean, colour = series, group = series)) +
    ggplot2::geom_line() + ggplot2::geom_point() +
    ggplot2::labs(x = object$columns$scale_by, y = "Mean severity", colour = NULL,
                  title = "Severity scaling diagnostic") + ggplot2::theme_minimal()
}

#' Diagnose fit within a severity layer
#'
#' Compares actual and simulated claim behavior within a user-selected observed
#' severity layer, optionally restricted to a range of scaling values.
#'
#' @param object Fitted model.
#' @param lower Lower observed-severity bound.
#' @param upper Upper observed-severity bound.
#' @param scale_range Optional range of scaling values.
#' @param nsim Number of simulated observed portfolios.
#' @param seed Optional random seed.
#' @return A data frame comparing actual and simulated behavior in the layer.
#' @export
diagnose_fit <- function(object, lower = 0, upper = Inf, scale_range = NULL,
                         nsim = 100, seed = NULL) {
  if (!is.finite(lower) || lower < 0 || (!(is.finite(upper) || is.infinite(upper))) || upper <= lower) {
    .afd_stop("Supply a nonnegative `lower` and an `upper` greater than `lower`; `Inf` is allowed.")
  }
  dat <- fit_plot_data(object, scale_range, nsim, seed)
  pieces <- split(dat, dat$source)
  calc <- function(d) {
    inside <- d$observed_loss >= lower & d$observed_loss < upper
    w <- d$weight
    total_w <- sum(w)
    layer_w <- sum(w[inside])
    data.frame(
      source = d$source[1],
      rows = total_w,
      layer_count = layer_w,
      layer_proportion = layer_w / total_w,
      layer_mean = if (layer_w > 0) sum(d$observed_loss[inside] * w[inside]) / layer_w else NA_real_,
      censoring_rate = sum(d$censored * w) / total_w,
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, lapply(pieces, calc))
  # Pooling all simulations makes counts incomparable to one observed portfolio;
  # normalize simulated count to the average count per replicate.
  if ("simulated" %in% out$source) {
    i <- which(out$source == "simulated")
    out$layer_count[i] <- out$layer_count[i] / nsim
    out$rows[i] <- out$rows[i] / nsim
  }
  rownames(out) <- NULL
  out
}
