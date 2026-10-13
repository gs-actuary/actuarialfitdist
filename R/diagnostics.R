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
#' @param output `"raw"` (default) preserves individual actual/simulated rows;
#'   `"plot_ready"` returns the exact deterministic summary consumed by the
#'   default plotting functions, with columns `source`, `x`, `value`, `lower`,
#'   and `upper`. `value` is probability density in the displayed x coordinate
#'   (per log10 severity when requested), or a CDF/survival probability.
#' @param type Summary type when `output = "plot_ready"`.
#' @param bins Number of histogram bins.
#' @param points Number of grid intervals for density/CDF/survival summaries.
#' @param x_scale `"log10"` or `"linear"` for plot-ready summaries.
#' @param xlim Optional grid interval. Unlike data filtering, the probabilities
#'   are computed relative to the complete selected portfolio.
#' @return A data frame. For `output = "raw"`, each selected original
#'     claim and each simulated replicate has `row`, `source`, `simulation`,
#'     `observed_loss`, `censored`, `deductible`, `limit`, `scale_value`, and `weight`.
#'     For `output = "plot_ready"`, columns `source`, `x`, `value`, `lower`, and
#'     `upper` give the deterministic observed and modeled values used in analytic
#'     plots. Histograms/densities use density per displayed coordinate (per
#'     log10-severity unit when `x_scale = "log10"`); CDF/survival values are
#'     probabilities. The analytic output does not depend on `nsim` or `seed`.
#' @examples
#' set.seed(123)
#' d <- data.frame(loss = rlnorm(120, log(10000), 0.8))
#' fit <- fit_severity(d, "loss", distribution = "lognormal", hessian = FALSE)
#' head(fit_plot_data(fit, nsim = 1, seed = 1))
#' @export
fit_plot_data <- function(object, scale_range = NULL, nsim = 20, seed = NULL,
                          output = c("raw", "plot_ready"),
                          type = c("density", "histogram", "cdf", "survival"),
                          bins = 40, points = 160, x_scale = c("log10", "linear"),
                          xlim = NULL) {
  output <- match.arg(output)
  if (output == "plot_ready") {
    return(.afd_plot_ready(object, scale_range, match.arg(type), bins, points,
                            match.arg(x_scale), xlim))
  }
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

# Shared x-axis styling helper (not a user-facing function).
.afd_plot_x <- function(p, x_scale, xlim, x_breaks = NULL,
                        x_text_angle = 0, x_text_size = 9, y_text_size = 9) {
  x_scale <- match.arg(x_scale, c("log10", "linear"))
  if (!is.null(x_breaks) && (!is.numeric(x_breaks) || any(!is.finite(x_breaks)) ||
      (x_scale == "log10" && any(x_breaks <= 0)))) {
    .afd_stop("`x_breaks` must contain finite numeric values, positive for log10.")
  }
  if (x_scale == "log10") {
    p <- p + ggplot2::scale_x_log10(
      breaks = x_breaks %||% scales::breaks_log(n = 5),
      labels = scales::label_number(big.mark = ",", accuracy = 1))
  } else {
    p <- p + ggplot2::scale_x_continuous(
      breaks = x_breaks %||% scales::breaks_extended(n = 5),
      labels = scales::label_number(big.mark = ",", accuracy = 1))
  }
  if (!is.null(xlim)) {
    if (length(xlim) != 2L || any(!is.finite(xlim)) || xlim[1] >= xlim[2] ||
        (x_scale == "log10" && xlim[1] <= 0))
      .afd_stop("`xlim` must be two increasing finite values (positive for log10).")
    p <- p + ggplot2::coord_cartesian(xlim = xlim)
  }
  .afd_plot_style(p, x_text_angle, x_text_size, y_text_size)
}

.afd_plot_style <- function(p, x_text_angle = 0, x_text_size = 9, y_text_size = 9) {
  if (length(x_text_angle) != 1L || !is.finite(x_text_angle) ||
      length(x_text_size) != 1L || !is.finite(x_text_size) || x_text_size <= 0 ||
      length(y_text_size) != 1L || !is.finite(y_text_size) || y_text_size <= 0)
    .afd_stop("Axis angles and text sizes must be finite scalars; sizes must be positive.")
  p + ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = x_text_angle,
      hjust = if (x_text_angle == 0) .5 else 1, vjust = 1, size = x_text_size),
    axis.text.y = ggplot2::element_text(size = y_text_size))
}

#' Plot censoring-aware goodness of fit
#'
#' The default log10 severity axis reveals the body of heavy-tailed claim
#' distributions without discarding large losses. For histograms, ggplot
#' computes density in log-severity coordinates, ensuring simulated and
#' observed bars use the same normalization. Axis limits only zoom the view.
#'
#' @section Analytic versus simulation plots:
#' `method = "analytic"` (the default) calculates expected observed-claim
#' probabilities across the actual mix of deductibles, limits, and scaling
#' values. It is deterministic and ignores `nsim` and `seed`. Use
#' `method = "simulation"` when you want to see Monte Carlo variability;
#' `nsim` controls the number of simulated portfolios in that mode.
#' @inheritParams fit_plot_data
#' @param type `"histogram"`, `"density"`, `"cdf"`, or `"survival"`.
#' @param bins Number of histogram bins.
#' @param x_scale `"log10"` (default) or `"linear"`.
#' @param xlim Optional visible x-axis range (without dropping observations).
#' @param method `"analytic"` (default) computes fitted expected bin counts
#'   and CDF directly, including censoring atoms. `"simulation"` retains the
#'   earlier simulation-based diagnostic; `nsim` is used only in this mode.
#' @param x_breaks Optional user-supplied x-axis tick positions.
#' @param x_text_angle Axis tick label rotation in degrees.
#' @param x_text_size Horizontal-axis tick label font size in points.
#' @param y_text_size Vertical-axis tick label font size in points.
#' @param points Number of intervals for analytic density and CDF summaries.
#' @return A `ggplot2` plot comparing observed severity with modeled
#'     observations on the same deductible, limit and scaling basis. The default
#'     analytic method uses deterministic expected probabilities; choose
#'     `method = "simulation"` for a Monte Carlo comparison. To customize the
#'     underlying analytic values, use `fit_plot_data(output = "plot_ready")`.
#' @examples
#' set.seed(123)
#' d <- data.frame(loss = rlnorm(120, log(10000), 0.8))
#' fit <- fit_severity(d, "loss", distribution = "lognormal", hessian = FALSE)
#' plot_fit(fit, x_scale = "log10")
#' @export
plot_fit <- function(object, type = c("histogram", "density", "cdf", "survival"),
                     scale_range = NULL, nsim = 20, seed = NULL, bins = 30,
                     x_scale = c("log10", "linear"), xlim = NULL,
                     method = c("analytic", "simulation"), points = 160,
                     x_breaks = NULL, x_text_angle = 0,
                     x_text_size = 9, y_text_size = 9) {
  type <- match.arg(type); x_scale <- match.arg(x_scale)
  method <- match.arg(method)
  if (method == "analytic") {
    dat <- fit_plot_data(object, scale_range = scale_range,
      output = "plot_ready", type = type, bins = bins, points = points,
      x_scale = x_scale, xlim = xlim)
    p <- .afd_plot_ready_ggplot(dat, type, facet = FALSE,
      title = "Observed vs fitted claim severity", x_scale = x_scale)
    return(.afd_plot_x(p, x_scale, xlim, x_breaks,
                       x_text_angle, x_text_size, y_text_size))
  }
  dat <- fit_plot_data(object, scale_range, nsim, seed)
  if (x_scale == "log10" && any(dat$observed_loss <= 0))
    .afd_stop("Log10 plots require positive observed severities; select `x_scale = 'linear'`.")
  if (type == "histogram") {
    p <- ggplot2::ggplot(dat, ggplot2::aes(x = observed_loss,
       y = ggplot2::after_stat(density), weight = weight, fill = source,
       group = source)) +
       ggplot2::geom_histogram(position = "identity", alpha = 0.35, bins = bins) +
       ggplot2::labs(x = "Observed claim severity", y = if (x_scale == "log10")
         "Density per log10 severity" else "Density", fill = NULL,
         title = "Observed vs simulated claim severity") + ggplot2::theme_minimal()
  } else if (type == "density") {
    p <- ggplot2::ggplot(dat, ggplot2::aes(x = observed_loss, weight = weight,
      group = interaction(source, simulation), colour = source)) +
      ggplot2::geom_density(alpha = .3) +
      ggplot2::labs(x = "Observed claim severity", y = if (x_scale == "log10")
        "Density per log10 severity" else "Density", colour = NULL,
        title = "Observed vs simulated claim severity") + ggplot2::theme_minimal()
  } else {
    curve <- .afd_empirical_curve(dat, survival = type == "survival")
    p <- ggplot2::ggplot(curve, ggplot2::aes(x = x, y = y,
      group = interaction(source, simulation), colour = source)) +
      ggplot2::geom_line(alpha = .45) +
      ggplot2::labs(x = "Observed claim severity",
        y = if (type == "survival") "Empirical survival" else "Empirical CDF",
        colour = NULL, title = paste("Observed vs simulated", toupper(type))) +
      ggplot2::theme_minimal()
  }
  .afd_plot_x(p, x_scale, xlim, x_breaks,
              x_text_angle, x_text_size, y_text_size)
}

#' Data for comparing several candidate severity distributions
#'
#' Recreates each candidate's row-level deductible truncation and limit
#' censoring, then returns actual and simulated observed claim amounts with
#' the same scale-range filter. Inspect or modify the data with ggplot2/plotly.
#'
#' @param object Output of `fit_severity_candidates()`.
#' @param scale_range Optional range of `scale_by` values.
#' @param nsim Number of observed-portfolio simulations per candidate.
#' @param seed Optional reproducibility seed.
#' @param output `"raw"` (default) returns claim-level simulations;
#'   `"plot_ready"` returns deterministic bin densities/CDF/survival values
#'   consumed by `plot_candidate_fits(method = "analytic")`.
#' @param type Plot-ready summary type.
#' @param bins Histogram bin count.
#' @param points Density/CDF grid intervals.
#' @param x_scale `"log10"` (default) or `"linear"` plot coordinate.
#' @param xlim Optional plot-ready grid range; not a claim filter.
#' @return A data frame with a `distribution` column for each
#'     successful candidate. `output = "raw"` returns actual and simulated
#'     claim-level observations with `observed_loss`, censoring flags and weights.
#'     `output = "plot_ready"` returns `source`, `x`, `value`, `lower`, `upper`, and
#'     `distribution`, matching the default candidate plot; `source = "modeled"`
#'     denotes expected portfolio probabilities, not Monte Carlo simulations.
#' @examples
#' set.seed(11)
#' d <- data.frame(loss = rlnorm(120, log(10000), 0.8))
#' candidates <- fit_severity_candidates(d, loss = "loss",
#'   distributions = c("lognormal", "gamma"), hessian = FALSE)
#' head(candidate_plot_data(candidates, nsim = 1, seed = 1))
#' @export
candidate_plot_data <- function(object, scale_range = NULL, nsim = 3, seed = NULL,
                                output = c("raw", "plot_ready"),
                                type = c("density", "histogram", "cdf", "survival"),
                                bins = 40, points = 160,
                                x_scale = c("log10", "linear"), xlim = NULL) {
  output <- match.arg(output)
  type <- match.arg(type)
  x_scale <- match.arg(x_scale)
  if (!inherits(object, "actuarialfitdist_candidates"))
    .afd_stop("`object` must be returned by `fit_severity_candidates()`. ")
  good <- vapply(object$fits, function(f) inherits(f, "actuarialfitdist_fit") &&
                    f$convergence == 0L, logical(1))
  if (!any(good)) .afd_stop("No converged candidate fits to plot.")
  if (output == "plot_ready") {
    chosen <- object$fits[good]
    first <- chosen[[1L]]
    keep <- .afd_filter_scale_range(first, first$data, scale_range)
    selected <- first$data[keep, , drop = FALSE]
    if (!nrow(selected)) .afd_stop("No observations remain after `scale_range`.")
    common_breaks <- .afd_plot_breaks(selected, first$columns,
      x_scale, xlim, bins, points, type)
    dfs <- lapply(chosen, function(f) {
      dat <- .afd_plot_ready(f, scale_range, type, bins, points,
                             x_scale, xlim, common_breaks)
      dat$distribution <- f$distribution
      dat
    })
    out <- do.call(rbind, dfs)
    rownames(out) <- NULL
    return(out)
  }
  if (!is.null(seed)) set.seed(seed)
  dfs <- lapply(object$fits[good], function(f) {
    dat <- fit_plot_data(f, scale_range = scale_range, nsim = nsim)
    dat$distribution <- f$distribution
    dat
  })
  out <- do.call(rbind, dfs)
  rownames(out) <- NULL
  out
}

# Plot precisely the tabular values produced by the public plot-ready builders.
.afd_plot_ready_ggplot <- function(dat, type, facet, title, x_scale) {
  dat$series <- ifelse(dat$source == "actual", "Observed", dat$distribution %||% "Fitted")
  if (!facet && "distribution" %in% names(dat)) {
    dat <- dat[dat$source != "actual" |
      !duplicated(paste(dat$x, dat$source)), , drop = FALSE]
  }
  if (type == "histogram") {
    p <- ggplot2::ggplot(dat, ggplot2::aes(xmin = .data$lower,
      xmax = .data$upper, ymin = 0, ymax = .data$value,
      fill = .data$series)) +
      ggplot2::geom_rect(alpha = .30, position = "identity")
  } else {
    p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$x, y = .data$value,
      colour = .data$series, group = .data$series)) +
      ggplot2::geom_line(linewidth = .65)
  }
  if (facet) p <- p + ggplot2::facet_wrap(~distribution, scales = "fixed")
  ylabel <- if (type %in% c("density", "histogram")) {
    if (x_scale == "log10") "Probability density per log10 severity" else "Probability density"
  } else if (type == "cdf") "CDF" else "Survival probability"
  p + ggplot2::labs(x = "Observed severity", y = ylabel,
                     title = title, colour = NULL, fill = NULL) +
    ggplot2::theme_minimal()
}

#' Compare observed and fitted severity across candidate distributions
#'
#' Facets or overlays observed severity against modeled severity histograms,
#' densities, CDFs or survival curves. The default uses expected probabilities,
#' not random simulations. Uses a shared log10 axis by default.
#' Fitted comparisons reproduce the observation mechanism, not unconditional
#' ground-up densities. Returned ggplot can be customized.
#'
#' @section Interpretation:
#' Analytic curves compare the empirical **observed** severity distribution
#' to the fitted distribution after the same observation rules. They are not
#' unconditional ground-up density curves. `nsim` and `seed` affect only
#' `method = "simulation"`. Models that fail fitting are omitted, and their
#' errors remain in `object$comparison`.
#' @param object Result of `fit_severity_candidates()`.
#' @param type Plot type: `"histogram"`, `"density"`, `"cdf"`, or `"survival"`.
#' @param scale_range Optional scale-by filter.
#' @param nsim Number of simulated portfolios when `method = "simulation"`;
#'   ignored by the deterministic analytic default.
#' @param seed Random seed.
#' @param bins Histogram bins.
#' @param x_scale `"log10"` (default) or `"linear"`.
#' @param xlim Optional visible x range.
#' @param facet Logical; facet models (default) or overlay them.
#' @param method `"analytic"` (default) uses fast deterministic modeled bin
#'   probabilities; `"simulation"` uses the older row-level simulation.
#' @param points Number of density and CDF grid intervals in analytic mode.
#' @param x_breaks Optional explicit severity-axis tick positions.
#' @param x_text_angle Axis label rotation (degrees); defaults to 45 in facets.
#' @param x_text_size Horizontal-axis tick font size in points.
#' @param y_text_size Vertical-axis tick font size in points.
#' @return A `ggplot2` plot, faceted by distribution unless
#'     `facet = FALSE`. Only successfully fitted candidates are plotted. The
#'     default is analytic (deterministic); `candidate_plot_data(output =
#'     "plot_ready")` returns the exact plotting values for custom charts.
#' @examples
#' set.seed(2)
#' d <- data.frame(loss = rlnorm(150, log(9000), 0.9))
#' fits <- fit_severity_candidates(d, "loss",
#'   distributions = c("lognormal", "weibull"), hessian = FALSE)
#' plot_candidate_fits(fits)
#' @export
plot_candidate_fits <- function(object, type = c("histogram", "density", "cdf", "survival"),
                                scale_range = NULL, nsim = 3, seed = NULL,
                                bins = 40, x_scale = c("log10", "linear"),
                                xlim = NULL, facet = TRUE,
                                method = c("analytic", "simulation"), points = 160,
                                x_breaks = NULL, x_text_angle = if (facet) 45 else 0,
                                x_text_size = 9, y_text_size = 9) {
  type <- match.arg(type); x_scale <- match.arg(x_scale)
  method <- match.arg(method)
  if (method == "analytic") {
    dat <- candidate_plot_data(object, scale_range = scale_range,
      output = "plot_ready", type = type, bins = bins, points = points,
      x_scale = x_scale, xlim = xlim)
    p <- .afd_plot_ready_ggplot(dat, type, facet,
      "Candidate fits vs observed claims", x_scale)
    return(.afd_plot_x(p, x_scale, xlim, x_breaks,
                       x_text_angle, x_text_size, y_text_size))
  }
  dat <- candidate_plot_data(object, scale_range, nsim, seed)
  if (x_scale == "log10" && any(dat$observed_loss <= 0))
    .afd_stop("Log-scale plotting requires positive observed losses.")
  if (!facet) {
    actual <- dat[dat$source == "actual", , drop = FALSE]
    actual <- actual[!duplicated(actual$row), , drop = FALSE]
    modeled <- dat[dat$source == "simulated", , drop = FALSE]
    dat <- rbind(actual, modeled)
    dat$series <- ifelse(dat$source == "actual", "Observed", dat$distribution)
  } else dat$series <- dat$source
  if (type == "histogram") {
    p <- ggplot2::ggplot(dat, ggplot2::aes(x = observed_loss,
        y = ggplot2::after_stat(density), weight = weight, fill = series,
        group = series)) + ggplot2::geom_histogram(bins = bins,
        position = "identity", alpha = .25)
  } else if (type == "density") {
    p <- ggplot2::ggplot(dat, ggplot2::aes(x = observed_loss, weight = weight,
        colour = series, group = interaction(series, simulation))) +
        ggplot2::geom_density(alpha = .35)
  } else {
    if (!facet) {
      # .afd_empirical_curve groups only by source/simulation; retain models.
      curves <- do.call(rbind, lapply(split(dat, dat$series), function(a) {
        cr <- .afd_empirical_curve(a, type == "survival")
        cr$series <- a$series[1]
        cr
      }))
    } else {
      curves <- do.call(rbind, lapply(split(dat, dat$distribution), function(a) {
        cr <- .afd_empirical_curve(a, type == "survival")
        cr$distribution <- a$distribution[1]
        cr$series <- cr$source
        cr
      }))
    }
    p <- ggplot2::ggplot(curves, ggplot2::aes(x = x, y = y, colour = series,
      group = interaction(series, simulation))) + ggplot2::geom_line(alpha = .5)
  }
  if (facet) p <- p + ggplot2::facet_wrap(~distribution, scales = "fixed")
  p <- p + ggplot2::labs(x = "Observed severity", y = if (type %in% c("density", "histogram"))
    if (x_scale == "log10") "Density per log10 severity" else "Density" else type,
    colour = NULL, fill = NULL,
    title = "Candidate fits vs observed claims") + ggplot2::theme_minimal()
  .afd_plot_x(p, x_scale, xlim, x_breaks,
              x_text_angle, x_text_size, y_text_size)
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
#' @param output `"wide"` (default) returns summary columns; `"plot_ready"`
#'   returns the exact long data used by `plot_scaling_fit()`.
#' @return A data frame with scale bins. For `output = "wide"`,
#'     columns include `bin`, `scale_value`, `n`, `actual_observed_mean`,
#'     `modeled_observed_mean`, and `fitted_ground_up_mean`. For
#'     `output = "plot_ready"`, columns `bin`, `scale_value`, `mean`, and `series`
#'     give the exact long-format data used by `plot_scaling_fit()`. Observed and
#'     modeled observed means incorporate policy terms; the ground-up mean does not.
#' @examples
#' set.seed(2)
#' d <- data.frame(cov_a = rep(c(200000, 400000, 600000), each = 80))
#' d$loss <- rlnorm(nrow(d), log(.07*d$cov_a), .8)
#' f <- fit_severity(d, "loss", scale_by = "cov_a", hessian = FALSE)
#' scaling_plot_data(f, bins = 3, nsim = 1)
#' @export
scaling_plot_data <- function(object, bins = 10, bin_method = c("quantile", "width"),
                              scale_range = NULL, nsim = 50, seed = NULL,
                              output = c("wide", "plot_ready")) {
  output <- match.arg(output)
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
  summary_data <- do.call(rbind, out)
  if (output == "wide") return(summary_data)
  rbind(
    data.frame(bin = summary_data$bin, scale_value = summary_data$scale_value,
      mean = summary_data$actual_observed_mean, series = "Actual observed mean"),
    data.frame(bin = summary_data$bin, scale_value = summary_data$scale_value,
      mean = summary_data$modeled_observed_mean, series = "Modeled observed mean"),
    data.frame(bin = summary_data$bin, scale_value = summary_data$scale_value,
      mean = summary_data$fitted_ground_up_mean, series = "Fitted ground-up mean"))
}

#' Plot the fitted scaling relationship
#'
#' Compares empirical observed mean severity by scale bin with simulated
#' observed means and the fitted ground-up mean relationship.
#'
#' @inheritParams scaling_plot_data
#' @param x_breaks Optional numeric x tick positions.
#' @param x_text_angle Rotation of the scale-axis labels in degrees.
#' @param x_text_size Size of the scale-axis tick labels.
#' @param y_text_size Size of the mean-severity-axis tick labels.
#' @return A `ggplot2` object showing observed means, simulated
#'     observed means, and fitted ground-up means by scaling-variable bin. The
#'     plotted data can be obtained with `scaling_plot_data(output =
#'     "plot_ready")`.
#' @examples
#' set.seed(2)
#' d <- data.frame(cov_a = rep(c(200000, 400000, 600000), each = 80))
#' d$loss <- rlnorm(nrow(d), log(.07*d$cov_a), .8)
#' f <- fit_severity(d, "loss", scale_by = "cov_a", hessian = FALSE)
#' plot_scaling_fit(f, bins = 3, nsim = 1)
#' @export
plot_scaling_fit <- function(object, bins = 10, bin_method = c("quantile", "width"),
                             scale_range = NULL, nsim = 50, seed = NULL,
                             x_breaks = NULL, x_text_angle = 0,
                             x_text_size = 9, y_text_size = 9) {
  long <- scaling_plot_data(object, bins, bin_method, scale_range, nsim, seed,
                             output = "plot_ready")
  p <- ggplot2::ggplot(long,
      ggplot2::aes(x = scale_value, y = mean, colour = series, group = series)) +
    ggplot2::geom_line() + ggplot2::geom_point() +
    ggplot2::labs(x = object$columns$scale_by, y = "Mean severity", colour = NULL,
                  title = "Severity scaling diagnostic") + ggplot2::theme_minimal()
  if (!is.null(x_breaks)) p <- p + ggplot2::scale_x_continuous(breaks = x_breaks)
  .afd_plot_style(p, x_text_angle, x_text_size, y_text_size)
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
#' @return A data frame with one row for actual and one for simulated
#'     observed claims. Columns include `source`, `rows`, `layer_count`,
#'     `layer_proportion`, `layer_mean`, and `censoring_rate`. Simulated counts are
#'     averaged over replicates; results reflect observed (possibly censored)
#'     severity, not uncensored ground-up losses.
#' @examples
#' set.seed(123)
#' d <- data.frame(loss = rlnorm(120, log(10000), 0.8))
#' fit <- fit_severity(d, "loss", distribution = "lognormal", hessian = FALSE)
#' diagnose_fit(fit, lower = 1000, upper = 30000, nsim = 1)
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
