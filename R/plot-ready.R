# Public plotting-data builders use these helpers. No random sampling is
# required to calculate expected observed-claim bin probabilities: for claim i,
# Y_i = min(X_i, u_i) conditional on X_i > d_i. Thus the conditional CDF of Y_i
# is zero below d_i, one at/above u_i, and (F(t)-F(d_i))/S(d_i) otherwise.
# This explicitly includes probability atoms caused by finite censoring limits.

.afd_plot_breaks <- function(dat, cols, x_scale, xlim, bins, points, type) {
  x <- pmin(dat[[cols$loss]], dat[[cols$limit]])
  if (x_scale == "log10" && any(x <= 0)) {
    .afd_stop("Log10 plotting requires strictly positive observed losses; choose `x_scale = 'linear'`.")
  }
  bounds <- if (is.null(xlim)) range(x) else xlim
  if (length(bounds) != 2L || any(!is.finite(bounds)) || bounds[1] >= bounds[2] ||
      (x_scale == "log10" && bounds[1] <= 0)) {
    .afd_stop("Plot range must have two increasing finite values, positive for `log10`.")
  }
  nb <- if (type == "density") max(120L, as.integer(points)) else
    if (type == "histogram") as.integer(bins) else max(80L, as.integer(points))
  if (is.na(nb) || nb < 5L || nb > 2000L) .afd_stop("`bins` and `points` must be within 5 and 2000.")
  if (x_scale == "log10") {
    10^seq(log10(bounds[1]), log10(bounds[2]), length.out = nb + 1L)
  } else {
    seq(bounds[1], bounds[2], length.out = nb + 1L)
  }
}

.afd_actual_cdf_grid <- function(x, w, grid) {
  ord <- order(x)
  x <- x[ord]; w <- w[ord]
  cumul <- cumsum(w) / sum(w)
  j <- findInterval(grid, x)
  ans <- numeric(length(grid))
  valid <- j > 0L
  ans[valid] <- cumul[j[valid]]
  ans
}

.afd_expected_cdf_grid <- function(fit, dat, grid) {
  cols <- fit$columns
  d <- dat[[cols$deductible]]
  u <- dat[[cols$limit]]
  z <- if (is.null(cols$scale_by)) rep(NA_real_, nrow(dat)) else dat[[cols$scale_by]]
  w <- if (is.null(cols$weights)) rep(1, nrow(dat)) else dat[[cols$weights]]
  key <- paste(sprintf("%.17g", d), sprintf("%.17g", u),
               sprintf("%.17g", z), sep = "\r")
  unique_key <- unique(key)
  group_id <- match(key, unique_key)
  first <- match(unique_key, key)
  ws <- as.numeric(rowsum(w, group = group_id, reorder = TRUE)[, 1L])
  # rowsum() orders numeric group labels, matching `first` order.
  d <- d[first]; u <- u[first]; z <- z[first]
  positive <- ws > 0
  d <- d[positive]; u <- u[positive]; z <- z[positive]; ws <- ws[positive]
  scale_arg <- if (is.null(cols$scale_by)) NULL else z
  Fd <- .afd_fit_cdf(fit, d, scale_arg)
  surv <- 1 - Fd
  if (any(!is.finite(surv) | surv <= 0)) {
    .afd_stop("A fitted model gives zero/invalid survival above a selected deductible; cannot construct fitted plot probabilities.")
  }
  out <- numeric(length(grid))
  for (j in seq_along(grid)) {
    x <- grid[j]
    val <- numeric(length(d))
    upper <- x >= u
    interior <- !upper & x > d
    val[upper] <- 1
    if (any(interior)) {
      zz <- if (is.null(scale_arg)) NULL else z[interior]
      Fx <- .afd_fit_cdf(fit, rep(x, sum(interior)), zz)
      val[interior] <- pmin(1, pmax(0, (Fx - Fd[interior]) / surv[interior]))
    }
    out[j] <- sum(ws * val) / sum(ws)
  }
  pmax(0, pmin(1, out))
}

.afd_plot_ready <- function(fit, scale_range = NULL, type = "density",
                            bins = 40L, points = 160L,
                            x_scale = "log10", xlim = NULL, breaks = NULL) {
  if (!inherits(fit, "actuarialfitdist_fit")) .afd_stop("`object` must be a fitted model.")
  type <- match.arg(type, c("histogram", "density", "cdf", "survival"))
  x_scale <- match.arg(x_scale, c("log10", "linear"))
  keep <- .afd_filter_scale_range(fit, fit$data, scale_range)
  dat <- fit$data[keep, , drop = FALSE]
  if (!nrow(dat)) .afd_stop("No observations remain after applying `scale_range`.")
  cols <- fit$columns
  observed <- pmin(dat[[cols$loss]], dat[[cols$limit]])
  w <- if (is.null(cols$weights)) rep(1, nrow(dat)) else dat[[cols$weights]]
  if (sum(w) <= 0) .afd_stop("Selected rows have zero total likelihood weight.")
  if (is.null(breaks)) breaks <- .afd_plot_breaks(dat, cols, x_scale, xlim, bins, points, type)
  model_cdf <- .afd_expected_cdf_grid(fit, dat, breaks)
  if (type %in% c("cdf", "survival")) {
    actual_cdf <- .afd_actual_cdf_grid(observed, w, breaks)
    if (type == "survival") {
      model_cdf <- 1 - model_cdf
      actual_cdf <- 1 - actual_cdf
    }
    return(rbind(
      data.frame(source = "actual", x = breaks, value = actual_cdf,
                 lower = NA_real_, upper = NA_real_),
      data.frame(source = "modeled", x = breaks, value = model_cdf,
                 lower = NA_real_, upper = NA_real_)))
  }
  idx <- findInterval(observed, breaks, rightmost.closed = TRUE)
  nb <- length(breaks) - 1L
  valid <- idx >= 1L & idx <= nb
  count <- numeric(nb)
  if (any(valid)) {
    counted <- rowsum(w[valid], group = idx[valid], reorder = TRUE)
    count[as.integer(rownames(counted))] <- counted[, 1L]
  }
  modeled_mass <- pmax(0, diff(model_cdf))
  actual_mass <- count / sum(w)
  width <- if (x_scale == "log10") diff(log10(breaks)) else diff(breaks)
  center <- if (x_scale == "log10") {
    sqrt(utils::head(breaks, -1) * utils::tail(breaks, -1))
  } else {
    (utils::head(breaks, -1) + utils::tail(breaks, -1)) / 2
  }
  rbind(
    data.frame(source = "actual", x = center, value = actual_mass / width,
               lower = utils::head(breaks, -1L), upper = utils::tail(breaks, -1L)),
    data.frame(source = "modeled", x = center, value = modeled_mass / width,
               lower = utils::head(breaks, -1L), upper = utils::tail(breaks, -1L)))
}
