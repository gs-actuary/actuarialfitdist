# Internal utilities -------------------------------------------------------

.afd_stop <- function(...) stop(..., call. = FALSE)
.afd_warn <- function(...) warning(..., call. = FALSE)

.afd_match_col <- function(data, col, arg, required = TRUE) {
  if (is.null(col)) {
    if (required) .afd_stop("`", arg, "` must identify a column in `data`.")
    return(NULL)
  }
  if (length(col) != 1L || !is.character(col)) {
    .afd_stop("`", arg, "` must be a single column name supplied as a character string.")
  }
  if (!col %in% names(data)) {
    .afd_stop("Column `", col, "` supplied to `", arg, "` was not found in `data`.")
  }
  col
}

.afd_validate_ground_up <- function(loss, deductible, limit, weights, scale_by = NULL) {
  n <- length(loss)
  if (!is.numeric(loss) || length(loss) != n || any(!is.finite(loss))) {
    .afd_stop("The ground-up loss column must contain finite numeric values.")
  }
  if (any(loss <= 0)) {
    .afd_stop("Ground-up losses must be strictly positive. The fitter expects ground-up claim severities, not payments or losses net of deductible.")
  }
  if (!is.numeric(deductible) || length(deductible) != n || any(!is.finite(deductible)) || any(deductible < 0)) {
    .afd_stop("Deductibles must be finite nonnegative numeric values.")
  }
  if (!is.numeric(limit) || length(limit) != n || any(is.na(limit)) || any(limit <= 0)) {
    .afd_stop("Limits must be positive numeric values; `Inf` is allowed.")
  }
  if (any(limit <= deductible)) {
    .afd_stop("Every limit must exceed its deductible under the ground-up-loss observation model.")
  }
  # A claim is observed only if it exceeds its deductible. Equality is not an observed positive claim.
  if (any(loss <= deductible)) {
    .afd_stop(
      "At least one reported ground-up loss is less than or equal to its deductible. ",
      "The core fitter assumes ground-up losses with claims below the deductible absent. ",
      "Do not supply insurer payments, excess-of-deductible losses, or depreciated payments as `loss`."
    )
  }
  if (!is.numeric(weights) || length(weights) != n || any(!is.finite(weights)) || any(weights < 0)) {
    .afd_stop("Likelihood weights must be finite nonnegative numeric values.")
  }
  if (!is.null(scale_by)) {
    if (!is.numeric(scale_by) || length(scale_by) != n || any(!is.finite(scale_by))) {
      .afd_stop("The scaling column must contain finite numeric values.")
    }
  }
  invisible(TRUE)
}

.afd_recycle <- function(x, n, arg) {
  if (length(x) == 1L) return(rep(x, n))
  if (length(x) != n) .afd_stop("`", arg, "` must have length 1 or match the requested output length.")
  x
}

.afd_log1mexp <- function(logp) {
  # log(1 - exp(logp)) for logp <= 0
  out <- numeric(length(logp))
  idx <- logp < log(0.5)
  out[idx] <- log1p(-exp(logp[idx]))
  out[!idx] <- log(-expm1(logp[!idx]))
  out
}

.afd_clip_prob <- function(p) pmin(pmax(p, 1e-12), 1 - 1e-12)

.afd_num_hessian <- function(fn, par, rel_step = 1e-4) {
  p <- length(par)
  if (p == 0L) return(matrix(numeric(), 0, 0))
  h <- rel_step * pmax(abs(par), 1)
  H <- matrix(NA_real_, p, p)
  f0 <- fn(par)
  for (i in seq_len(p)) {
    ei <- rep(0, p); ei[i] <- h[i]
    fp <- fn(par + ei)
    fm <- fn(par - ei)
    H[i, i] <- (fp - 2 * f0 + fm) / (h[i]^2)
    if (i < p) {
      for (j in (i + 1L):p) {
        ej <- rep(0, p); ej[j] <- h[j]
        fpp <- fn(par + ei + ej)
        fpm <- fn(par + ei - ej)
        fmp <- fn(par - ei + ej)
        fmm <- fn(par - ei - ej)
        H[i, j] <- H[j, i] <- (fpp - fpm - fmp + fmm) / (4 * h[i] * h[j])
      }
    }
  }
  H
}

.afd_safe_inverse <- function(M) {
  if (!length(M)) return(M)
  out <- tryCatch(solve(M), error = function(e) NULL)
  if (is.null(out) || any(!is.finite(out))) return(NULL)
  out
}

.afd_numeric_quantile <- function(p, cdf_fun, lower = 0, upper_start = 1) {
  vapply(p, function(pp) {
    if (pp <= 0) return(lower)
    if (pp >= 1) return(Inf)
    upper <- upper_start
    f_upper <- cdf_fun(upper)
    iter <- 0L
    while ((!is.finite(f_upper) || f_upper < pp) && iter < 80L) {
      upper <- upper * 2 + 1
      f_upper <- cdf_fun(upper)
      iter <- iter + 1L
    }
    if (!is.finite(f_upper) || f_upper < pp) return(Inf)
    stats::uniroot(function(x) cdf_fun(x) - pp, lower = lower, upper = upper, tol = 1e-9)$root
  }, numeric(1))
}

.afd_param_count <- function(fit) length(fit$optim_par)

.afd_n_eff <- function(weights) sum(weights)

# Prepare one consistent observation model. Original positional row indices are
# returned for audit. Optional policy terms use internal columns, never asking
# the analyst to manufacture zero or infinity fields.
.afd_prepare <- function(data, loss, deductible = NULL, limit = NULL,
                         weights = NULL, scale_by = NULL,
                         invalid_rows = c("warn_drop", "error")) {
  invalid_rows <- match.arg(invalid_rows)
  if (!is.data.frame(data)) .afd_stop("`data` must be a data frame.")
  lc <- .afd_match_col(data, loss, "loss")
  dc <- .afd_match_col(data, deductible, "deductible", required = FALSE)
  uc <- .afd_match_col(data, limit, "limit", required = FALSE)
  wc <- .afd_match_col(data, weights, "weights", required = FALSE)
  sc <- .afd_match_col(data, scale_by, "scale_by", required = FALSE)
  unique_col <- function(prefix) {
    ans <- prefix
    while (ans %in% names(data)) ans <- paste0(ans, "_")
    ans
  }
  if (is.null(dc)) { dc <- unique_col(".afd_deductible"); data[[dc]] <- 0 }
  if (is.null(uc)) { uc <- unique_col(".afd_limit"); data[[uc]] <- Inf }
  x <- data[[lc]]; d <- data[[dc]]; u <- data[[uc]]
  w <- if (is.null(wc)) rep(1, nrow(data)) else data[[wc]]
  z <- if (is.null(sc)) NULL else data[[sc]]
  fields <- list(loss = x, deductible = d, limit = u, weights = w, scale_by = z)
  for (nm in names(fields)) {
    v <- fields[[nm]]
    if (!is.null(v) && !is.numeric(v)) .afd_stop("`", nm, "` column must be numeric.")
  }
  n <- nrow(data)
  reason <- rep("", n)
  add <- function(mask, description) {
    mask[is.na(mask)] <- TRUE
    reason[mask] <<- ifelse(nzchar(reason[mask]),
                            paste(reason[mask], description, sep = "; "), description)
  }
  add(!is.finite(x) | x <= 0, "loss not finite/positive")
  add(!is.finite(d) | d < 0, "invalid deductible")
  add(is.na(u) | u <= 0, "invalid limit")
  add(d >= u, "limit not above deductible")
  add(x <= d, "loss at/below deductible")
  add(is.finite(u) & x > u, "loss above censoring limit")
  add(!is.finite(w) | w < 0, "invalid weight")
  if (!is.null(z)) add(!is.finite(z), "invalid scaling value")
  excluded <- which(nzchar(reason))
  if (length(excluded)) {
    detail <- paste0(length(excluded), " anomalous row(s) excluded; original positions: ",
                     paste(utils::head(excluded, 12L), collapse = ", "),
                     if (length(excluded) > 12L) ", ..." else "", ".")
    if (invalid_rows == "error") .afd_stop(detail, " First reason: ", reason[excluded[1L]])
    .afd_warn(detail, " See `$excluded_rows` and `$excluded_details`.")
  }
  retained <- which(!nzchar(reason))
  if (length(retained) < 5L) .afd_stop("Fewer than five valid claims remain after screening.")
  if (!any(w[retained] > 0)) .afd_stop("At least one positive likelihood weight is required.")
  clean <- data[retained, , drop = FALSE]
  rownames(clean) <- NULL
  details <- data.frame(row = excluded, reason = reason[excluded], stringsAsFactors = FALSE)
  list(data = clean, cols = list(loss = lc, deductible = dc, limit = uc,
       weights = wc, scale_by = sc), excluded_rows = excluded,
       excluded_details = details, retained_rows = retained)
}
