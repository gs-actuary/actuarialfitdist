# Distribution definitions ---------------------------------------------------

.afd_supported_distributions <- function() {
  c("weibull", "gamma", "lognormal", "pareto2", "burr", "normal",
    "exponential", "loglogistic", "invgauss", "gpd")
}

.afd_normalize_distribution <- function(x) {
  if (length(x) != 1L || !is.character(x)) .afd_stop("`distribution` must be a single character value.")
  key <- tolower(trimws(x))
  aliases <- c(
    "pareto" = "pareto2", "paretoii" = "pareto2", "pareto ii" = "pareto2",
    "lomax" = "pareto2", "log-logistic" = "loglogistic",
    "inversegaussian" = "invgauss", "inverse gaussian" = "invgauss",
    "inverse-gaussian" = "invgauss", "generalizedpareto" = "gpd",
    "generalized pareto" = "gpd", "generalized-pareto" = "gpd",
    "exp" = "exponential"
  )
  if (key %in% names(aliases)) key <- unname(aliases[key])
  if (!key %in% .afd_supported_distributions()) {
    .afd_stop("Unsupported distribution `", x, "`. Supported families are: ",
              paste(.afd_supported_distributions(), collapse = ", "), ".")
  }
  key
}

.afd_distribution_note <- function(dist) {
  dist <- .afd_normalize_distribution(dist)
  switch(dist,
    lognormal = "The internal scale is exp(meanlog). With scale_by, mean severity is proportional to scale_coef * scale_by + scale_intercept.",
    weibull = "The internal scale is the Weibull scale parameter; with common shape, mean severity is proportional to this scale.",
    gamma = "The internal scale is the Gamma scale parameter; with common shape, mean severity is proportional to this scale.",
    normal = "The internal scale is the Normal mean and sd = cv * mean. The fitted claim model is interpreted conditional on positive severity.",
    invgauss = "The internal scale is the inverse-Gaussian mean; lambda = shape * mean, producing constant relative shape under scaling.",
    gpd = "The internal scale is the generalized Pareto scale. The unrestricted mean exists only when xi < 1.",
    pareto2 = "Pareto II (Lomax) is used. The unrestricted mean exists only when shape > 1.",
    burr = "Burr XII is used. Its unrestricted mean exists only when shape2 > 1 / shape1.",
    loglogistic = "The unrestricted mean exists only when shape > 1.",
    exponential = "The scale parameter is the mean severity.",
    ""
  )
}

.afd_make_param_spec <- function(dist, x, scale_by = NULL, scale_intercept = FALSE) {
  dist <- .afd_normalize_distribution(dist)
  x_pos <- x[is.finite(x) & x > 0]
  typical <- if (length(x_pos)) stats::median(x_pos) else 1
  typical <- max(typical, 1e-8)

  if (is.null(scale_by)) {
    spec <- list(scale = list(start = typical, transform = "positive"))
  } else {
    z <- scale_by[is.finite(scale_by)]
    ztyp <- if (length(z)) stats::median(z) else 1
    if (!is.finite(ztyp) || abs(ztyp) < 1e-8) {
      denom <- if (length(z)) max(abs(z), na.rm = TRUE) else 1
      if (!is.finite(denom) || denom < 1e-8) denom <- 1
      ztyp <- denom
    }
    coef0 <- typical / abs(ztyp)
    spec <- list(scale_coef = list(start = max(coef0, 1e-8), transform = "positive"))
    if (isTRUE(scale_intercept)) {
      spec$scale_intercept <- list(start = 0, transform = "real")
    }
  }

  extras <- switch(dist,
    weibull = list(shape = list(start = 1.5, transform = "positive")),
    gamma = list(shape = list(start = 2, transform = "positive")),
    lognormal = list(sdlog = list(start = 1, transform = "positive")),
    pareto2 = list(shape = list(start = 2.5, transform = "positive")),
    burr = list(shape1 = list(start = 1.5, transform = "positive"),
                shape2 = list(start = 2.5, transform = "positive")),
    normal = list(cv = list(start = 0.6, transform = "positive")),
    exponential = list(),
    loglogistic = list(shape = list(start = 2.5, transform = "positive")),
    invgauss = list(shape = list(start = 4, transform = "positive")),
    gpd = list(xi = list(start = 0.2, transform = "real"))
  )
  c(spec, extras)
}

.afd_pack_parameters <- function(spec, fixed = list()) {
  if (is.null(fixed)) fixed <- list()
  if (is.atomic(fixed) && length(fixed)) fixed <- as.list(fixed)
  if (!is.list(fixed)) .afd_stop("`fixed` must be a named list or named vector.")
  if (length(fixed) && (is.null(names(fixed)) || any(!nzchar(names(fixed))))) {
    .afd_stop("Every fixed parameter must be named.")
  }
  bad <- setdiff(names(fixed), names(spec))
  if (length(bad)) .afd_stop("Unknown fixed parameter(s): ", paste(bad, collapse = ", "), ".")

  for (nm in names(fixed)) {
    val <- as.numeric(fixed[[nm]])
    if (length(val) != 1L || !is.finite(val)) .afd_stop("Fixed parameter `", nm, "` must be one finite number.")
    tr <- spec[[nm]]$transform
    if (tr == "positive" && val <= 0) .afd_stop("Fixed parameter `", nm, "` must be positive.")
    if (tr == "prob" && (val <= 0 || val >= 1)) .afd_stop("Fixed probability `", nm, "` must lie strictly between 0 and 1.")
    fixed[[nm]] <- val
  }

  unknown <- setdiff(names(spec), names(fixed))
  encode <- function(value, transform) {
    if (transform == "positive") log(value)
    else if (transform == "prob") stats::qlogis(value)
    else value
  }
  decode_one <- function(value, transform) {
    if (transform == "positive") exp(value)
    else if (transform == "prob") stats::plogis(value)
    else value
  }
  start <- vapply(unknown, function(nm) encode(spec[[nm]]$start, spec[[nm]]$transform), numeric(1))
  names(start) <- unknown
  decode <- function(opt) {
    opt <- as.numeric(opt); names(opt) <- unknown
    out <- vector("list", length(spec)); names(out) <- names(spec)
    for (nm in names(spec)) {
      if (nm %in% names(fixed)) out[[nm]] <- fixed[[nm]]
      else out[[nm]] <- decode_one(opt[[nm]], spec[[nm]]$transform)
    }
    out
  }
  list(start = start, decode = decode, unknown = unknown, fixed = fixed)
}

.afd_scale_value <- function(params, scale_by) {
  b <- params$scale_intercept %||% 0
  params$scale_coef * scale_by + b
}

# Validate natural-scale distribution parameters before calling base-R density
# or probability functions. During numerical optimization, BFGS may try very
# remote parameter values even when the final fit is well behaved. Invalid
# trial points are part of normal optimizer behavior and are treated as having
# zero likelihood rather than being allowed to emit user-visible warnings.
.afd_valid_distribution_params <- function(dist, params) {
  dist <- .afd_normalize_distribution(dist)
  vals <- unlist(params, use.names = FALSE)
  if (length(vals) && any(!is.finite(vals))) return(FALSE)

  switch(dist,
    weibull = is.finite(params$shape) && params$shape > 0,
    gamma = is.finite(params$shape) && params$shape > 0,
    lognormal = is.finite(params$sdlog) && params$sdlog > 0,
    pareto2 = is.finite(params$shape) && params$shape > 0,
    burr = is.finite(params$shape1) && params$shape1 > 0 &&
      is.finite(params$shape2) && params$shape2 > 0,
    normal = is.finite(params$cv) && params$cv > 0,
    exponential = TRUE,
    loglogistic = is.finite(params$shape) && params$shape > 0,
    invgauss = is.finite(params$shape) && params$shape > 0,
    gpd = is.finite(params$xi),
    FALSE
  )
}

.afd_d <- function(dist, x, scale, params, log = FALSE) {
  dist <- .afd_normalize_distribution(dist)
  n <- max(length(x), length(scale))
  if (!.afd_valid_distribution_params(dist, params)) {
    return(rep(if (log) -Inf else 0, n))
  }
  x <- rep(x, length.out = n); scale <- rep(scale, length.out = n)
  out_log <- rep(-Inf, n)
  valid_scale <- is.finite(scale) & scale > 0
  if (any(valid_scale)) {
    xx <- x[valid_scale]; ss <- scale[valid_scale]
    val <- suppressWarnings(switch(dist,
      weibull = stats::dweibull(xx, shape = params$shape, scale = ss, log = TRUE),
      gamma = stats::dgamma(xx, shape = params$shape, scale = ss, log = TRUE),
      lognormal = stats::dlnorm(xx, meanlog = log(ss), sdlog = params$sdlog, log = TRUE),
      pareto2 = ifelse(xx >= 0,
                       log(params$shape) - log(ss) - (params$shape + 1) * log1p(xx / ss), -Inf),
      burr = {
        y <- xx / ss
        ifelse(xx > 0,
               log(params$shape1) + log(params$shape2) - log(ss) +
                 (params$shape1 - 1) * log(y) -
                 (params$shape2 + 1) * log1p(y^params$shape1), -Inf)
      },
      normal = stats::dnorm(xx, mean = ss, sd = params$cv * ss, log = TRUE),
      exponential = ifelse(xx >= 0, -log(ss) - xx / ss, -Inf),
      loglogistic = {
        y <- xx / ss
        ifelse(xx > 0,
               log(params$shape) - log(ss) + (params$shape - 1) * log(y) -
                 2 * log1p(y^params$shape), -Inf)
      },
      invgauss = {
        mu <- ss; lambda <- params$shape * ss
        ifelse(xx > 0,
               0.5 * (log(lambda) - log(2 * pi) - 3 * log(xx)) -
                 lambda * (xx - mu)^2 / (2 * mu^2 * xx), -Inf)
      },
      gpd = {
        xi <- params$xi
        if (abs(xi) < 1e-8) {
          ifelse(xx >= 0, -log(ss) - xx / ss, -Inf)
        } else {
          z <- 1 + xi * xx / ss
          ifelse(xx >= 0 & z > 0, -log(ss) - (1 / xi + 1) * log(z), -Inf)
        }
      }
    ))
    out_log[valid_scale] <- val
  }
  if (log) out_log else exp(out_log)
}

.afd_invgauss_cdf <- function(q, scale, shape) {
  n <- max(length(q), length(scale)); q <- rep(q, length.out = n); scale <- rep(scale, length.out = n)
  ans <- rep(0, n); pos <- q > 0 & is.finite(scale) & scale > 0
  if (any(pos)) {
    mu <- scale[pos]; lambda <- shape * scale[pos]; x <- q[pos]
    root <- sqrt(lambda / x)
    a <- root * (x / mu - 1)
    b <- -root * (x / mu + 1)
    first <- stats::pnorm(a)
    logsecond <- 2 * lambda / mu + stats::pnorm(b, log.p = TRUE)
    second <- exp(pmin(logsecond, 0))
    ans[pos] <- pmin(1, pmax(0, first + second))
  }
  ans
}

.afd_p <- function(dist, q, scale, params, lower.tail = TRUE, log.p = FALSE) {
  dist <- .afd_normalize_distribution(dist)
  n <- max(length(q), length(scale))
  q <- rep(q, length.out = n); scale <- rep(scale, length.out = n)
  if (!.afd_valid_distribution_params(dist, params)) return(rep(NA_real_, n))
  if (any(!is.finite(scale) | scale <= 0)) return(rep(NA_real_, n))

  # Use native lower-tail/log-probability support when available. These calls
  # are materially more stable for censored observations deep in the tail.
  if (dist == "weibull") return(suppressWarnings(stats::pweibull(q, shape = params$shape, scale = scale, lower.tail = lower.tail, log.p = log.p)))
  if (dist == "gamma") return(suppressWarnings(stats::pgamma(q, shape = params$shape, scale = scale, lower.tail = lower.tail, log.p = log.p)))
  if (dist == "lognormal") return(suppressWarnings(stats::plnorm(q, meanlog = log(scale), sdlog = params$sdlog, lower.tail = lower.tail, log.p = log.p)))
  if (dist == "normal") return(suppressWarnings(stats::pnorm(q, mean = scale, sd = params$cv * scale, lower.tail = lower.tail, log.p = log.p)))

  log_surv <- suppressWarnings(switch(dist,
    pareto2 = ifelse(q < 0, 0, -params$shape * log1p(q / scale)),
    burr = ifelse(q <= 0, 0, -params$shape2 * log1p((q / scale)^params$shape1)),
    exponential = ifelse(q < 0, 0, -q / scale),
    loglogistic = ifelse(q <= 0, 0, -log1p((q / scale)^params$shape)),
    invgauss = {
      cdf <- .afd_invgauss_cdf(q, scale, params$shape)
      log1p(-pmin(cdf, 1))
    },
    gpd = {
      xi <- params$xi
      if (abs(xi) < 1e-8) {
        ifelse(q < 0, 0, -q / scale)
      } else {
        z <- 1 + xi * q / scale
        ifelse(q < 0, 0, ifelse(z > 0, -(1 / xi) * log(z), -Inf))
      }
    }
  ))
  if (!lower.tail) {
    if (log.p) return(log_surv)
    return(exp(log_surv))
  }
  if (log.p) return(.afd_log1mexp(log_surv))
  -expm1(log_surv)
}

.afd_q <- function(dist, p, scale, params) {
  dist <- .afd_normalize_distribution(dist)
  n <- max(length(p), length(scale))
  p <- rep(p, length.out = n); scale <- rep(scale, length.out = n)
  if (any(p < 0 | p > 1, na.rm = TRUE)) .afd_stop("Probabilities must lie in [0, 1].")
  switch(dist,
    weibull = stats::qweibull(p, shape = params$shape, scale = scale),
    gamma = stats::qgamma(p, shape = params$shape, scale = scale),
    lognormal = stats::qlnorm(p, meanlog = log(scale), sdlog = params$sdlog),
    pareto2 = scale * ((1 - p)^(-1 / params$shape) - 1),
    burr = scale * ((1 - p)^(-1 / params$shape2) - 1)^(1 / params$shape1),
    normal = stats::qnorm(p, mean = scale, sd = params$cv * scale),
    exponential = -scale * log1p(-p),
    loglogistic = scale * (p / (1 - p))^(1 / params$shape),
    invgauss = vapply(seq_len(n), function(i) {
      if (p[i] <= 0) return(0)
      if (p[i] >= 1) return(Inf)
      .afd_numeric_quantile(p[i], function(x) .afd_p("invgauss", x, scale[i], params),
                            lower = 0, upper_start = max(scale[i], 1))
    }, numeric(1)),
    gpd = {
      xi <- params$xi
      if (abs(xi) < 1e-8) -scale * log1p(-p)
      else scale / xi * ((1 - p)^(-xi) - 1)
    }
  )
}

.afd_r <- function(dist, n, scale, params) {
  dist <- .afd_normalize_distribution(dist)
  scale <- rep(scale, length.out = n)
  switch(dist,
    weibull = stats::rweibull(n, shape = params$shape, scale = scale),
    gamma = stats::rgamma(n, shape = params$shape, scale = scale),
    lognormal = stats::rlnorm(n, meanlog = log(scale), sdlog = params$sdlog),
    pareto2 = scale * ((1 - stats::runif(n))^(-1 / params$shape) - 1),
    burr = scale * ((1 - stats::runif(n))^(-1 / params$shape2) - 1)^(1 / params$shape1),
    normal = stats::rnorm(n, mean = scale, sd = params$cv * scale),
    exponential = -scale * log(stats::runif(n)),
    loglogistic = {
      u <- stats::runif(n); scale * (u / (1 - u))^(1 / params$shape)
    },
    invgauss = {
      mu <- scale; lambda <- params$shape * scale
      v <- stats::rnorm(n)^2
      x <- mu + mu^2 * v / (2 * lambda) -
        mu / (2 * lambda) * sqrt(4 * mu * lambda * v + mu^2 * v^2)
      u <- stats::runif(n)
      ifelse(u <= mu / (mu + x), x, mu^2 / x)
    },
    gpd = {
      u <- stats::runif(n); xi <- params$xi
      if (abs(xi) < 1e-8) -scale * log1p(-u)
      else scale / xi * ((1 - u)^(-xi) - 1)
    }
  )
}

.afd_mean <- function(dist, scale, params) {
  dist <- .afd_normalize_distribution(dist)
  switch(dist,
    weibull = scale * gamma(1 + 1 / params$shape),
    gamma = scale * params$shape,
    lognormal = scale * exp(params$sdlog^2 / 2),
    pareto2 = if (params$shape > 1) scale / (params$shape - 1) else rep(Inf, length(scale)),
    burr = if (params$shape2 > 1 / params$shape1)
      scale * params$shape2 * beta(1 + 1 / params$shape1,
                                   params$shape2 - 1 / params$shape1) else rep(Inf, length(scale)),
    normal = scale,
    exponential = scale,
    loglogistic = if (params$shape > 1)
      scale * pi / params$shape / sin(pi / params$shape) else rep(Inf, length(scale)),
    invgauss = scale,
    gpd = if (params$xi < 1) scale / (1 - params$xi) else rep(Inf, length(scale))
  )
}
