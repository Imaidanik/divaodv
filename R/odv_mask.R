# =============================================================================
# odv_mask.R
# ODV-style masking of poorly-constrained grid nodes.
#
# Two methods:
#   "support" - pure geometry, no DIVAnd. Masks nodes whose nearest observation,
#               measured in the anisotropic correlation metric, is further than
#               `mask_threshold` correlation lengths.
#   "cpme"    - DIVAnd's clever poor man's error estimate. One extra DIVAndrun.
#
# Threshold semantics and the cpme calibration are documented in README.md.
# =============================================================================

#' @importFrom JuliaCall julia_eval julia_assign
NULL

# Per-method default thresholds.
#   support: correlation lengths (dimensionless multiplier on len)
#   cpme:    dimensionless error in [0, 1]
.odv_mask_defaults <- list(support = 2, cpme = 0.8)

# The support metric is computed exactly out to `.odv_mask_search_factor` times
# the threshold, so callers can re-threshold upward (up to that factor) using the
# returned `mask_metric` column without re-running. Beyond it the metric is Inf.
.odv_mask_search_factor <- 2


# -----------------------------------------------------------------------------
#' Resolve and validate masking arguments (internal)
#'
#' @param mask Character. One of \code{"none"}, \code{"support"}, \code{"cpme"}.
#' @param mask_threshold Numeric or NULL. NULL takes the per-method default.
#' @return List with \code{method} and \code{threshold}.
#' @noRd
.odv_mask_resolve <- function(mask = "none",
                             mask_threshold = NULL) {

  valid <- c("none", "support", "cpme")

  if (!is.character(mask) || length(mask) != 1L)
    stop("`mask` must be a single character string.", call. = FALSE)
  if (!mask %in% valid)
    stop("`mask` must be one of: ", paste(valid, collapse = ", "),
         ". Got: '", mask, "'.", call. = FALSE)

  if (mask == "none") {
    if (!is.null(mask_threshold))
      warning("`mask_threshold` is ignored when `mask = \"none\"`.",
              call. = FALSE)
    return(list(method = "none", threshold = NA_real_))
  }

  # Threshold --------------------------------------------------------------
  if (is.null(mask_threshold))
    mask_threshold <- .odv_mask_defaults[[mask]]

  if (!is.numeric(mask_threshold) || length(mask_threshold) != 1L ||
      !is.finite(mask_threshold) || mask_threshold <= 0)
    stop("`mask_threshold` must be a single positive finite number.",
         call. = FALSE)

  if (mask == "cpme" && mask_threshold >= 1)
    stop("For `mask = \"cpme\"` the threshold is a dimensionless error in ",
         "(0, 1); ", mask_threshold, " would mask nothing.", call. = FALSE)

  list(method = mask, threshold = as.numeric(mask_threshold))
}


# -----------------------------------------------------------------------------
#' Nearest-observation distance in the anisotropic correlation metric (internal)
#'
#' For every node of a regular depth x time grid, returns
#' \code{min_i sqrt(((z - z_i)/L_z)^2 + ((t - t_i)/L_t)^2)}, computed exactly
#' within a search radius and \code{Inf} beyond it.
#'
#' Implemented as a bounding-box paint: each observation only touches nodes
#' inside its own search ellipse's bounding box, so cost is
#' O(n_obs * nodes_per_box) rather than O(n_obs * n_grid).
#'
#' @param depth_grid,time_grid Numeric, sorted, regularly spaced grid axes.
#' @param obs_depth,obs_time Numeric observation coordinates (same length).
#' @param depth_corr,time_corr Correlation lengths, in the units of the axes.
#' @param radius Numeric. Search radius in correlation lengths.
#' @param cyclic Logical. Wrap the time axis (annual climatology).
#' @param period Numeric. Length of the time period when \code{cyclic = TRUE}.
#' @return Matrix, \code{length(depth_grid)} x \code{length(time_grid)}.
#' @noRd
.odv_support_rmin <- function(depth_grid, time_grid,
                             obs_depth, obs_time,
                             depth_corr, time_corr,
                             radius,
                             cyclic = FALSE,
                             period = 1) {

  nz <- length(depth_grid)
  nt <- length(time_grid)

  if (length(obs_depth) != length(obs_time))
    stop("`obs_depth` and `obs_time` must have the same length.", call. = FALSE)
  if (depth_corr <= 0 || time_corr <= 0)
    stop("Correlation lengths must be positive.", call. = FALSE)

  rmin <- matrix(Inf, nrow = nz, ncol = nt)
  if (length(obs_depth) == 0L) return(rmin)

  z0 <- depth_grid[1L]
  dz <- if (nz > 1L) (depth_grid[nz] - z0) / (nz - 1L) else 1
  t0 <- time_grid[1L]
  dt <- if (nt > 1L) (time_grid[nt] - t0) / (nt - 1L) else 1

  rz <- radius * depth_corr
  rt <- radius * time_corr

  # Cyclic wrap is handled by painting three shifted copies of each
  # observation. This reproduces min(|dt|, period - |dt|) over the search
  # radius, and the clamping below keeps it correct even when rt exceeds
  # period / 2 (in which case the whole axis is inside the radius anyway).
  shifts <- if (isTRUE(cyclic)) c(-period, 0, period) else 0

  for (k in seq_along(obs_depth)) {

    zk <- obs_depth[k]
    if (!is.finite(zk)) next

    i1 <- max(1L, as.integer(floor((zk - rz - z0) / dz)) + 1L)
    i2 <- min(nz, as.integer(ceiling((zk + rz - z0) / dz)) + 1L)
    if (i2 < i1) next

    ii <- i1:i2
    bz <- ((depth_grid[ii] - zk) / depth_corr)^2

    for (sh in shifts) {

      tk <- obs_time[k] + sh
      if (!is.finite(tk)) next

      j1 <- max(1L, as.integer(floor((tk - rt - t0) / dt)) + 1L)
      j2 <- min(nt, as.integer(ceiling((tk + rt - t0) / dt)) + 1L)
      if (j2 < j1) next

      jj <- j1:j2
      at <- ((time_grid[jj] - tk) / time_corr)^2

      # outer() is column-major, matching the sub-block assignment below.
      block <- sqrt(outer(bz, at, "+"))
      rmin[ii, jj] <- pmin(as.vector(rmin[ii, jj, drop = FALSE]),
                           as.vector(block))
    }
  }

  rmin
}


# -----------------------------------------------------------------------------
#' Clever poor man's error estimate via DIVAnd (internal)
#'
#' Assumes the Julia session already holds the analysis inputs assigned by
#' \code{diva_plot_odv()}: \code{_odv_mask}, \code{_odv_pmn}, \code{_odv_xi},
#' \code{_odv_x}, \code{_odv_f_norm}, \code{_odv_len}, \code{_odv_eps2}.
#'
#' \code{DIVAnd_cpme()} forwards unmatched keyword arguments to
#' \code{DIVAndrun()}, so \code{moddim} must be passed here whenever it was
#' passed to the analysis, or the error field will be discontinuous at the
#' cyclic seam while the field itself is continuous.
#'
#' \code{diva_plot_odv()} always assigns \code{_odv_moddim} and always passes it
#' to \code{DIVAndrun}, so it is reused here rather than rebuilt. That keeps the
#' error field and the analysis on the same cyclic topology by construction.
#'
#' @return Matrix of dimensionless error in \[0, 1\].
#' @noRd
.odv_cpme <- function() {

  kw <- ", moddim = _odv_moddim"

  call_str <- paste0(
    "_odv_cpme = DIVAnd.DIVAnd_cpme(_odv_mask, _odv_pmn, _odv_xi, _odv_x, ",
    "_odv_f_norm, _odv_len, _odv_eps2", kw, ")"
  )

  tryCatch(
    JuliaCall::julia_eval(call_str),
    error = function(e)
      stop("DIVAnd_cpme failed: ", conditionMessage(e),
           "\n  Call: ", call_str, call. = FALSE)
  )

  out <- JuliaCall::julia_eval("_odv_cpme")

  if (!is.matrix(out))
    stop("DIVAnd_cpme returned an unexpected shape (not a matrix).",
         call. = FALSE)

  out
}


# -----------------------------------------------------------------------------
#' Apply an ODV-style mask to an interpolated grid (internal)
#'
#' Sets \code{grid_df[[var]]} to \code{NA} where the field is not supported by
#' observations, and attaches a \code{mask_metric} column carrying the raw
#' metric so callers can re-threshold.
#'
#' Row order is irrelevant: the metric matrix is mapped onto rows by exact
#' match against the sorted unique axis values, which are bit-identical to the
#' column values they were derived from.
#'
#' @param grid_df Tibble with \code{Depth}, \code{time_yr} and \code{var}.
#' @param var Character. Value column name.
#' @param resolved List from \code{.odv_mask_resolve()}.
#' @param obs_depth,obs_time Observation coordinates on the analysis scales.
#' @param depth_corr,time_corr Correlation lengths on the analysis scales.
#' @param cyclic,period Cyclic-time handling.
#' @param verbose Logical. Report the masked fraction.
#' @return \code{grid_df}, masked, with a \code{mask_metric} column and a
#'   \code{divaodv_mask} attribute.
#' @noRd
.odv_apply_mask <- function(grid_df, var, resolved,
                           obs_depth, obs_time,
                           depth_corr, time_corr,
                           cyclic = FALSE, period = 1,
                           verbose = TRUE) {

  method <- resolved$method
  if (identical(method, "none")) return(grid_df)

  if (!all(c("Depth", "time_yr") %in% names(grid_df)))
    stop("Internal error: grid_df must contain 'Depth' and 'time_yr' before ",
         "masking.", call. = FALSE)
  if (!var %in% names(grid_df))
    stop("Internal error: grid_df must contain '", var, "' before masking.",
         call. = FALSE)

  depth_grid <- sort(unique(grid_df$Depth))
  time_grid  <- sort(unique(grid_df$time_yr))

  metric_mat <- switch(
    method,
    support = .odv_support_rmin(
      depth_grid = depth_grid, time_grid = time_grid,
      obs_depth  = obs_depth,  obs_time  = obs_time,
      depth_corr = depth_corr, time_corr = time_corr,
      radius     = resolved$threshold * .odv_mask_search_factor,
      cyclic     = cyclic, period = period
    ),
    cpme = .odv_cpme()
  )

  if (!identical(dim(metric_mat), c(length(depth_grid), length(time_grid))))
    stop("Internal error: mask metric has dimensions ",
         paste(dim(metric_mat), collapse = " x "), " but the grid is ",
         length(depth_grid), " x ", length(time_grid), ".", call. = FALSE)

  i <- match(grid_df$Depth,   depth_grid)
  j <- match(grid_df$time_yr, time_grid)
  if (anyNA(i) || anyNA(j))
    stop("Internal error: could not align the mask metric to the grid.",
         call. = FALSE)

  metric <- metric_mat[cbind(i, j)]
  masked <- metric > resolved$threshold

  grid_df[[var]][masked] <- NA_real_
  grid_df$mask_metric <- metric

  frac <- mean(masked)

  if (isTRUE(verbose)) message(sprintf(
    "[ODV] mask = %s (threshold %.3g) | %.1f%% of nodes masked",
    method, resolved$threshold, 100 * frac
  ))

  attr(grid_df, "divaodv_mask") <- list(
    method    = method,
    threshold = resolved$threshold,
    fraction  = frac,
    exact_to  = if (method == "support")
      resolved$threshold * .odv_mask_search_factor else NA_real_
  )

  grid_df
}
