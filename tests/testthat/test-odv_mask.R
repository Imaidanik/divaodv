# =============================================================================
# test-odv_mask.R
# Tests for ODV-style masking. None of these require Julia or DIVAnd:
# the "support" method is pure geometry and "cpme" is only exercised through
# argument validation here.
# =============================================================================

# --- helpers -----------------------------------------------------------------

make_grid <- function(depth_grid, time_grid, value = 1) {
  g <- expand.grid(Depth = depth_grid, time_yr = time_grid,
                   KEEP.OUT.ATTRS = FALSE)
  g$Temp <- value
  dplyr::as_tibble(g)
}


# --- .odv_mask_resolve -------------------------------------------------------

test_that("mask defaults resolve to the documented values", {
  expect_equal(.odv_mask_resolve("support")$threshold, 2)
  expect_equal(.odv_mask_resolve("cpme")$threshold, 0.8)
  expect_equal(.odv_mask_resolve("support")$method, "support")
  expect_equal(.odv_mask_resolve("none")$method, "none")
})

test_that("explicit thresholds override the defaults", {
  expect_equal(.odv_mask_resolve("support", 3.5)$threshold, 3.5)
  expect_equal(.odv_mask_resolve("cpme", 0.9)$threshold, 0.9)
})

test_that("invalid mask arguments are rejected", {
  expect_error(.odv_mask_resolve("errormap"), "must be one of")
  expect_error(.odv_mask_resolve(c("support", "cpme")), "single character")
  expect_error(.odv_mask_resolve("support", -1), "positive finite")
  expect_error(.odv_mask_resolve("support", 0), "positive finite")
  expect_error(.odv_mask_resolve("support", c(1, 2)), "positive finite")
  expect_error(.odv_mask_resolve("support", NA_real_), "positive finite")
})

test_that("a cpme threshold of 1 or more is rejected as a no-op", {
  expect_error(.odv_mask_resolve("cpme", 1), "would mask nothing")
  expect_error(.odv_mask_resolve("cpme", 1.5), "would mask nothing")
})

test_that("mask_beyond_corr is deprecated but still honoured", {
  expect_warning(res <- .odv_mask_resolve(mask_beyond_corr = TRUE),
                 "deprecated")
  expect_equal(res$method, "legacy")
})

test_that("mask_beyond_corr cannot be combined with mask", {
  expect_error(
    .odv_mask_resolve("support", mask_beyond_corr = TRUE),
    "cannot be combined"
  )
  expect_error(
    .odv_mask_resolve(mask_threshold = 2, mask_beyond_corr = TRUE),
    "no effect"
  )
})

test_that("mask_threshold without a mask warns rather than failing silently", {
  expect_warning(.odv_mask_resolve("none", 2), "ignored")
})


# --- .odv_support_rmin: geometry ---------------------------------------------

test_that("distance is zero at an observation sitting on a node", {
  rmin <- .odv_support_rmin(
    depth_grid = 0:10, time_grid = seq(0, 1, length.out = 11),
    obs_depth = 5, obs_time = 0.5,
    depth_corr = 5, time_corr = 0.5, radius = 2
  )
  expect_equal(rmin[6, 6], 0)
})

test_that("the metric is anisotropic in the correlation lengths", {
  rmin <- .odv_support_rmin(
    depth_grid = 0:10, time_grid = seq(0, 1, length.out = 11),
    obs_depth = 5, obs_time = 0.5,
    depth_corr = 5, time_corr = 0.5, radius = 2
  )
  # One correlation length away in depth only ...
  expect_equal(rmin[11, 6], 1)
  # ... and one correlation length away in time only.
  expect_equal(rmin[6, 11], 1)
})

test_that("nodes beyond the search radius are Inf", {
  rmin <- .odv_support_rmin(
    depth_grid = 0:10, time_grid = seq(0, 1, length.out = 11),
    obs_depth = 5, obs_time = 0.5,
    depth_corr = 5, time_corr = 0.5, radius = 0.5
  )
  expect_true(is.infinite(rmin[11, 6]))
  expect_equal(rmin[6, 6], 0)
})

test_that("the metric takes the minimum over several observations", {
  rmin <- .odv_support_rmin(
    depth_grid = 0:10, time_grid = seq(0, 1, length.out = 11),
    obs_depth = c(0, 10), obs_time = c(0.5, 0.5),
    depth_corr = 5, time_corr = 0.5, radius = 3
  )
  # Midpoint is one correlation length from each; the min is still 1.
  expect_equal(rmin[6, 6], 1)
  expect_equal(rmin[1, 6], 0)
  expect_equal(rmin[11, 6], 0)
})

test_that("an empty observation set masks everything", {
  rmin <- .odv_support_rmin(
    depth_grid = 0:10, time_grid = seq(0, 1, length.out = 11),
    obs_depth = numeric(0), obs_time = numeric(0),
    depth_corr = 5, time_corr = 0.5, radius = 2
  )
  expect_true(all(is.infinite(rmin)))
})

test_that("a larger radius yields a pointwise-smaller-or-equal metric", {
  args <- list(
    depth_grid = 0:20, time_grid = seq(0, 2, length.out = 21),
    obs_depth = c(3, 12), obs_time = c(0.4, 1.6),
    depth_corr = 4, time_corr = 0.3
  )
  small <- do.call(.odv_support_rmin, c(args, radius = 1))
  large <- do.call(.odv_support_rmin, c(args, radius = 4))
  expect_true(all(large <= small))
  # And so masks a subset, for any threshold.
  expect_true(all((large > 1) <= (small > 1)))
})


# --- .odv_support_rmin: cyclic time -----------------------------------------

test_that("cyclic time wraps the distance across the seam", {
  monthly <- seq(0, 1 - 1 / 12, length.out = 12)

  common <- list(
    depth_grid = 0:10, time_grid = monthly,
    obs_depth = 5, obs_time = 0,
    depth_corr = 5, time_corr = 0.2, radius = 2
  )

  cyc <- do.call(.odv_support_rmin, c(common, cyclic = TRUE,  period = 1))
  lin <- do.call(.odv_support_rmin, c(common, cyclic = FALSE))

  # December (last node) is one month from a 1 January observation when the
  # axis wraps, and eleven months away when it does not.
  expect_true(is.finite(cyc[6, 12]))
  expect_true(is.infinite(lin[6, 12]))
})

test_that("the cyclic metric is continuous across the December/January seam", {
  monthly <- seq(0, 1 - 1 / 12, length.out = 12)

  rmin <- .odv_support_rmin(
    depth_grid = 0:10, time_grid = monthly,
    obs_depth = 5, obs_time = 0,
    depth_corr = 5, time_corr = 0.2, radius = 3,
    cyclic = TRUE, period = 1
  )

  # One month either side of the observation must be equidistant.
  expect_equal(rmin[6, 12], rmin[6, 2])
  # No jump at the fold: the seam step matches the interior step.
  expect_equal(rmin[6, 12] - rmin[6, 1], rmin[6, 2] - rmin[6, 1])
})


# --- .odv_apply_mask ---------------------------------------------------------

test_that("mask = none leaves the grid untouched", {
  g <- make_grid(0:10, seq(0, 1, length.out = 11))
  out <- .odv_apply_mask(
    g, "Temp", .odv_mask_resolve("none"),
    obs_depth = 5, obs_time = 0.5,
    depth_corr = 5, time_corr = 0.5, verbose = FALSE
  )
  expect_identical(out, g)
  expect_false("mask_metric" %in% names(out))
})

test_that("the legacy path is a no-op inside apply_mask", {
  g <- make_grid(0:10, seq(0, 1, length.out = 11))
  suppressWarnings(res <- .odv_mask_resolve(mask_beyond_corr = TRUE))
  out <- .odv_apply_mask(
    g, "Temp", res,
    obs_depth = 5, obs_time = 0.5,
    depth_corr = 5, time_corr = 0.5, verbose = FALSE
  )
  expect_identical(out, g)
})

test_that("masking sets NA, adds mask_metric, and records an attribute", {
  g <- make_grid(0:10, seq(0, 1, length.out = 11))
  out <- .odv_apply_mask(
    g, "Temp", .odv_mask_resolve("support", 1),
    obs_depth = 5, obs_time = 0.5,
    depth_corr = 5, time_corr = 0.5, verbose = FALSE
  )

  expect_true("mask_metric" %in% names(out))
  expect_equal(nrow(out), nrow(g))
  expect_true(anyNA(out$Temp))

  # Every NA is above threshold, every retained value is at or below it.
  expect_true(all(out$mask_metric[is.na(out$Temp)]  > 1))
  expect_true(all(out$mask_metric[!is.na(out$Temp)] <= 1))

  a <- attr(out, "divaodv_mask")
  expect_equal(a$method, "support")
  expect_equal(a$threshold, 1)
  expect_equal(a$fraction, mean(is.na(out$Temp)))
  expect_equal(a$exact_to, 2)   # threshold * search factor
})

test_that("mask_metric is exact beyond the threshold, up to the search factor", {
  g <- make_grid(0:10, seq(0, 1, length.out = 11))
  out <- .odv_apply_mask(
    g, "Temp", .odv_mask_resolve("support", 1),
    obs_depth = 5, obs_time = 0.5,
    depth_corr = 5, time_corr = 0.5, verbose = FALSE
  )
  # A node one correlation length out in *both* axes sits at sqrt(2): masked,
  # but its metric is still finite, so the user can re-threshold upward.
  row <- out[out$Depth == 10 & out$time_yr == 1, ]
  expect_true(is.na(row$Temp))
  expect_equal(row$mask_metric, sqrt(2))
})

test_that("the threshold is exclusive: metric == threshold is retained", {
  g <- make_grid(0:10, seq(0, 1, length.out = 11))
  out <- .odv_apply_mask(
    g, "Temp", .odv_mask_resolve("support", 1),
    obs_depth = 5, obs_time = 0.5,
    depth_corr = 5, time_corr = 0.5, verbose = FALSE
  )
  row <- out[out$Depth == 10 & out$time_yr == 0.5, ]
  expect_equal(row$mask_metric, 1)
  expect_false(is.na(row$Temp))
})

test_that("row order does not affect the mask", {
  g <- make_grid(0:10, seq(0, 1, length.out = 11))
  shuffled <- g[sample(nrow(g)), ]

  common <- list(
    resolved = .odv_mask_resolve("support", 1),
    obs_depth = 5, obs_time = 0.5,
    depth_corr = 5, time_corr = 0.5, verbose = FALSE
  )

  a <- do.call(.odv_apply_mask, c(list(g, "Temp"), common))
  b <- do.call(.odv_apply_mask, c(list(shuffled, "Temp"), common))

  key_a <- paste(a$Depth, a$time_yr)
  key_b <- paste(b$Depth, b$time_yr)
  expect_equal(a$mask_metric, b$mask_metric[match(key_a, key_b)])
})

test_that("a missing value column is caught", {
  g <- make_grid(0:10, seq(0, 1, length.out = 11))
  expect_error(
    .odv_apply_mask(g, "Salinity", .odv_mask_resolve("support"),
                    obs_depth = 5, obs_time = 0.5,
                    depth_corr = 5, time_corr = 0.5, verbose = FALSE),
    "must contain 'Salinity'"
  )
})
