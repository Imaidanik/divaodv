# =============================================================================
# scale_fill_odv.R
# ODV-faithful nonlinear colour mapping for fill scales.
#
# Reproduces the visual effect of Ocean Data View's Color Mapping tab
# (Median + Nonlinearity sliders) by repositioning the colour stops of a
# ggplot2 gradient scale, leaving the data and the colourbar axis linear.
# =============================================================================
#
#' @importFrom grDevices colorRampPalette
#' @importFrom ggplot2 scale_fill_gradientn waiver
#' @importFrom stats median quantile
NULL


# --- internal: NULL-coalesce -------------------------------------------------
.odv_or <- function(a, b) if (is.null(a)) b else a


# -----------------------------------------------------------------------------
# Transfer function
#
# g: [0,1] -> [0,1], monotone, g(0)=0, g(1)=1, composed of two stages.
#
#   Stage 1 (median):       g1(u) = u ^ (log(0.5) / log(median))
#                           sends `median` to the palette midpoint; identity
#                           at median = 0.5.
#
#   Stage 2 (nonlinearity): symmetric S-curve about 0.5 with exponent
#                           k = 1 + 3 * nonlinearity; identity at k = 1.
#                           Exponent k (NOT 1/k) steepens the curve at the
#                           midpoint, concentrating palette resolution around
#                           the median. Using 1/k inverts the intended effect.
#
# Both stages are pure powers, so the inverse is closed form.
# -----------------------------------------------------------------------------

#' @noRd
.odv_clamp_params <- function(median, nonlinearity) {
  if (!is.numeric(median) || length(median) != 1L || !is.finite(median))
    stop("`median` must be a single finite number in [0, 1].", call. = FALSE)
  if (!is.numeric(nonlinearity) || length(nonlinearity) != 1L ||
      !is.finite(nonlinearity))
    stop("`nonlinearity` must be a single finite number in [0, 1].",
         call. = FALSE)

  list(
    median       = min(max(median, 0.01), 0.99),
    nonlinearity = min(max(nonlinearity, 0), 1)
  )
}

#' @noRd
.odv_s_curve <- function(v, k) {
  ifelse(v <= 0.5,
         0.5 * (2 * v)^k,
         1 - 0.5 * (2 * (1 - v))^k)
}

#' @noRd
.odv_transfer <- function(u, median = 0.5, nonlinearity = 0) {
  pr    <- .odv_clamp_params(median, nonlinearity)
  gamma <- log(0.5) / log(pr$median)
  k     <- 1 + 3 * pr$nonlinearity
  .odv_s_curve(u^gamma, k)
}

#' @noRd
.odv_transfer_inv <- function(x, median = 0.5, nonlinearity = 0) {
  pr    <- .odv_clamp_params(median, nonlinearity)
  gamma <- log(0.5) / log(pr$median)
  k     <- 1 + 3 * pr$nonlinearity
  .odv_s_curve(x, 1 / k)^(1 / gamma)
}


# -----------------------------------------------------------------------------
# Auto-fitting: derive (median, nonlinearity) from a data distribution.
#
# The ODV transfer curve is a two-parameter approximation to the empirical CDF
# of the field. Fitting it to the data is histogram equalisation: colour is
# allocated in proportion to where observations sit, not uniformly across the
# nominal range. `median` has a closed form (normalised data median -> palette
# midpoint); `nonlinearity` is a 1-D least-squares fit of the transfer curve to
# the empirical CDF, or a closed-form IQR heuristic.
# -----------------------------------------------------------------------------

#' @noRd
.odv_validate_or_auto <- function(x, nm) {
  if (identical(x, "auto")) return(invisible())
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x))
    stop(sprintf('`%s` must be a single finite number in [0, 1] or "auto".', nm),
         call. = FALSE)
  invisible()
}

#' @noRd
.odv_fit_map <- function(values, limits = NULL, method = c("cdf", "iqr"),
                         n_grid = 201L) {
  method <- match.arg(method)
  v <- values[is.finite(values)]
  if (length(v) < 2L)
    stop("Need at least 2 finite values to fit an ODV colour mapping.",
         call. = FALSE)

  if (is.null(limits)) limits <- range(v)
  if (!is.numeric(limits) || length(limits) != 2L ||
      !all(is.finite(limits)) || limits[2] <= limits[1])
    stop("`limits` must be a finite, increasing numeric pair.", call. = FALSE)

  # Normalised data positions in [0, 1].
  u <- pmin(pmax((v - limits[1]) / (limits[2] - limits[1]), 0), 1)

  # median: palette midpoint at the data median (ODV's Median-slider default).
  med <- min(max(stats::median(u), 0.01), 0.99)

  if (method == "iqr") {
    qs   <- stats::quantile(u, c(0.25, 0.75), names = FALSE)
    nlin <- min(max(1 - (qs[2] - qs[1]) / 0.5, 0), 1)
    return(c(median = med, nonlinearity = nlin))
  }

  # cdf: nonlinearity whose S-curve best tracks the empirical CDF (LS on a
  # grid of candidates -- the objective is smooth and cheap to evaluate).
  us  <- sort(u)
  Fs  <- (seq_along(us) - 0.5) / length(us)
  cand <- seq(0, 1, length.out = as.integer(n_grid))
  sse  <- vapply(cand, function(nl)
    mean((.odv_transfer(us, med, nl) - Fs)^2), numeric(1))
  c(median = med, nonlinearity = cand[which.min(sse)])
}

#' @noRd
.odv_resolve_auto <- function(object, inherited, plot) {
  med  <- object$median
  nlin <- object$nonlinearity
  if (!identical(med, "auto") && !identical(nlin, "auto"))
    return(list(median = med, nonlinearity = nlin))

  var  <- inherited$var
  vals <- if (!is.null(var) && !is.null(plot$data) &&
              var %in% names(plot$data)) plot$data[[var]] else NULL
  if (is.null(vals))
    stop('scale_fill_odv(median/nonlinearity = "auto") needs a diva_plot_odv() ',
         "plot that carries its interpolated grid.", call. = FALSE)

  fit <- .odv_fit_map(vals, .odv_or(object$limits, inherited$limits))
  if (identical(med,  "auto")) med  <- unname(fit[["median"]])
  if (identical(nlin, "auto")) nlin <- unname(fit[["nonlinearity"]])
  list(median = med, nonlinearity = nlin)
}


# -----------------------------------------------------------------------------
#' Fit an ODV colour mapping to a data distribution
#'
#' Derives the \code{median} and \code{nonlinearity} arguments of
#' \code{\link{scale_fill_odv}} directly from a set of values, so the colour
#' scale allocates palette resolution in proportion to where the data actually
#' sit. The ODV transfer curve is a two-parameter approximation to the
#' empirical cumulative distribution of the field (histogram equalisation):
#'
#' \itemize{
#'   \item \strong{median} -- the normalised position of the data median
#'     within \code{limits}, so the palette midpoint lands on the median value.
#'     This matches the default behaviour of Ocean Data View's Median slider.
#'   \item \strong{nonlinearity} -- with \code{method = "cdf"} (default), the
#'     value in \[0, 1\] whose S-curve best matches the empirical CDF by least
#'     squares; with \code{method = "iqr"}, a closed-form estimate from the
#'     inter-quartile spread (\code{1 - 2 * IQR / range}), a cheap fallback.
#' }
#'
#' @param values Numeric vector. The field to fit -- typically the interpolated
#'   grid from \code{diva_plot_odv(..., return_data = TRUE)}. Non-finite values
#'   are dropped.
#' @param limits Numeric(2) or NULL. Colour range the mapping is defined over.
#'   NULL (default) uses \code{range(values)}. Pass the plot's \code{zlim} to
#'   fit against the same range the scale will use.
#' @param method Character. \code{"cdf"} (default) or \code{"iqr"}.
#' @param n_grid Integer. Number of nonlinearity candidates for the CDF fit.
#'   Default 201.
#'
#' @return A named numeric vector \code{c(median = , nonlinearity = )}, ready
#'   to splice into \code{scale_fill_odv()} or \code{diva_plot_odv()}.
#'
#' @examples
#' \dontrun{
#' grid <- diva_plot_odv(df, "PO4", time_corr = 15, depth_corr = 10,
#'                       return_data = TRUE)
#' fit  <- odv_auto_map(grid$PO4, limits = c(0, 2))
#' fit
#' #>       median nonlinearity
#' #>        0.100        0.810
#'
#' p <- diva_plot_odv(df, "PO4", time_corr = 15, depth_corr = 10, zlim = c(0, 2))
#' p + scale_fill_odv(median = fit[["median"]], nonlinearity = fit[["nonlinearity"]])
#'
#' # Or let the scale fit itself from the plot's grid:
#' p + scale_fill_odv(median = "auto", nonlinearity = "auto")
#' }
#'
#' @seealso \code{\link{scale_fill_odv}}, \code{\link{diva_plot_odv}}
#' @export
odv_auto_map <- function(values, limits = NULL, method = c("cdf", "iqr"),
                         n_grid = 201L) {
  method <- match.arg(method)
  .odv_fit_map(values, limits = limits, method = method, n_grid = n_grid)
}


# -----------------------------------------------------------------------------
#' Colour stop positions for an ODV-style mapping (internal)
#'
#' Returns the \code{values} vector for \code{scale_fill_gradientn()}: the
#' position in rescaled \[0,1\] data space at which each of \code{n_colours}
#' evenly indexed palette colours should sit.
#'
#' @noRd
.odv_colour_stops <- function(median = 0.5, nonlinearity = 0, n_colours = 256) {
  if (!is.numeric(n_colours) || length(n_colours) != 1L || n_colours < 2)
    stop("`n_colours` must be a single number >= 2.", call. = FALSE)

  idx   <- seq(0, 1, length.out = as.integer(n_colours))
  stops <- .odv_transfer_inv(idx, median, nonlinearity)

  # Guard against floating-point drift at the endpoints and enforce strict
  # monotonicity, which scale_fill_gradientn() requires.
  stops[1]            <- 0
  stops[length(stops)] <- 1
  cummax(stops)
}


# -----------------------------------------------------------------------------
#' Resolve a palette specification to a colour vector (internal)
#' @noRd
.odv_palette_colours <- function(palette = "odv", reverse = TRUE,
                                 n_colours = 256) {
  base <- if (identical(palette, "odv")) {
    .odv_colours
  } else if (identical(palette, "viridis")) {
    scales::viridis_pal()(11)
  } else {
    palette
  }

  if (!is.character(base) || length(base) < 2L)
    stop("`palette` must be \"odv\", \"viridis\", or a character vector of ",
         "at least two colours.", call. = FALSE)

  if (isTRUE(reverse)) base <- rev(base)

  # Interpolate to n_colours first. Applying the transfer curve to only the
  # 7 raw ODV stops produces visible banding -- the curve can only reposition
  # stops that exist.
  grDevices::colorRampPalette(base)(as.integer(n_colours))
}


# -----------------------------------------------------------------------------
#' ODV-style nonlinear fill scale
#'
#' A drop-in replacement for the fill scale of a \code{\link{diva_plot_odv}}
#' plot, reproducing the visual effect of Ocean Data View's Color Mapping
#' controls. The data and the colourbar axis stay linear; only the positions
#' of the palette colours move, so legend ticks remain evenly spaced and
#' round-numbered while the colour distribution inside the bar becomes
#' non-uniform -- the ODV appearance.
#'
#' Because \code{diva_plot_odv()} returns an ordinary ggplot object, adding
#' this scale does \strong{not} re-run DIVAnd. The interpolated grid is
#' already baked into the plot, so recolouring is instant and can be iterated
#' the way you would drag the sliders in ODV.
#'
#' @param palette Character or vector, or NULL. \code{"odv"}, \code{"viridis"},
#'   or a custom hex colour vector. \code{NULL} (default) inherits from the
#'   \code{diva_plot_odv()} plot being added to, falling back to \code{"odv"}.
#' @param median Numeric in \[0, 1\], or \code{"auto"}. Position within the
#'   colour range that maps to the middle of the palette. Default 0.5 (no
#'   shift). Values below 0.5 give more palette to the upper part of the range;
#'   above 0.5, to the lower part. Clamped to \[0.01, 0.99\]. \code{"auto"}
#'   places the midpoint at the interpolated grid's median, resolved when the
#'   scale is added to the plot; see \code{\link{odv_auto_map}}.
#' @param nonlinearity Numeric in \[0, 1\], or \code{"auto"}. Strength of the
#'   S-curve. Default 0 (linear). Higher values concentrate palette resolution
#'   around \code{median} at the expense of the range extremes. \code{"auto"}
#'   fits the value that best matches the grid's empirical CDF (histogram
#'   equalisation); see \code{\link{odv_auto_map}}.
#' @param limits Numeric(2) or NULL. Colour scale limits. \code{NULL} (default)
#'   inherits the \code{zlim} of the plot being added to, falling back to the
#'   data range.
#' @param name Character or NULL. Legend title. \code{NULL} (default) inherits
#'   the \code{fill_label} of the plot being added to.
#' @param n_colours Integer. Number of interpolated palette stops. Default 256.
#'   Lower values band visibly at high \code{nonlinearity}.
#' @param reverse Logical. Reverse the palette (cold = blue, warm = red).
#'   Default TRUE, matching \code{diva_plot_odv()}.
#' @param oob Function handling out-of-bounds values. Default
#'   \code{scales::squish}.
#' @param na.value Colour for NA cells. Default \code{"white"}.
#' @param ... Further arguments passed to
#'   \code{\link[ggplot2]{scale_fill_gradientn}}.
#'
#' @return An object added to a ggplot with \code{+}. Inheritance from the
#'   host plot is resolved at that point, so the returned object is not itself
#'   a ggplot2 \code{Scale} until added.
#'
#' @examples
#' \dontrun{
#' p <- diva_plot_odv(df, "PO4", time_corr = 15, depth_corr = 10,
#'                    zlim = c(0, 2), fill_label = "PO4 (umol/kg)")
#'
#' # Recolour without re-interpolating; zlim and fill_label carry over.
#' p + scale_fill_odv(median = 0.35, nonlinearity = 0.6)
#'
#' # Override an inherited setting explicitly.
#' p + scale_fill_odv(median = 0.35, palette = "viridis")
#' }
#'
#' @seealso \code{\link{diva_plot_odv}}
#' @export
scale_fill_odv <- function(palette      = NULL,
                           median       = 0.5,
                           nonlinearity = 0,
                           limits       = NULL,
                           name         = NULL,
                           n_colours    = 256,
                           reverse      = TRUE,
                           oob          = scales::squish,
                           na.value     = "white",
                           ...) {

  # Validate eagerly so errors surface at the call site, not at print time.
  # median / nonlinearity may be the string "auto" (resolved at add time from
  # the host plot's interpolated grid); validate only the numeric case here.
  .odv_validate_or_auto(median, "median")
  .odv_validate_or_auto(nonlinearity, "nonlinearity")
  if (!is.null(limits) && (!is.numeric(limits) || length(limits) != 2L))
    stop("`limits` must be NULL or a numeric vector of length 2.",
         call. = FALSE)

  structure(
    list(
      palette      = palette,
      median       = median,
      nonlinearity = nonlinearity,
      limits       = limits,
      name         = name,
      n_colours    = n_colours,
      reverse      = reverse,
      oob          = oob,
      na.value     = na.value,
      extra        = list(...)
    ),
    class = "divaodv_deferred_scale"
  )
}


# -----------------------------------------------------------------------------
#' Build the concrete gradient scale from resolved settings (internal)
#' @noRd
.odv_build_scale <- function(spec) {
  do.call(
    ggplot2::scale_fill_gradientn,
    c(
      list(
        colours  = .odv_palette_colours(spec$palette, spec$reverse,
                                        spec$n_colours),
        values   = .odv_colour_stops(spec$median, spec$nonlinearity,
                                     spec$n_colours),
        limits   = spec$limits,
        name     = spec$name,
        na.value = spec$na.value,
        oob      = spec$oob
      ),
      spec$extra
    )
  )
}


# -----------------------------------------------------------------------------
#' Add an ODV fill scale to a ggplot, inheriting unset settings
#'
#' Resolution happens here rather than in \code{scale_fill_odv()} because in
#' \code{p + scale_fill_odv(...)} the scale is constructed before \code{+}
#' has any access to \code{p}.
#'
#' @param object A \code{divaodv_deferred_scale}.
#' @param plot The ggplot being added to.
#' @param object_name Name of the object, supplied by ggplot2.
#' @return The modified ggplot.
#' @exportS3Method ggplot2::ggplot_add
ggplot_add.divaodv_deferred_scale <- function(object, plot, object_name) {

  inherited <- attr(plot, "divaodv_fill")

  # Resolve "auto" median / nonlinearity from the plot's interpolated grid.
  resolved <- .odv_resolve_auto(object, inherited, plot)

  spec <- list(
    palette      = .odv_or(object$palette, .odv_or(inherited$palette, "odv")),
    median       = resolved$median,
    nonlinearity = resolved$nonlinearity,
    limits       = .odv_or(object$limits,  inherited$limits),
    name         = .odv_or(object$name,
                           .odv_or(inherited$name, ggplot2::waiver())),
    n_colours    = object$n_colours,
    reverse      = .odv_or(object$reverse, .odv_or(inherited$reverse, TRUE)),
    oob          = object$oob,
    na.value     = object$na.value,
    extra        = object$extra
  )

  out <- plot + .odv_build_scale(spec)

  # Preserve the inheritance attribute so a further + scale_fill_odv() on the
  # result can still inherit.
  if (!is.null(inherited)) attr(out, "divaodv_fill") <- inherited
  out
}


# -----------------------------------------------------------------------------
#' Print a deferred ODV fill scale
#'
#' @param x A \code{divaodv_deferred_scale}.
#' @param ... Ignored.
#' @return \code{x}, invisibly.
#' @export
print.divaodv_deferred_scale <- function(x, ...) {
  fmt  <- function(v) if (is.null(v)) "<inherit>" else
    paste(format(v, trim = TRUE), collapse = ", ")
  fmtp <- function(v) if (identical(v, "auto")) "auto" else sprintf("%g", v)
  cat(sprintf(
    paste0("<divaodv fill scale>  median=%s  nonlinearity=%s  ",
           "n_colours=%d\n  palette=%s  limits=%s  name=%s  reverse=%s\n",
           "  (settings shown as <inherit> resolve when added to a plot;\n",
           '   "auto" resolves from the plot\'s interpolated grid)\n'),
    fmtp(x$median), fmtp(x$nonlinearity), as.integer(x$n_colours),
    fmt(x$palette), fmt(x$limits), fmt(x$name), fmt(x$reverse)
  ))
  invisible(x)
}
