# =============================================================================
# odv_contours.R
# Post-hoc contour control for ODV section plots.
#
# Both objects below are added to a finished ggplot with `+`, so contours can
# be set or stripped without re-running DIVAnd -- the same property that makes
# scale_fill_odv() cheap to iterate.
# =============================================================================
#
#' @importFrom ggplot2 geom_contour
#' @importFrom metR geom_text_contour
#' @importFrom rlang as_label
NULL


# -----------------------------------------------------------------------------
#' Is this layer a contour layer? (internal)
#'
#' Matched on geom class name rather than identity so that ggplot2's
#' \code{GeomContour} and metR's \code{GeomTextContour} / \code{GeomLabelContour}
#' / \code{GeomContour2} are all caught without metR needing to be attached.
#'
#' @noRd
.odv_is_contour_layer <- function(layer) {
  any(grepl("Contour", class(layer$geom), fixed = TRUE))
}


# -----------------------------------------------------------------------------
#' Drop every contour layer from a plot (internal)
#' @noRd
.odv_strip_contours <- function(plot) {
  keep <- !vapply(plot$layers, .odv_is_contour_layer, logical(1))
  plot$layers <- plot$layers[keep]
  plot
}


# -----------------------------------------------------------------------------
#' Work out which column the contours should be drawn on (internal)
#'
#' Prefers the \code{var} recorded by \code{diva_plot_odv()}; falls back to
#' sniffing the fill mapping of the first layer that has one, so the functions
#' still work on a hand-built ggplot.
#'
#' @noRd
.odv_resolve_var <- function(plot) {

  v <- attr(plot, "divaodv_fill")$var
  if (!is.null(v)) return(v)

  unwrap <- function(mapping) {
    if (is.null(mapping) || is.null(mapping$fill)) return(NULL)
    lbl <- rlang::as_label(mapping$fill)
    # aes(fill = .data[["Temp"]]) -> "Temp"; aes(fill = Temp) -> "Temp"
    if (grepl('^\\.data\\[\\[".*"\\]\\]$', lbl)) {
      sub('^\\.data\\[\\["(.*)"\\]\\]$', "\\1", lbl)
    } else {
      lbl
    }
  }

  for (l in plot$layers) {
    hit <- unwrap(l$mapping)
    if (!is.null(hit)) return(hit)
  }
  hit <- unwrap(plot$mapping)
  if (!is.null(hit)) return(hit)

  stop("Could not work out which variable to contour. Add this scale to a ",
       "plot from diva_plot_odv(), or map `fill` in the plot.", call. = FALSE)
}


# -----------------------------------------------------------------------------
#' Build contour (and optional label) layers (internal)
#'
#' Returns a list of layers, which ggplot2 accepts directly with \code{+}.
#' Shared by \code{diva_plot_odv()} and \code{odv_contours()} so the two can
#' never drift apart.
#'
#' @noRd
.odv_contour_layers <- function(var,
                                breaks         = NULL,
                                binwidth       = 1,
                                labels         = TRUE,
                                label_breaks   = NULL,
                                label_binwidth = NULL,
                                label_gap      = 0,
                                colour         = "black",
                                alpha          = 0.8,
                                linewidth      = 0.5,
                                extra          = list()) {

  # breaks wins over binwidth; passing both to geom_contour() is an error.
  line_spec <- if (!is.null(breaks)) list(breaks = breaks) else
    list(binwidth = binwidth)

  lab_spec <- if (!is.null(label_breaks)) list(breaks = label_breaks) else
    if (!is.null(label_binwidth)) list(binwidth = label_binwidth) else line_spec

  z_aes <- ggplot2::aes(z = .data[[var]])

  out <- list(
    do.call(ggplot2::geom_contour,
            c(list(mapping = z_aes, colour = colour, alpha = alpha,
                   linewidth = linewidth),
              line_spec, extra))
  )

  if (isTRUE(labels)) {
    out <- c(out, list(
      do.call(metR::geom_text_contour,
              c(list(mapping = z_aes, stroke = 0.025, skip = label_gap),
                lab_spec))
    ))
  }

  out
}


# -----------------------------------------------------------------------------
#' Set the contour lines on an ODV section plot
#'
#' Replaces any contour layers already on the plot with a new set. Because
#' \code{diva_plot_odv()} returns an ordinary ggplot object, adding this does
#' \strong{not} re-run DIVAnd -- the interpolated grid is already in the plot,
#' so contours can be iterated as cheaply as colour mapping.
#'
#' Existing contour layers are always removed first, so calling this on a plot
#' built with \code{add_contours = TRUE} leaves one set of contours, not two.
#'
#' @param breaks Numeric vector or NULL. Explicit contour levels, e.g.
#'   \code{c(20, 22, 24)}. Takes precedence over \code{binwidth}.
#' @param binwidth Numeric or NULL. Even spacing between contours, used only
#'   when \code{breaks} is NULL. Defaults to 1.
#' @param labels Logical. Label every contour drawn. Default TRUE.
#' @param label_gap Numeric. Minimum gap between labels, passed to
#'   \code{metR::geom_text_contour(skip = )}. Default 0.
#' @param colour Character. Contour line colour. Default \code{"black"}.
#' @param alpha Numeric. Contour line alpha. Default 0.8.
#' @param linewidth Numeric. Contour line width. Default 0.5.
#' @param ... Further arguments passed to \code{\link[ggplot2]{geom_contour}}.
#'
#' @return An object added to a ggplot with \code{+}.
#'
#' @examples
#' \dontrun{
#' p <- diva_plot_odv(df, "Temp", time_corr = 20, depth_corr = 15)
#'
#' p + odv_contours(breaks = c(20, 22, 24))     # explicit levels
#' p + odv_contours(binwidth = 2)               # even spacing
#' p + odv_contours(breaks = 22, labels = FALSE) # one unlabelled isotherm
#' }
#'
#' @seealso \code{\link{odv_contours_remove}}, \code{\link{diva_plot_odv}}
#' @export
odv_contours <- function(breaks    = NULL,
                         binwidth  = NULL,
                         labels    = TRUE,
                         label_gap = 0,
                         colour    = "black",
                         alpha     = 0.8,
                         linewidth = 0.5,
                         ...) {

  if (!is.null(breaks) && !is.numeric(breaks))
    stop("`breaks` must be NULL or a numeric vector.", call. = FALSE)
  if (!is.null(binwidth) &&
      (!is.numeric(binwidth) || length(binwidth) != 1L || binwidth <= 0))
    stop("`binwidth` must be NULL or a single positive number.", call. = FALSE)
  if (!is.logical(labels) || length(labels) != 1L)
    stop("`labels` must be TRUE or FALSE.", call. = FALSE)
  if (!is.null(breaks) && !is.null(binwidth))
    warning("Both `breaks` and `binwidth` given; `binwidth` is ignored.",
            call. = FALSE)

  structure(
    list(breaks = breaks, binwidth = binwidth, labels = labels,
         label_gap = label_gap, colour = colour, alpha = alpha,
         linewidth = linewidth, extra = list(...)),
    class = "divaodv_contours"
  )
}


# -----------------------------------------------------------------------------
#' Remove all contour lines from an ODV section plot
#'
#' Strips every contour and contour-label layer, leaving the raster fill,
#' sample points, scales and theme untouched. Contour layers added by hand are
#' removed too.
#'
#' @return An object added to a ggplot with \code{+}.
#'
#' @examples
#' \dontrun{
#' p <- diva_plot_odv(df, "Temp", time_corr = 20, depth_corr = 15,
#'                    add_contours = TRUE)
#' p + odv_contours_remove()
#' }
#'
#' @seealso \code{\link{odv_contours}}
#' @export
odv_contours_remove <- function() {
  structure(list(), class = "divaodv_contours_remove")
}


# -----------------------------------------------------------------------------
#' Add ODV contour layers to a ggplot
#'
#' @param object A \code{divaodv_contours}.
#' @param plot The ggplot being added to.
#' @param object_name Name of the object, supplied by ggplot2.
#' @return The modified ggplot.
#' @exportS3Method ggplot2::ggplot_add
ggplot_add.divaodv_contours <- function(object, plot, object_name) {

  var  <- .odv_resolve_var(plot)
  keep <- attr(plot, "divaodv_fill")
  out  <- .odv_strip_contours(plot)

  out <- out + .odv_contour_layers(
    var       = var,
    breaks    = object$breaks,
    binwidth  = if (is.null(object$breaks))
      if (is.null(object$binwidth)) 1 else object$binwidth else NULL,
    labels    = object$labels,
    label_gap = object$label_gap,
    colour    = object$colour,
    alpha     = object$alpha,
    linewidth = object$linewidth,
    extra     = object$extra
  )

  if (!is.null(keep)) attr(out, "divaodv_fill") <- keep
  out
}


# -----------------------------------------------------------------------------
#' Remove ODV contour layers from a ggplot
#'
#' @param object A \code{divaodv_contours_remove}.
#' @param plot The ggplot being added to.
#' @param object_name Name of the object, supplied by ggplot2.
#' @return The modified ggplot.
#' @exportS3Method ggplot2::ggplot_add
ggplot_add.divaodv_contours_remove <- function(object, plot, object_name) {
  keep <- attr(plot, "divaodv_fill")
  out  <- .odv_strip_contours(plot)
  if (!is.null(keep)) attr(out, "divaodv_fill") <- keep
  out
}


# -----------------------------------------------------------------------------
#' Print a deferred ODV contour specification
#'
#' @param x A \code{divaodv_contours}.
#' @param ... Ignored.
#' @return \code{x}, invisibly.
#' @export
print.divaodv_contours <- function(x, ...) {
  spec <- if (!is.null(x$breaks))
    paste0("breaks=", paste(format(x$breaks, trim = TRUE), collapse = ", "))
  else
    paste0("binwidth=", if (is.null(x$binwidth)) 1 else x$binwidth)
  cat(sprintf("<divaodv contours>  %s  labels=%s  colour=%s\n",
              spec, x$labels, x$colour))
  invisible(x)
}


# -----------------------------------------------------------------------------
#' Print a deferred ODV contour removal
#'
#' @param x A \code{divaodv_contours_remove}.
#' @param ... Ignored.
#' @return \code{x}, invisibly.
#' @export
print.divaodv_contours_remove <- function(x, ...) {
  cat("<divaodv contours>  remove all contour layers\n")
  invisible(x)
}
