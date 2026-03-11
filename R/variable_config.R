# =============================================================================
# variable_config.R
# DIVA variable configuration constructor
# =============================================================================

#' Construct a DIVA variable configuration object
#'
#' Creates a validated configuration list for a single oceanographic variable,
#' specifying the DIVA interpolation parameters and optional transformation.
#' Useful for multi-variable workflows where you loop over a list of configs.
#'
#' @param var Character. Column name of the variable in the data tibble.
#' @param transform Character. Transformation to apply before DIVA interpolation.
#'   Must be \code{"none"} or \code{"log"}. Default \code{"none"}.
#' @param time_corr Numeric. Temporal correlation length in days. Must be > 0.
#' @param depth_corr Numeric. Depth correlation length in metres. Must be > 0.
#' @param epsilon2 Numeric. Signal-to-noise ratio parameter for DIVAnd.
#'   Must be > 0. Default \code{0.01}.
#' @param category Character or NULL. Semantic category label — one of
#'   \code{"physical"}, \code{"chemical"}, \code{"biological"}, or \code{NULL}
#'   (default). Used for faceting and labelling in downstream workflows.
#'
#' @return A named list of class \code{"diva_variable_config"} with elements
#'   \code{var}, \code{transform}, \code{time_corr}, \code{depth_corr},
#'   \code{epsilon2}, and \code{category}.
#'
#' @examples
#' # Physical variable — no transform, longer correlation lengths
#' cfg_temp <- diva_variable_config(
#'   var        = "Temp",
#'   transform  = "none",
#'   time_corr  = 20,
#'   depth_corr = 15,
#'   epsilon2   = 0.01,
#'   category   = "physical"
#' )
#'
#' # Biological variable — log transform, shorter correlations
#' cfg_chl <- diva_variable_config(
#'   var        = "Chl_a",
#'   transform  = "log",
#'   time_corr  = 7,
#'   depth_corr = 10,
#'   epsilon2   = 0.02,
#'   category   = "biological"
#' )
#'
#' # Use in a multi-variable loop
#' configs <- list(cfg_temp, cfg_chl)
#' for (cfg in configs) {
#'   cat(cfg$var, "— L_t:", cfg$time_corr, "d, L_z:", cfg$depth_corr, "m\n")
#' }
#'
#' @export
diva_variable_config <- function(var,
                                  transform  = "none",
                                  time_corr,
                                  depth_corr,
                                  epsilon2   = 0.01,
                                  category   = NULL) {

  # --- input validation -------------------------------------------------------
  if (!is.character(var) || length(var) != 1L || nchar(var) == 0L) {
    stop("`var` must be a single non-empty character string.")
  }

  valid_transforms <- c("none", "log")
  if (!transform %in% valid_transforms) {
    stop("`transform` must be one of: ", paste(valid_transforms, collapse = ", "),
         ". Got: '", transform, "'.")
  }

  if (!is.numeric(time_corr) || length(time_corr) != 1L || time_corr <= 0) {
    stop("`time_corr` must be a single positive number.")
  }

  if (!is.numeric(depth_corr) || length(depth_corr) != 1L || depth_corr <= 0) {
    stop("`depth_corr` must be a single positive number.")
  }

  if (!is.numeric(epsilon2) || length(epsilon2) != 1L || epsilon2 <= 0) {
    stop("`epsilon2` must be a single positive number.")
  }

  if (!is.null(category) && (!is.character(category) ||
      !category %in% c("physical", "chemical", "biological"))) {
    stop("`category` must be NULL or one of: 'physical', 'chemical', 'biological'.")
  }

  # --- construct --------------------------------------------------------------
  cfg <- list(
    var        = var,
    transform  = transform,
    time_corr  = time_corr,
    depth_corr = depth_corr,
    epsilon2   = epsilon2,
    category   = category
  )
  class(cfg) <- "diva_variable_config"
  cfg
}


#' Print method for diva_variable_config
#'
#' @param x A \code{diva_variable_config} object.
#' @param ... Ignored.
#' @export
print.diva_variable_config <- function(x, ...) {
  cat(sprintf(
    "<diva_variable_config>  %s  [%s]  L_t=%g d  L_z=%g m  eps2=%g  transform=%s\n",
    x$var,
    if (is.null(x$category)) "uncat." else x$category,
    x$time_corr, x$depth_corr, x$epsilon2, x$transform
  ))
  invisible(x)
}
