# =============================================================================
# diva_plot_odv.R
# ODV-style depth x time section plot — geom_raster + geom_contour +
# geom_text_contour, matching the original ODV_plots_JuliaCall_fixed.R style.
# =============================================================================
#
#' @importFrom dplyr filter mutate select distinct if_else
#' @importFrom ggplot2 ggplot aes geom_raster geom_point geom_contour
#'   scale_fill_gradientn scale_y_reverse scale_x_date labs ggtitle
#'   theme_minimal theme element_text unit
#' @importFrom metR geom_text_contour
#' @importFrom lubridate as_date
#' @importFrom JuliaCall julia_assign julia_eval
#' @importFrom rlang .data
NULL

# Suppress R CMD check NOTEs for column names used in NSE
utils::globalVariables(c("Date", "Depth", "depth", "time_yr", "value"))

# ODV rainbow palette — reversed for fill (cold = blue, warm = red)
.odv_colours <- c("#feb483", "#d31f2a", "#ffc000", "#27ab19",
                  "#0db5e6", "#7139fe", "#d16cfa")


# -----------------------------------------------------------------------------
#' ODV-style depth-time section plot
#'
#' Runs DIVAnd interpolation on a single variable using year-scaled time
#' (~365 grid points per year) and renders a depth x time raster with
#' contour lines and contour labels, matching the Ocean Data View aesthetic.
#'
#' @param df Tibble with \code{Date} (Date), \code{depth} (numeric m), and
#'   the variable column.
#' @param var Character. Variable column name.
#' @param time_corr Numeric. Temporal correlation length in **days**. Default 15.
#' @param depth_corr Numeric. Depth correlation length in metres. Default 10.
#' @param epsilon2 Numeric. DIVAnd signal-to-noise ratio. Default 0.01.
#' @param transform Character. \code{"none"} (default) or \code{"log"}.
#' @param max_depth Numeric or NULL. Maximum grid depth in metres. NULL
#'   auto-derives from non-NA observations.
#' @param depth_resolution Numeric. Grid spacing in metres (default 1).
#'   Increasing to 2 halves the depth dimension.
#' @param time_resolution Numeric. Grid points per year (default 365).
#'   Use 180 for ~bi-daily, 52 for weekly, 12 for monthly.
#' @param log_constant Numeric. Constant added before log transform. Default 1e-10.
#' @param palette Character or vector. \code{"odv"} (default), \code{"viridis"},
#'   or a custom hex colour vector. Palette is always reversed for fill.
#' @param mask_beyond_corr Logical. Mask cells outside the
#'   ±\code{time_corr}-day observation envelope. Default FALSE.
#' @param contour_binwidth Numeric. Binwidth for \code{geom_contour} and
#'   \code{geom_text_contour}. Default 1.
#' @param label_binwidth Numeric. Binwidth for contour labels (should be a
#'   multiple of \code{contour_binwidth}). Default same as
#'   \code{contour_binwidth}.
#' @param label_gap Numeric. Minimum gap between contour labels passed to
#'   \code{metR::geom_text_contour(skip)}. Default 0 (no extra skipping).
#' @param sample_points Logical. Overlay observation locations as small black
#'   dots. Default TRUE.
#' @param zlim Numeric(2) or NULL. Colour scale limits.
#' @param title Character or NULL. Plot title; defaults to \code{var}.
#' @param y_label Character. Y-axis label. Default \code{"Depth (m)"}.
#' @param x_label Character. X-axis label. Default \code{NULL} (no label).
#' @param fill_label Character. Legend title. Default \code{var}.
#' @param return_data Logical. Return grid tibble instead of plot. Default FALSE.
#' @param verbose Logical. Print progress. Default TRUE.
#'
#' @return A \code{ggplot} object, or a tibble when \code{return_data = TRUE}.
#' @export
diva_plot_odv <- function(df,
                           var,
                           time_corr        = 15,
                           depth_corr       = 10,
                           epsilon2         = 0.01,
                           transform        = "none",
                           max_depth        = NULL,
                           depth_resolution = 1,
                           time_resolution  = 365,
                           log_constant     = 1e-10,
                           palette          = "odv",
                           mask_beyond_corr = FALSE,
                           contour_binwidth = 1,
                           label_binwidth   = NULL,
                           label_gap        = 0,
                           sample_points    = TRUE,
                           zlim             = NULL,
                           title            = NULL,
                           y_label          = "Depth (m)",
                           x_label          = NULL,
                           fill_label       = NULL,
                           return_data      = FALSE,
                           verbose          = TRUE) {

  # ── 0. Guards --------------------------------------------------------------
  if (!var %in% names(df))
    stop("Column '", var, "' not found in df.")
  if (!all(c("Date", "depth") %in% names(df)))
    stop("df must contain 'Date' and 'depth'.")

  if (is.null(label_binwidth)) label_binwidth <- contour_binwidth
  if (is.null(fill_label))     fill_label     <- var

  .diva_ensure_julia()

  # ── 1. Prepare observations -----------------------------------------------
  dat <- df |>
    dplyr::select(dplyr::all_of(c("Date", "depth", var))) |>
    dplyr::mutate(Date = as.Date(Date), depth = as.numeric(depth)) |>
    dplyr::filter(!is.na(.data[[var]]), !is.na(Date), !is.na(depth))

  if (nrow(dat) < 5L)
    stop("Fewer than 5 non-NA observations for '", var, "'.")

  # ── 2. Log transform -------------------------------------------------------
  if (transform == "log") {
    if (any(dat[[var]] <= 0, na.rm = TRUE))
      warning("Non-positive values in '", var,
              "' -- adding log_constant (", log_constant, ") before log.")
    dat[[var]] <- log(dat[[var]] + log_constant)
    dat <- dat[is.finite(dat[[var]]), ]
  }

  # ── 3. Grid dimensions — year-scaled time ---------------------------------
  min_date_r      <- min(dat$Date)
  max_date_r      <- max(dat$Date)
  date_span_years <- as.numeric(max_date_r - min_date_r) / 365.0

  if (is.null(max_depth))
    max_depth <- ceiling(max(dat$depth) / 10) * 10

  n_depth     <- max(10L, as.integer(floor(max_depth / depth_resolution)) + 1L)
  n_time      <- max(20L, as.integer(ceiling(date_span_years * time_resolution)))
  scaled_time <- as.numeric(dat$Date - min_date_r) / 365.0
  max_scaled  <- max(scaled_time)
  time_corr_yr <- time_corr / 365.0

  n_cells <- as.numeric(n_depth) * n_time

  if (verbose) message(sprintf(
    "[ODV] %s | %s to %s (%.2f yr) | grid %d x %d = %s cells (dz=%gm, dt=%.0f/yr) | len=(%.0fm, %.3f yr) eps2=%.3f",
    var, format(min_date_r), format(max_date_r), date_span_years,
    n_depth, n_time, format(round(n_cells), big.mark = ","),
    depth_resolution, time_resolution,
    depth_corr, time_corr_yr, epsilon2
  ))

  if (n_cells > 500000)
    warning(sprintf(
      "Grid for '%s' has %s cells -- consider increasing depth_resolution or decreasing time_resolution.",
      var, format(round(n_cells), big.mark = ",")
    ))

  # ── 4. Julia: assign -------------------------------------------------------
  JuliaCall::julia_assign("_odv_depths",    dat$depth)
  JuliaCall::julia_assign("_odv_times",     scaled_time)
  JuliaCall::julia_assign("_odv_values",    dat[[var]])
  JuliaCall::julia_assign("_odv_max_depth", as.numeric(max_depth))
  JuliaCall::julia_assign("_odv_max_time",  max_scaled)
  JuliaCall::julia_assign("_odv_n_depth",   as.integer(n_depth))
  JuliaCall::julia_assign("_odv_n_time",    as.integer(n_time))
  JuliaCall::julia_assign("_odv_len1",      as.numeric(depth_corr))
  JuliaCall::julia_assign("_odv_len2",      as.numeric(time_corr_yr))
  JuliaCall::julia_assign("_odv_eps2",      as.numeric(epsilon2))

  # ── 5. Julia: build grid, compute metric tensor, normalise -----------------
  JuliaCall::julia_eval("_odv_depth_grid = collect(Float64, LinRange(0.0, _odv_max_depth, _odv_n_depth))")
  JuliaCall::julia_eval("_odv_time_grid  = collect(Float64, LinRange(0.0, _odv_max_time,  _odv_n_time))")
  JuliaCall::julia_eval("_odv_mask       = BitArray(ones(Bool, _odv_n_depth, _odv_n_time))")

  # pmn = inverse of grid spacing (points per unit length) in each dimension.
  # depth axis: spacing = max_depth / (n_depth - 1) metres
  # time axis:  spacing = max_time  / (n_time  - 1) years
  # DIVAnd requires pmn so that correlation lengths (in metres / years) are

  # interpreted on the physical grid. Using ones() here is WRONG and causes
  # vertical-stripe artefacts when the grid is not unit-spaced.
  JuliaCall::julia_eval("_odv_dz = _odv_n_depth > 1 ? _odv_max_depth / (_odv_n_depth - 1) : 1.0")
  JuliaCall::julia_eval("_odv_dt = _odv_n_time  > 1 ? _odv_max_time  / (_odv_n_time  - 1) : 1.0")
  JuliaCall::julia_eval("_odv_pm = fill(1.0 / _odv_dz, _odv_n_depth, _odv_n_time)")
  JuliaCall::julia_eval("_odv_pn = fill(1.0 / _odv_dt, _odv_n_depth, _odv_n_time)")
  JuliaCall::julia_eval("_odv_pmn = (_odv_pm, _odv_pn)")

  JuliaCall::julia_eval("_odv_depth_mat  = [_odv_depth_grid[i] for i in 1:_odv_n_depth, j in 1:_odv_n_time]")
  JuliaCall::julia_eval("_odv_time_mat   = [_odv_time_grid[j]  for i in 1:_odv_n_depth, j in 1:_odv_n_time]")
  JuliaCall::julia_eval("_odv_xi         = (_odv_depth_mat, _odv_time_mat)")
  JuliaCall::julia_eval("_odv_x          = (_odv_depths, _odv_times)")
  JuliaCall::julia_eval("_odv_f_mean     = mean(_odv_values)")
  JuliaCall::julia_eval("_odv_f_std      = std(_odv_values)")
  JuliaCall::julia_eval("_odv_f_std      = _odv_f_std == 0.0 ? 1.0 : _odv_f_std")
  JuliaCall::julia_eval("_odv_f_norm     = (_odv_values .- _odv_f_mean) ./ _odv_f_std")
  JuliaCall::julia_eval("_odv_len        = (_odv_len1, _odv_len2)")

  # ── 6. Julia: DIVAnd ------------------------------------------------------
  tryCatch(
    JuliaCall::julia_eval("_odv_result = DIVAnd.DIVAndrun(_odv_mask, _odv_pmn, _odv_xi, _odv_x, _odv_f_norm, _odv_len, _odv_eps2)"),
    error = function(e) stop("DIVAnd failed for '", var, "': ", conditionMessage(e))
  )
  JuliaCall::julia_eval("_odv_rescaled = _odv_result[1] .* _odv_f_std .+ _odv_f_mean")

  # ── 7. Julia: back-transform ----------------------------------------------
  if (transform == "log") {
    JuliaCall::julia_assign("_odv_log_const", log_constant)
    JuliaCall::julia_eval("_odv_final = exp.(_odv_rescaled) .- _odv_log_const")
    JuliaCall::julia_eval("_odv_final = max.(_odv_final, 0.0)")
  } else {
    JuliaCall::julia_eval("_odv_final = _odv_rescaled")
  }

  # ── 8. Extract into R tibble ----------------------------------------------
  grid_df <- dplyr::tibble(
    Depth   = JuliaCall::julia_eval("vec(_odv_depth_mat)"),
    time_yr = JuliaCall::julia_eval("vec(_odv_time_mat)"),
    value   = JuliaCall::julia_eval("vec(_odv_final)")
  ) |>
    dplyr::mutate(
      Date = lubridate::as_date(min_date_r + as.integer(round(time_yr * 365)))
    )

  # ── 9. Mask outside correlation envelope (optional) ----------------------
  if (mask_beyond_corr) {
    obs_dates     <- unique(as.Date(dat$Date))
    allowed_dates <- lapply(obs_dates, function(d)
      seq.Date(d - as.integer(time_corr), d + as.integer(time_corr), by = "day")
    ) |> unlist() |> as.Date(origin = "1970-01-01") |> unique()
    allowed_dates <- allowed_dates[
      allowed_dates >= min_date_r & allowed_dates <= max_date_r
    ]
    grid_df <- grid_df |>
      dplyr::mutate(value = dplyr::if_else(Date %in% allowed_dates, value, NA_real_))
  }

  names(grid_df)[names(grid_df) == "value"] <- var

  if (return_data)
    return(dplyr::select(grid_df, Date, Depth, dplyr::all_of(var)))

  # ── 10. Sample-point reference tibble ------------------------------------
  sampled <- df |>
    dplyr::filter(!is.na(.data[[var]])) |>
    dplyr::select(Date, depth) |>
    dplyr::rename(Depth = depth) |>
    dplyr::mutate(Date = lubridate::as_date(Date)) |>
    dplyr::distinct()

  # ── 11. Colour palette (reversed — cold = blue, warm = red) --------------
  fill_colours <- rev(
    if (identical(palette, "odv")) {
      .odv_colours
    } else if (identical(palette, "viridis")) {
      scales::viridis_pal()(11)
    } else {
      palette
    }
  )

  if (is.null(zlim)) zlim <- range(grid_df[[var]], na.rm = TRUE)

  # ── 12. Build ggplot (matching original ODV style) -----------------------
  p <- ggplot2::ggplot(grid_df, ggplot2::aes(x = Date, y = Depth)) +

    # Raster fill layer
    ggplot2::geom_raster(ggplot2::aes(fill = .data[[var]])) +

    # Sample locations
    {if (sample_points)
      ggplot2::geom_point(
        data        = sampled,
        ggplot2::aes(x = lubridate::as_date(Date), y = Depth),
        colour      = "black",
        size        = 0.25,
        inherit.aes = FALSE
      )
    } +

    # Contour lines
    ggplot2::geom_contour(
      ggplot2::aes(z = .data[[var]]),
      binwidth = contour_binwidth,
      colour   = "black",
      alpha    = 0.8
    ) +

    # Contour labels
    metR::geom_text_contour(
      ggplot2::aes(z = .data[[var]]),
      binwidth = label_binwidth,
      stroke   = 0.025,
      skip     = label_gap
    ) +

    # Colour scale
    ggplot2::scale_fill_gradientn(
      colours  = fill_colours,
      limits   = zlim,
      na.value = "white",
      name     = fill_label,
      oob      = scales::squish
    ) +

    # Axes
    ggplot2::scale_y_reverse() +
    ggplot2::scale_x_date(date_labels = "%b %Y", date_breaks = "1 year") +

    # Labels
    ggplot2::labs(
      y = y_label,
      x = x_label
    ) +
    ggplot2::ggtitle(if (is.null(title)) var else title) +

    # Theme
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      axis.text.x       = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid.minor  = ggplot2::element_blank(),
      legend.key.height = ggplot2::unit(1.5, "cm")
    )

  p
}
