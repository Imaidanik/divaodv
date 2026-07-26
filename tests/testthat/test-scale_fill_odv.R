test_that("transfer curve is the identity at default parameters", {
  expect_equal(.odv_colour_stops(0.5, 0, 256), seq(0, 1, length.out = 256))
})

test_that("stops are monotone, finite and span [0, 1] across a sweep", {
  grid <- expand.grid(m = c(0.01, 0.2, 0.5, 0.8, 0.99),
                      n = c(0, 0.3, 0.6, 1))
  for (i in seq_len(nrow(grid))) {
    s <- .odv_colour_stops(grid$m[i], grid$n[i], 256)
    expect_true(all(is.finite(s)))
    expect_true(all(diff(s) >= 0))
    expect_identical(s[1], 0)
    expect_identical(s[length(s)], 1)
  }
})

test_that("median maps to the palette midpoint", {
  expect_equal(.odv_transfer(0.7, median = 0.7, nonlinearity = 0), 0.5)
  expect_equal(.odv_transfer(0.3, median = 0.3, nonlinearity = 0.8), 0.5)
})

test_that("the inverse round-trips", {
  u <- seq(0.001, 0.999, length.out = 99)
  expect_equal(.odv_transfer_inv(.odv_transfer(u, 0.35, 0.6), 0.35, 0.6), u,
               tolerance = 1e-8)
})

test_that("nonlinearity concentrates resolution at the median, not the ends", {
  u     <- seq(0.001, 0.999, length.out = 999)
  slope <- function(nl) {
    g <- .odv_transfer(u, 0.5, nl)
    diff(g) / diff(u)
  }
  # Steeper at the midpoint and flatter at the low end than the linear case.
  expect_gt(slope(0.8)[500], slope(0)[500])
  expect_lt(slope(0.8)[3],   slope(0)[3])
})

test_that("out-of-range parameters are clamped rather than erroring", {
  expect_true(all(is.finite(.odv_colour_stops(0, 0.5, 64))))
  expect_true(all(is.finite(.odv_colour_stops(1, 0.5, 64))))
  expect_true(all(is.finite(.odv_colour_stops(0.5, 5, 64))))
  expect_error(scale_fill_odv(median = "a"), "single finite number")
  expect_error(scale_fill_odv(limits = c(1, 2, 3)), "length 2")
})

test_that("palettes resolve to n_colours entries", {
  expect_length(.odv_palette_colours("odv", TRUE, 256), 256)
  expect_length(.odv_palette_colours("viridis", TRUE, 256), 256)
  expect_length(.odv_palette_colours(c("#000000", "#ffffff"), TRUE, 64), 64)
  expect_error(.odv_palette_colours("#000000", TRUE, 64), "at least two")
})

test_that("unset arguments are inherited from a diva_plot_odv() plot", {
  d <- expand.grid(x = 1:5, y = 1:5)
  d$z <- seq(0, 2, length.out = 25)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, fill = z)) + ggplot2::geom_raster()
  attr(p, "divaodv_fill") <- list(palette = "odv", limits = c(0, 2),
                                  name = "PO4", reverse = TRUE)

  sc <- suppressMessages(
    ggplot2::ggplot_build(p + scale_fill_odv(median = 0.35))
  )$plot$scales$get_scales("fill")

  expect_equal(sc$limits, c(0, 2))
  expect_equal(sc$name, "PO4")
})

test_that("explicit arguments override inherited ones", {
  d <- expand.grid(x = 1:5, y = 1:5); d$z <- seq(0, 2, length.out = 25)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, fill = z)) + ggplot2::geom_raster()
  attr(p, "divaodv_fill") <- list(palette = "odv", limits = c(0, 2),
                                  name = "PO4", reverse = TRUE)
  sc <- suppressMessages(
    ggplot2::ggplot_build(p + scale_fill_odv(limits = c(0, 5)))
  )$plot$scales$get_scales("fill")
  expect_equal(sc$limits, c(0, 5))
})

test_that("the inheritance attribute survives so recolouring can be chained", {
  d <- expand.grid(x = 1:5, y = 1:5); d$z <- seq(0, 2, length.out = 25)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y, fill = z)) + ggplot2::geom_raster()
  attr(p, "divaodv_fill") <- list(palette = "odv", limits = c(0, 2),
                                  name = "PO4", reverse = TRUE)
  p2 <- suppressMessages(p + scale_fill_odv(median = 0.4))
  expect_false(is.null(attr(p2, "divaodv_fill")))
  sc <- suppressMessages(
    ggplot2::ggplot_build(p2 + scale_fill_odv(median = 0.6))
  )$plot$scales$get_scales("fill")
  expect_equal(sc$limits, c(0, 2))
})

test_that("the scale works on a plot divaodv did not produce", {
  d <- expand.grid(x = 1:5, y = 1:5); d$z <- seq(0, 2, length.out = 25)
  q <- ggplot2::ggplot(d, ggplot2::aes(x, y, fill = z)) + ggplot2::geom_raster()
  expect_s3_class(suppressMessages(q + scale_fill_odv(median = 0.4)), "ggplot")
})
