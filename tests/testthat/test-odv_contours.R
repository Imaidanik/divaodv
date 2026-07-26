make_plot <- function() {
  d <- expand.grid(Date = seq(as.Date("2010-01-01"), by = "7 days",
                              length.out = 40),
                   Depth = 0:40)
  d$Temp <- 21 + 5 / (1 + exp((d$Depth - 20) / 8))
  p <- ggplot2::ggplot(d, ggplot2::aes(Date, Depth)) +
    ggplot2::geom_raster(ggplot2::aes(fill = .data[["Temp"]])) +
    ggplot2::geom_point(data = head(d, 10),
                        ggplot2::aes(Date, Depth), inherit.aes = FALSE)
  attr(p, "divaodv_fill") <- list(palette = "odv", limits = c(18, 28),
                                  name = "T", reverse = TRUE, var = "Temp")
  p
}
n_contour <- function(p) sum(vapply(p$layers, .odv_is_contour_layer, logical(1)))

test_that("odv_contours() adds a line layer and a label layer", {
  p <- make_plot() + odv_contours(breaks = c(20, 22, 24))
  expect_equal(n_contour(p), 2)
  expect_equal(length(p$layers), 4)
})

test_that("explicit breaks reach the layer", {
  p <- make_plot() + odv_contours(breaks = c(20, 22, 24))
  expect_equal(p$layers[[3]]$stat_params$breaks, c(20, 22, 24))
})

test_that("binwidth is used when breaks is NULL", {
  p <- make_plot() + odv_contours(binwidth = 2)
  expect_equal(p$layers[[3]]$stat_params$binwidth, 2)
})

test_that("labels = FALSE draws lines only", {
  p <- make_plot() + odv_contours(breaks = 22, labels = FALSE)
  expect_equal(n_contour(p), 1)
})

test_that("contours are replaced, not accumulated", {
  p <- make_plot() + odv_contours(binwidth = 1) + odv_contours(breaks = c(21, 23))
  expect_equal(n_contour(p), 2)
  expect_equal(p$layers[[3]]$stat_params$breaks, c(21, 23))
})

test_that("odv_contours_remove() strips contours and keeps everything else", {
  p <- make_plot() + odv_contours(breaks = 22)
  q <- p + odv_contours_remove()
  expect_equal(n_contour(q), 0)
  expect_equal(length(q$layers), 2)
  expect_setequal(vapply(q$layers, function(l) class(l$geom)[1], character(1)),
                  c("GeomRaster", "GeomPoint"))
})

test_that("removing contours from a plot with none is a no-op", {
  p <- make_plot()
  expect_equal(length((p + odv_contours_remove())$layers), 2)
})

test_that("the divaodv_fill attribute survives both operations", {
  p <- make_plot() + odv_contours(breaks = 22)
  expect_false(is.null(attr(p, "divaodv_fill")))
  expect_false(is.null(attr(p + odv_contours_remove(), "divaodv_fill")))
})

test_that("scale_fill_odv() still inherits after contours are added", {
  p  <- make_plot() + odv_contours(breaks = 22)
  sc <- suppressMessages(
    ggplot2::ggplot_build(p + scale_fill_odv(median = 0.4))
  )$plot$scales$get_scales("fill")
  expect_equal(sc$limits, c(18, 28))
})

test_that("the variable is found without the divaodv_fill attribute", {
  d <- expand.grid(x = 1:10, y = 1:10); d$Temp <- seq(0, 2, length.out = 100)
  bare <- ggplot2::ggplot(d, ggplot2::aes(x, y, fill = Temp)) +
    ggplot2::geom_raster()
  expect_equal(n_contour(bare + odv_contours(breaks = 1)), 2)

  dotdata <- ggplot2::ggplot(d, ggplot2::aes(x, y)) +
    ggplot2::geom_raster(ggplot2::aes(fill = .data[["Temp"]]))
  expect_equal(n_contour(dotdata + odv_contours(breaks = 1)), 2)
})

test_that("a plot with no fill mapping gives a clear error", {
  d <- data.frame(x = 1:10, y = 1:10)
  p <- ggplot2::ggplot(d, ggplot2::aes(x, y)) + ggplot2::geom_point()
  expect_error(p + odv_contours(), "which variable to contour")
})

test_that("arguments are validated", {
  expect_error(odv_contours(breaks = "a"), "numeric vector")
  expect_error(odv_contours(binwidth = -1), "positive number")
  expect_error(odv_contours(labels = "yes"), "TRUE or FALSE")
  expect_warning(odv_contours(breaks = 1, binwidth = 1), "ignored")
})
