# tests/testthat/test-scale_fill_odv_auto.R
# Auto ODV colour-mapping fit (odv_auto_map + "auto" sentinel).

test_that("odv_auto_map places the palette midpoint at the data median", {
  set.seed(1)
  x    <- rlnorm(2000, log(0.15), 0.9)
  lims <- c(0, unname(quantile(x, 0.995)))
  fit  <- odv_auto_map(x, lims)

  expect_named(fit, c("median", "nonlinearity"))
  u_med <- (median(x) - lims[1]) / (lims[2] - lims[1])
  expect_equal(unname(fit[["median"]]),
               min(max(u_med, 0.01), 0.99), tolerance = 1e-8)
  expect_gte(fit[["nonlinearity"]], 0)
  expect_lte(fit[["nonlinearity"]], 1)
})

test_that("cdf fit tracks the empirical CDF better than a linear map", {
  set.seed(2)
  x    <- rlnorm(3000, log(0.15), 0.9)
  lims <- c(0, unname(quantile(x, 0.995)))
  fit  <- odv_auto_map(x, lims, method = "cdf")

  u  <- pmin(pmax((sort(x) - lims[1]) / diff(lims), 0), 1)
  Fs <- (seq_along(u) - 0.5) / length(u)
  sse_fit <- mean((divaodv:::.odv_transfer(u, fit[["median"]],
                                           fit[["nonlinearity"]]) - Fs)^2)
  sse_lin <- mean((divaodv:::.odv_transfer(u, fit[["median"]], 0) - Fs)^2)
  expect_lt(sse_fit, sse_lin)
})

test_that('the "auto" sentinel constructs and prints', {
  s <- scale_fill_odv(median = "auto", nonlinearity = "auto")
  expect_s3_class(s, "divaodv_deferred_scale")
  expect_output(print(s), "median=auto")
  expect_output(print(s), "nonlinearity=auto")
})

test_that("odv_auto_map validates its inputs", {
  expect_error(odv_auto_map(1, c(0, 1)))        # < 2 finite values
  expect_error(odv_auto_map(1:10, c(1, 1)))     # degenerate limits
  expect_error(scale_fill_odv(median = "nope")) # bad sentinel
})
