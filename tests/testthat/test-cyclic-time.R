# Cyclic-time helpers for annual climatologies. All tests here are pure R and
# need no Julia session. The end-to-end "no Dec/Jan seam" check is a manual
# integration test (needs Julia + DIVAnd + a folded dataset) — see
# docs/plans/2026-07-26-cyclic-time-climatology-design.md, section 10.

test_that(".diva_cyclic_time_grid spans [0,1) with no duplicated endpoint", {
  g <- divaodv:::.diva_cyclic_time_grid(120)
  expect_length(g, 120)
  expect_equal(g[1], 0)
  expect_true(max(g) < 1)                          # wrap point excluded
  expect_equal(g[length(g)], 119 / 120)            # last point = (n-1)/n
  expect_equal(unique(round(diff(g), 12)), 1 / 120)  # uniform spacing = 1/n
})

test_that(".diva_cyclic_moddim is [0, n_time] (depth non-cyclic, time cyclic)", {
  expect_identical(divaodv:::.diva_cyclic_moddim(120L), c(0L, 120L))
  expect_type(divaodv:::.diva_cyclic_moddim(52L), "integer")
})

test_that(".diva_assert_folded warns only when the span exceeds one year", {
  y <- as.Date("2001-01-01")
  expect_silent(divaodv:::.diva_assert_folded(y, as.Date("2001-12-31")))
  expect_warning(
    divaodv:::.diva_assert_folded(as.Date("2004-01-01"), as.Date("2014-06-01")),
    "folded onto one reference year"
  )
})

test_that("cyclic spacing (1/n) differs from the linear-edge grid (1/(n-1))", {
  n      <- 100L
  cyclic <- divaodv:::.diva_cyclic_time_grid(n)
  linear <- seq(0, 1, length.out = n)              # LinRange(0, 1, n)
  expect_equal(diff(cyclic)[1], 1 / n)
  expect_equal(diff(linear)[1], 1 / (n - 1))
  expect_false(isTRUE(all.equal(diff(cyclic)[1], diff(linear)[1])))
})
