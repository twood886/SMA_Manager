test_that("SMARulePosition get_security_limits works with NAV divisor", {
  rule <- SMARulePosition$new(
    sma_name = "TestSMA",
    name = "Test Position Rule NAV",
    scope = "position",
    definition = function(ids, nav) rep(1, length(ids)),
    max_threshold = 0.05,
    min_threshold = -0.05
  )
  security_id <- "CDE"
  ids_all <- c("ABC", "XYZ", "CDE")
  qty_all <- c(1000, 2000, 0)
  nav <- 1000000
  prices_all <- c(50, 100, 20)
  f_all <- c(1, 1, 1)
  limits <- rule$get_security_limits(
    security_id = security_id,
    ids_all = ids_all,
    qty_all = qty_all,
    nav = nav,
    prices_all = prices_all,
    f_all = f_all,
  )
  expect_equal(limits$ABC$max, 1000, tolerance = 1e-6)
  expect_equal(limits$ABC$min, -1000, tolerance = 1e-6)
})
