test_that("SMARulePortfolio get_security_limits works with long GMV divisor", {
  rule <- SMARulePortfolio$new(
    sma_name = "TestSMA",
    name = "Test Portfolio Rule Long GMV",
    scope = "portfolio",
    definition = function(ids, nav) rep(1, length(ids)),
    max_threshold = 1,
    min_threshold = -Inf,
    relative_to = "long_gmv"
  )
  security_id <- "CDE"
  ids_all <- c("ABC", "FGH", "XYZ", "CDE")
  qty_all <- c(10, -20, 0, 0)
  nav <- 1000
  prices_all <- c(50, 100, 20, 10)
  f_all <- prices_all / nav
  limits <- rule$get_security_limits(
    security_id = security_id,
    ids_all = ids_all,
    qty_all = qty_all,
    nav = nav,
    prices_all = prices_all,
    f_all = f_all
  )
  expect_equal(limits$CDE$max, 2500, tolerance = 1e-6)
  expect_equal(limits$CDE$min, -2000, tolerance = 1e-6)
})