test_that("SMARulePosition get_security_limits works with GMV divisor", {
  rule <- SMARulePosition$new(
    sma_name = "TestSMA",
    name = "Test Position Rule GMV",
    scope = "position",
    definition = function(ids, gmv) rep(1, length(ids)),
    max_threshold = 0.05,
    min_threshold = -0.04,
    relative_to = "gmv"
  )
  security_id <- "CDE"
  ids_all <- c("ABC", "XYZ", "CDE")
  qty_all <- c(-1000, 2000, 0)
  prices_all <- c(50, 100, 20)
  nav <- 1000000
  f_all <- prices_all / nav * .5
  limits <- rule$get_security_limits(
    security_id = security_id,
    ids_all = ids_all,
    qty_all = qty_all,
    prices_all = prices_all,
    f_all = f_all,
    nav = nav
  )
  expect_equal(limits$CDE$max, 1389, tolerance = 1)
  expect_equal(limits$CDE$min, -1087, tolerance = 1)
})
