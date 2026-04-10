test_that("SMARulePortfolio get_security_limits works with NAV divisor and includes all securities", {
  rule <- SMARulePortfolio$new(
    sma_name = "TestSMA",
    name = "Test Portfolio Rule NAV",
    scope = "portfolio",
    definition = function(ids, nav) rep(1, length(ids)),
    max_threshold = 1,
    min_threshold = -1,
    relative_to = "nav"
  )
  security_id <- "CDE"
  ids_all <- c("ABC", "FGH", "XYZ", "CDE")
  qty_all <- c(10, -20, 0, 0)
  nav <- 1000
  prices_all <- c(50, 10, 20, 10)
  f_all <- prices_all / nav
  limits <- rule$get_security_limits(
    security_id = security_id,
    ids_all = ids_all,
    qty_all = qty_all,
    nav = nav,
    prices_all = prices_all,
    f_all = f_all
  )
  expect_equal(limits$CDE$max, 70, tolerance = 1e-6)
  expect_equal(limits$CDE$min, -130, tolerance = 1e-6)
})


test_that("SMARulePortfolio get_security_limits works with NAV divisor and includes long securities", {
  rule <- SMARulePortfolio$new(
    sma_name = "TestSMA",
    name = "Test Portfolio Rule NAV (long only)",
    scope = "portfolio",
    definition = function(ids, nav) rep(1, length(ids)),
    max_threshold = 1,
    min_threshold = -1,
    relative_to = "nav",
    include = "long_only"
  )
  security_id <- "CDE"
  ids_all <- c("ABC", "FGH", "XYZ", "CDE")
  qty_all <- c(10, -20, 0, 0)
  nav <- 1000
  prices_all <- c(50, 10, 20, 10)
  f_all <- prices_all / nav
  limits <- rule$get_security_limits(
    security_id = security_id,
    ids_all = ids_all,
    qty_all = qty_all,
    nav = nav,
    prices_all = prices_all,
    f_all = f_all
  )
  expect_equal(limits$CDE$max, 50, tolerance = 1e-6)
  expect_equal(limits$CDE$min, -Inf, tolerance = 1e-6)
})

test_that("SMARulePortfolio get_security_limits works with NAV divisor and includes short securities", {
  rule <- SMARulePortfolio$new(
    sma_name = "TestSMA",
    name = "Test Portfolio Rule NAV (short only)",
    scope = "portfolio",
    definition = function(ids, nav) rep(1, length(ids)),
    max_threshold = 1,
    min_threshold = -1,
    relative_to = "nav",
    include = "short_only"
  )
  security_id <- "CDE"
  ids_all <- c("ABC", "FGH", "XYZ", "CDE")
  qty_all <- c(10, -20, 0, 0)
  nav <- 1000
  prices_all <- c(50, 10, 20, 10)
  f_all <- prices_all / nav
  limits <- rule$get_security_limits(
    security_id = security_id,
    ids_all = ids_all,
    qty_all = qty_all,
    nav = nav,
    prices_all = prices_all,
    f_all = f_all
  )
  expect_equal(limits$CDE$max, Inf, tolerance = 1e-6)
  expect_equal(limits$CDE$min, -80, tolerance = 1e-6)
})