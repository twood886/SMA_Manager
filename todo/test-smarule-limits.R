# Test file for SMA Rule position and portfolio limit calculations

test_that("SMARulePosition get_security_limits works with NAV divisor", {
  # Create a simple position rule: max 5% NAV per position
  rule <- SMARulePosition$new(
    name = "Test Position Rule",
    rule_def = function(ids, nav = NULL) rep(1, length(ids)),
    divisor = DivisorProvider$new("nav"),
    max_threshold = 0.05,
    min_threshold = -0.05
  )

  # Portfolio: 2 securities, NAV = 1,000,000
  ids_all <- c("ABC", "XYZ")
  qty_all <- c(1000, 2000)  # ABC: 1000 shares @ $50 = $50k (5% NAV)
  nav <- 1000000
  prices_all <- c(50, 100)  # XYZ: 2000 shares @ $100 = $200k (20% NAV)
  f_all <- c(1, 1)  # Rule applies to both
  f_sec <- c(1)     # Getting limits for ABC

  limits <- rule$get_security_limits(
    security_id = "ABC",
    ids_all = ids_all,
    qty_all = qty_all,
    nav = nav,
    prices_all = prices_all,
    f_all = f_all,
    f_sec = f_sec
  )

  # For NAV divisor: qty * f <= max_t * 1
  # So qty <= max_t / f = 0.05 / 1 = 0.05
  # In shares: 0.05 * nav / price = 0.05 * 1000000 / 50 = 1000
  expect_equal(limits$ABC$max, 1000, tolerance = 1e-6)
  expect_equal(limits$ABC$min, -1000, tolerance = 1e-6)
})

test_that("SMARulePosition get_security_limits works with GMV divisor", {
  # Create position rule with GMV divisor: max 10% of GMV per position
  rule <- SMARulePosition$new(
    name = "Test GMV Position Rule",
    rule_def = function(ids, nav = NULL) rep(1, length(ids)),
    divisor = DivisorProvider$new("gmv"),
    max_threshold = 0.10,
    min_threshold = -0.10
  )

  # Portfolio with GMV = 250k (50k + 200k)
  ids_all <- c("ABC", "XYZ")
  qty_all <- c(1000, 2000)
  nav <- 1000000
  prices_all <- c(50, 100)
  f_all <- c(1, 1)
  f_sec <- c(1)

  limits <- rule$get_security_limits(
    security_id = "ABC",
    ids_all = ids_all,
    qty_all = qty_all,
    nav = nav,
    prices_all = prices_all,
    f_all = f_all,
    f_sec = f_sec
  )

  # Current GMV = 250k, ABC contributes 50k
  # GMV excluding ABC = 200k
  # Formula: max = denom_excl / (1/max_t * f - gamma_pos)
  # gamma_pos = |price|/nav = 50/1000000 = 0.00005
  # max = 200000 / (1/0.10 * 1 - 0.00005) = 200000 / (10 - 0.00005)
  # max ≈ 20000.1 shares

  expect_true(is.finite(limits$ABC$max))
  expect_true(limits$ABC$max > 0)
  # Check that it's reasonable (should be around 20000 shares)
  expect_true(limits$ABC$max > 19000 && limits$ABC$max < 21000)
})

test_that("SMARulePortfolio get_security_limits works with NAV divisor", {
  # Create portfolio rule: total tech exposure <= 30% NAV
  rule <- SMARulePortfolio$new(
    name = "Test Portfolio Rule",
    rule_def = function(ids, nav = NULL) {
      # Both securities contribute equally
      rep(1, length(ids))
    },
    divisor = DivisorProvider$new("nav"),
    max_threshold = 0.30,
    min_threshold = -0.30
  )

  ids_all <- c("ABC", "XYZ")
  qty_all <- c(1000, 2000)  # Current: 1000*1 + 2000*1 = 3000 qty units
  nav <- 1000000
  prices_all <- c(50, 100)
  f_all <- c(1, 1)  # Both contribute to numerator
  f_sec <- c(1)

  limits <- rule$get_security_limits(
    security_id = "ABC",
    ids_all = ids_all,
    qty_all = qty_all,
    nav = nav,
    prices_all = prices_all,
    f_all = f_all,
    f_sec = f_sec
  )

  # Current numerator = 1000*1 + 2000*1 = 3000
  # num_excl = 3000 - 1000 = 2000
  # Formula: max = (denom_excl - 1/max_t * num_excl) / (1/max_t * f - gamma_pos)
  # For NAV: denom_excl = 1, gamma_pos = 0
  # max = (1 - 1/0.30 * 2000) / (1/0.30 * 1 - 0)
  # max = (1 - 6666.67) / 3.333 = negative!
  # This means the constraint is already violated by XYZ alone

  expect_true(is.finite(limits$ABC$max))
})

test_that("SMARulePosition handles zero f values correctly", {
  # Rule that doesn't apply to certain securities
  rule <- SMARulePosition$new(
    name = "Test Selective Rule",
    rule_def = function(ids, nav = NULL) {
      ifelse(ids == "ABC", 1, 0)  # Only applies to ABC
    },
    divisor = DivisorProvider$new("nav"),
    max_threshold = 0.05,
    min_threshold = -0.05
  )

  ids_all <- c("ABC", "XYZ")
  qty_all <- c(1000, 2000)
  nav <- 1000000
  prices_all <- c(50, 100)
  f_all <- c(1, 0)  # Rule only applies to ABC
  f_sec <- c(0)     # Getting limits for XYZ (f=0)

  limits <- rule$get_security_limits(
    security_id = "XYZ",
    ids_all = ids_all,
    qty_all = qty_all,
    nav = nav,
    prices_all = prices_all,
    f_all = f_all,
    f_sec = f_sec
  )

  # When f=0, should return infinite limits
  expect_equal(limits$XYZ$max, Inf)
  expect_equal(limits$XYZ$min, -Inf)
})

test_that("SMARulePosition handles gross exposure correctly", {
  # Gross exposure rule: abs(qty * f) <= max_t
  rule <- SMARulePosition$new(
    name = "Test Gross Rule",
    rule_def = function(ids, nav = NULL) rep(1, length(ids)),
    divisor = DivisorProvider$new("nav"),
    max_threshold = 0.05,
    gross_exposure = TRUE
  )

  ids_all <- c("ABC")
  qty_all <- c(1000)
  nav <- 1000000
  prices_all <- c(50)
  f_all <- c(1)
  f_sec <- c(1)

  limits <- rule$get_security_limits(
    security_id = "ABC",
    ids_all = ids_all,
    qty_all = qty_all,
    nav = nav,
    prices_all = prices_all,
    f_all = f_all,
    f_sec = f_sec
  )

  # For gross exposure, min_t = -max_t
  expect_equal(limits$ABC$max, 1000, tolerance = 1e-6)
  expect_equal(limits$ABC$min, -1000, tolerance = 1e-6)
})

test_that("SMARulePortfolio handles multiple securities correctly", {
  # Portfolio rule with multiple securities
  rule <- SMARulePortfolio$new(
    name = "Test Multi-Security",
    rule_def = function(ids, nav = NULL) {
      # Different weights for different securities
      ifelse(ids == "ABC", 1.0, 0.5)
    },
    divisor = DivisorProvider$new("nav"),
    max_threshold = 0.50,
    min_threshold = -0.50
  )

  ids_all <- c("ABC", "XYZ", "DEF")
  qty_all <- c(1000, 2000, 1500)
  nav <- 1000000
  prices_all <- c(50, 100, 75)
  f_all <- c(1.0, 0.5, 0.5)  # ABC weighted higher
  f_sec <- c(1.0)  # Getting limits for ABC

  limits <- rule$get_security_limits(
    security_id = "ABC",
    ids_all = ids_all,
    qty_all = qty_all,
    nav = nav,
    prices_all = prices_all,
    f_all = f_all,
    f_sec = f_sec
  )

  # Current numerator = 1000*1.0 + 2000*0.5 + 1500*0.5 = 1000 + 1000 + 750 = 2750
  expect_true(is.finite(limits$ABC$max))
  expect_true(is.numeric(limits$ABC$max))
})

test_that("SMARulePosition works with long_gmv divisor", {
  rule <- SMARulePosition$new(
    name = "Test Long GMV",
    rule_def = function(ids, nav = NULL) rep(1, length(ids)),
    divisor = DivisorProvider$new("long_gmv"),
    max_threshold = 0.20,
    min_threshold = -0.20
  )

  # Mixed portfolio: long and short
  ids_all <- c("ABC", "XYZ")
  qty_all <- c(1000, -500)  # ABC long, XYZ short
  nav <- 1000000
  prices_all <- c(50, 100)  # Long GMV = 50k (only ABC counts)
  f_all <- c(1, 1)
  f_sec <- c(1)

  limits <- rule$get_security_limits(
    security_id = "ABC",
    ids_all = ids_all,
    qty_all = qty_all,
    nav = nav,
    prices_all = prices_all,
    f_all = f_all,
    f_sec = f_sec
  )

  # gamma_pos = price/nav, gamma_neg = 0 for long_gmv
  expect_true(is.finite(limits$ABC$max))
  expect_true(is.numeric(limits$ABC$max))
})

test_that("check_compliance works correctly for position rules", {
  rule <- SMARulePosition$new(
    name = "Position Limit",
    rule_def = function(ids, nav = NULL) rep(1, length(ids)),
    divisor = DivisorProvider$new("nav"),
    max_threshold = 0.05,  # 5% max
    min_threshold = -0.05
  )

  # Compliant portfolio
  result1 <- rule$check_compliance(
    ids = c("ABC", "XYZ"),
    qty = c(1000, 800),  # 5% and 4% of NAV
    nav = 1000000,
    prices = c(50, 50),
    f_all = c(1, 1)
  )
  expect_true(result1$pass)

  # Non-compliant portfolio (ABC too large)
  result2 <- rule$check_compliance(
    ids = c("ABC", "XYZ"),
    qty = c(2000, 800),  # 10% and 4% of NAV
    nav = 1000000,
    prices = c(50, 50),
    f_all = c(1, 1)
  )
  expect_false(result2$pass)
  expect_true("ABC" %in% result2$non_comply)
})

test_that("check_compliance works correctly for portfolio rules", {
  rule <- SMARulePortfolio$new(
    name = "Aggregate Limit",
    rule_def = function(ids, nav = NULL) rep(1, length(ids)),
    divisor = DivisorProvider$new("nav"),
    max_threshold = 0.30,  # 30% max aggregate
    min_threshold = -0.30
  )

  # Compliant portfolio (total = 25%)
  result1 <- rule$check_compliance(
    ids = c("ABC", "XYZ"),
    qty = c(1000, 2000),  # 1000 + 2000 = 3000 qty units
    nav = 1000000,
    prices = c(50, 100),  # 5% + 20% = 25%
    f_all = c(1, 1)
  )
  expect_true(result1$pass)

  # Non-compliant portfolio (total = 35%)
  result2 <- rule$check_compliance(
    ids = c("ABC", "XYZ"),
    qty = c(1000, 3000),
    nav = 1000000,
    prices = c(50, 100),  # 5% + 30% = 35%
    f_all = c(1, 1)
  )
  expect_false(result2$pass)
  expect_true(result2$violates_max)
})

test_that("DivisorProvider gamma method returns correct values", {
  nav <- 1000000
  price <- 50

  # Test NAV divisor
  d_nav <- DivisorProvider$new("nav")
  gamma_nav <- d_nav$gamma(price, nav)
  expect_equal(gamma_nav$gamma_pos, 0)
  expect_equal(gamma_nav$gamma_neg, 0)

  # Test GMV divisor
  d_gmv <- DivisorProvider$new("gmv")
  gamma_gmv <- d_gmv$gamma(price, nav)
  expect_equal(gamma_gmv$gamma_pos, abs(price) / nav)
  expect_equal(gamma_gmv$gamma_neg, abs(price) / nav)

  # Test Long GMV divisor
  d_long <- DivisorProvider$new("long_gmv")
  gamma_long <- d_long$gamma(price, nav)
  expect_equal(gamma_long$gamma_pos, price / nav)
  expect_equal(gamma_long$gamma_neg, 0)

  # Test Short GMV divisor
  d_short <- DivisorProvider$new("short_gmv")
  gamma_short <- d_short$gamma(price, nav)
  expect_equal(gamma_short$gamma_pos, 0)
  expect_equal(gamma_short$gamma_neg, price / nav)
})

test_that("Position limits handle edge cases", {
  rule <- SMARulePosition$new(
    name = "Edge Case Rule",
    rule_def = function(ids, nav = NULL) rep(1, length(ids)),
    divisor = DivisorProvider$new("nav"),
    max_threshold = 0.05,
    min_threshold = -0.05
  )

  # Test with security not in portfolio (new position)
  limits <- rule$get_security_limits(
    security_id = "NEW",
    ids_all = c("ABC", "XYZ"),
    qty_all = c(1000, 2000),
    nav = 1000000,
    prices_all = c(50, 100),
    f_all = c(1, 1),
    f_sec = c(1)
  )

  # Should still get finite limits
  expect_true(is.finite(limits$NEW$max))
  expect_true(is.finite(limits$NEW$min))
})

test_that("Portfolio limits handle edge cases with all securities excluded", {
  rule <- SMARulePortfolio$new(
    name = "Selective Portfolio Rule",
    rule_def = function(ids, nav = NULL) {
      ifelse(ids %in% c("ABC", "XYZ"), 1, 0)
    },
    divisor = DivisorProvider$new("nav"),
    max_threshold = 0.50,
    min_threshold = -0.50
  )

  # Get limits for security that doesn't contribute
  limits <- rule$get_security_limits(
    security_id = "OTHER",
    ids_all = c("ABC", "XYZ", "OTHER"),
    qty_all = c(1000, 2000, 500),
    nav = 1000000,
    prices_all = c(50, 100, 75),
    f_all = c(1, 1, 0),
    f_sec = c(0)
  )

  # Should return infinite limits since f=0
  expect_equal(limits$OTHER$max, Inf)
  expect_equal(limits$OTHER$min, -Inf)
})
