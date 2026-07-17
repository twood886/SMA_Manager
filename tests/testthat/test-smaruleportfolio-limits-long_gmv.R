# SMARulePortfolio$get_security_limits with the long-GMV divisor.
#
# Rule under test: a net-exposure rule (definition f = price / nav, so the
# rule value is sum(f * qty) = net exposure as a fraction of NAV) constrained
# relative to the LONG side gross market value:
#
#   min_t * long_gmv <= sum(f * qty) <= max_t * long_gmv
#   where long_gmv = sum(pmax(qty * price / nav, 0))
#
# Buying a security adds to both the numerator and the divisor
# (gamma_pos = price / nav); shorting only moves the numerator
# (gamma_neg = 0, shorts never contribute to long GMV).
#
# Test portfolio (nav = 1000):
#   ABC:  10 @  50  ->  w =  0.5   (the only long)
#   FGH:  -5 @ 100  ->  w = -0.5
#   XYZ:   0 @  20
#   CDE:   0 @  10  <- limits computed for this one
#
#   net exposure = 0.5 - 0.5 = 0
#   long_gmv     = 0.5
#
# Hand-derived limits for CDE (f_sec = 10/1000 = 0.01), max_t = 0.5,
# min_t = -0.5:
#   Buy q:   0.01*q <= 0.5 * (0.5 + 0.01*q)  =>  0.005*q <= 0.25  =>  q <=  50
#   Short q: 0.01*q >= -0.5 * 0.5            =>  0.01*q  >= -0.25 =>  q >= -25

prices <- c(ABC = 50, FGH = 100, XYZ = 20, CDE = 10)
nav <- 1000
ids_all <- names(prices)
qty_all <- c(10, -5, 0, 0)

make_long_gmv_rule <- function(max_threshold, min_threshold) {
  SMARulePortfolio$new(
    sma_name = "TestSMA",
    name = "Test Portfolio Rule Long GMV",
    scope = "portfolio",
    definition = function(ids, nav) prices[ids] / nav,
    max_threshold = max_threshold,
    min_threshold = min_threshold,
    relative_to = "long_gmv"
  )
}

cde_limits <- function(rule) {
  rule$get_security_limits(
    security_id = "CDE",
    ids_all = ids_all,
    qty_all = qty_all,
    nav = nav,
    prices_all = as.numeric(prices),
    f_all = as.numeric(prices) / nav
  )$CDE
}

test_that("limits match the hand-derived long-GMV boundaries", {
  rule <- make_long_gmv_rule(max_threshold = 0.5, min_threshold = -0.5)
  limits <- cde_limits(rule)
  expect_equal(limits$max, 50, tolerance = 1e-9)
  expect_equal(limits$min, -25, tolerance = 1e-9)
})

test_that("limits are exactly the boundary where check_compliance flips", {
  rule <- make_long_gmv_rule(max_threshold = 0.5, min_threshold = -0.5)
  passes_with_cde_qty <- function(q) {
    rule$check_compliance(
      ids = ids_all,
      qty = c(10, -5, 0, q),
      nav = nav,
      prices = as.numeric(prices)
    )$pass
  }
  expect_true(passes_with_cde_qty(50))
  expect_false(passes_with_cde_qty(51))
  expect_true(passes_with_cde_qty(-25))
  expect_false(passes_with_cde_qty(-26))
})

test_that("an unbounded min threshold gives -Inf, not NaN", {
  # -Inf is the default min_threshold for rules hydrated from the database,
  # and used to produce NaN via -Inf * gamma_neg with gamma_neg = 0.
  rule <- make_long_gmv_rule(max_threshold = 0.5, min_threshold = -Inf)
  limits <- cde_limits(rule)
  expect_equal(limits$max, 50, tolerance = 1e-9)
  expect_identical(limits$min, -Inf)
})

test_that("a cap the rule can never breach gives an infinite max limit", {
  # With f = price / nav and max_threshold = 1, every long dollar adds
  # equally to the numerator and to long_gmv, so no long position can ever
  # breach the cap: the correct limit is +Inf (denominator of the limit
  # formula is exactly zero).
  rule <- make_long_gmv_rule(max_threshold = 1, min_threshold = -0.5)
  limits <- cde_limits(rule)
  expect_identical(limits$max, Inf)
})
