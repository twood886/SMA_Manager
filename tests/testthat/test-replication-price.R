# get_replication_price() is the delta-notional valuation used to weight
# securities for replication and rule limits: |delta| * underlying_price. It
# equals get_price() for equity/fixed income and |delta| * underlying for
# options, so replacing an option with its underlying preserves exposure.

test_that("get_replication_price equals price for non-options", {
  eq <- Security$new(
    "owl us equity",
    description = "Blue Owl Capital", instrument_type = "Equity",
    price = 20, delta = 1
  )
  expect_equal(eq$get_replication_price(), 20)
  expect_equal(eq$get_replication_price(), eq$get_price())
})

test_that("get_replication_price is |delta| * underlying for a call option", {
  underlying <- Security$new(
    "aapl us equity",
    description = "Apple", instrument_type = "Equity", price = 200, delta = 1
  )
  call <- Security$new(
    "aapl call",
    description = "AAPL Call", instrument_type = "Option",
    price = 12.5, delta = 0.55, underlying_security = underlying
  )
  expect_equal(call$get_replication_price(), 0.55 * 200)
})

test_that("get_replication_price stays positive for a negative-delta put", {
  underlying <- Security$new(
    "owl us equity",
    description = "Blue Owl", instrument_type = "Equity", price = 20, delta = 1
  )
  put <- Security$new(
    "owl 2029 p8.5 otc",
    description = "BLUE OWL 2029-01-19 8.50 Put OTC",
    instrument_type = "OTC Option",
    price = 8, delta = -0.40, underlying_security = underlying
  )
  # abs() keeps the weight-price positive though the signed delta_price is not
  expect_equal(put$get_replication_price(), 0.40 * 20)
  expect_equal(put$get_delta_price(), -0.40 * 20)
})
