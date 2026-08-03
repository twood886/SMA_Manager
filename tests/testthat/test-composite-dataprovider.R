# CompositeDataProvider serves manually-registered OTC options locally (Enfusion
# delta + underlying-derived price) and delegates everything else to a primary
# provider, splitting batched requests that mix both.

make_primary <- function() {
  p <- StaticDataProvider$new()
  p$add_security(
    "owl us equity",
    description = "Blue Owl Capital", instrument_type = "Equity", price = 20
  )
  p$add_security(
    "aapl us equity",
    description = "Apple Inc", instrument_type = "Equity", price = 200,
    fields = list(GICS_SECTOR_NAME = "Information Technology")
  )
  p
}

make_composite <- function(primary = make_primary()) {
  comp <- CompositeDataProvider$new(primary = primary)
  comp$register_otc_option(
    sec_id = "owl 2029 p8.5 otc",
    underlying_id = "owl us equity",
    delta = -0.40, # a put: negative delta
    description = "BLUE OWL CAPITAL 2029-01-19 8.50 Put OTC"
  )
  comp
}

test_that("CompositeDataProvider delegates unknown securities to primary", {
  comp <- make_composite()
  expect_true(comp$security_exists("owl us equity"))
  expect_true(comp$security_exists("owl 2029 p8.5 otc"))
  expect_false(comp$security_exists("msft us equity"))
  expect_equal(comp$get_instrument_type("owl us equity"), "Equity")
  expect_equal(comp$get_price("owl us equity"), 20)
})

test_that("CompositeDataProvider serves OTC override delta and derived price", {
  comp <- make_composite()
  expect_true(comp$is_registered("owl 2029 p8.5 otc"))
  expect_equal(comp$override_ids(), "owl 2029 p8.5 otc")
  expect_equal(comp$get_instrument_type("owl 2029 p8.5 otc"), "OTC Option")
  expect_equal(comp$get_underlying_id("owl 2029 p8.5 otc"), "owl us equity")
  # signed delta preserved
  expect_equal(comp$get_delta("owl 2029 p8.5 otc"), -0.40)
  # price derived = |delta| * underlying price = 0.40 * 20
  expect_equal(comp$get_price("owl 2029 p8.5 otc"), 0.40 * 20)

  prof <- comp$get_security_profile("owl 2029 p8.5 otc")
  expect_equal(prof$instrument_type, "OTC Option")
  expect_equal(prof$delta, -0.40)
  expect_equal(prof$underlying_id, "owl us equity")
  expect_equal(prof$price, 0.40 * 20)
})

test_that("CompositeDataProvider get_fields splits override and primary rows", {
  comp <- make_composite()
  ids <- c("owl 2029 p8.5 otc", "aapl us equity", "owl us equity")
  fields <- comp$get_fields(ids, c("PX_LAST", "OP006"))
  # rows preserved in the requested order
  expect_equal(rownames(fields), ids)
  # OTC row: derived price + signed delta
  expect_equal(fields["owl 2029 p8.5 otc", "PX_LAST"], 0.40 * 20)
  expect_equal(fields["owl 2029 p8.5 otc", "OP006"], -0.40)
  # primary rows delegated
  expect_equal(fields["aapl us equity", "PX_LAST"], 200)
  expect_equal(fields["owl us equity", "PX_LAST"], 20)
})

test_that("CompositeDataProvider get_fields tolerates a primary character field", {
  comp <- make_composite()
  ids <- c("aapl us equity", "owl 2029 p8.5 otc")
  fields <- comp$get_fields(ids, c("PX_LAST", "GICS_SECTOR_NAME"))
  expect_equal(rownames(fields), ids)
  expect_equal(
    fields["aapl us equity", "GICS_SECTOR_NAME"], "Information Technology"
  )
  # override has no such field -> NA, and the request does not error
  expect_true(is.na(fields["owl 2029 p8.5 otc", "GICS_SECTOR_NAME"]))
})

test_that("CompositeDataProvider get_fields works with only override ids", {
  comp <- make_composite()
  fields <- comp$get_fields("owl 2029 p8.5 otc", c("PX_LAST", "OP006"))
  expect_equal(nrow(fields), 1)
  expect_equal(fields["owl 2029 p8.5 otc", "OP006"], -0.40)
  expect_equal(fields["owl 2029 p8.5 otc", "PX_LAST"], 0.40 * 20)
})

test_that("CompositeDataProvider vectorized get_prices/get_deltas split cleanly", {
  comp <- make_composite()
  ids <- c("owl 2029 p8.5 otc", "aapl us equity")
  prices <- comp$get_prices(ids)
  expect_equal(names(prices), ids)
  expect_equal(unname(prices), c(0.40 * 20, 200))
  deltas <- comp$get_deltas(ids)
  expect_equal(unname(deltas[["owl 2029 p8.5 otc"]]), -0.40)
})

test_that("CompositeDataProvider derived price tracks the underlying", {
  primary <- make_primary()
  comp <- make_composite(primary)
  expect_equal(comp$get_price("owl 2029 p8.5 otc"), 0.40 * 20)
  primary$set_price("owl us equity", 25)
  expect_equal(comp$get_price("owl 2029 p8.5 otc"), 0.40 * 25)
})

test_that("a custom derive_price overrides the delta-notional default", {
  comp <- CompositeDataProvider$new(primary = make_primary())
  comp$register_otc_option(
    sec_id = "owl 2029 p8.5 otc",
    underlying_id = "owl us equity",
    delta = -0.40,
    derive_price = function(underlying_price, delta) 4.2 # fixed premium
  )
  expect_equal(comp$get_price("owl 2029 p8.5 otc"), 4.2)
  # delta-notional weighting is unaffected by the price choice
  expect_equal(comp$get_delta("owl 2029 p8.5 otc"), -0.40)
})
