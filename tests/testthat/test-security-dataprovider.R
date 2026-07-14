make_static_provider <- function() {
  provider <- StaticDataProvider$new()
  provider$add_security(
    "aapl us equity",
    description = "Apple Inc",
    instrument_type = "Equity",
    price = 200,
    fields = list(GICS_SECTOR_NAME = "Information Technology")
  )
  provider$add_security(
    "aapl 12/18/26 c210 equity",
    description = "AAPL Call Dec26 210",
    instrument_type = "Option",
    price = 12.5,
    delta = 0.55,
    underlying_id = "AAPL US"
  )
  provider
}

# Installs the static provider and empties the securities registry for the
# duration of the calling test.
with_static_provider <- function(env = parent.frame()) {
  pkg_state <- asNamespace("SMAManager")$.pkg_state
  old_provider <- pkg_state$security_data_provider
  provider <- make_static_provider()
  set_security_data_provider(provider)

  reg <- get_registries()$securities
  rm(list = ls(reg), envir = reg)

  withr::defer(
    {
      pkg_state$security_data_provider <- old_provider
      rm(list = ls(reg), envir = reg)
    },
    envir = env
  )
  provider
}

test_that("Security builds from a static provider without Bloomberg", {
  with_static_provider()
  sec <- Security$new("aapl us equity")
  expect_equal(sec$get_id(), "aapl us equity")
  expect_equal(sec$get_description(), "Apple Inc")
  expect_equal(sec$get_instrument_type(), "Equity")
  expect_equal(sec$get_price(), 200)
  expect_equal(sec$get_delta(), 1) # non-options default to delta 1
  expect_identical(sec$get_underlying_security(), sec)
})

test_that("Option securities resolve their underlying via the provider", {
  with_static_provider()
  opt <- .security("AAPL 12/18/26 C210 Equity")
  expect_equal(opt$get_instrument_type(), "Option")
  expect_equal(opt$get_delta(), 0.55)
  underlying <- opt$get_underlying_security()
  expect_equal(underlying$get_id(), "aapl us equity")
  expect_equal(opt$get_underlying_price(), 200)
  expect_equal(opt$get_delta_price(), 0.55 * 200)
})

test_that(".security registers, retrieves, and rejects unknown securities", {
  with_static_provider()
  sec <- .security("AAPL US Equity")
  reg <- get_registries()$securities
  expect_true(exists("aapl us equity", envir = reg, inherits = FALSE))
  expect_identical(.security("aapl us equity"), sec)
  expect_null(.security("msft us equity", create = FALSE))
  expect_error(.security("msft us equity"), "Security not found")
})

test_that("update_price pulls the latest price from the provider", {
  provider <- with_static_provider()
  sec <- .security("aapl us equity")
  expect_equal(sec$get_price(), 200)
  provider$set_price("aapl us equity", 210)
  sec$update_price()
  expect_equal(sec$get_price(), 210)
})

test_that("update_security_data refreshes prices and option deltas together", {
  provider <- with_static_provider()
  eq <- .security("aapl us equity")
  opt <- .security("aapl 12/18/26 c210 equity")
  expect_equal(eq$get_price(), 200)
  expect_equal(opt$get_delta(), 0.55)

  # Move the market: new prices and a new option delta
  provider$add_security(
    "aapl us equity",
    description = "Apple Inc", instrument_type = "Equity", price = 210
  )
  provider$add_security(
    "aapl 12/18/26 c210 equity",
    description = "AAPL Call Dec26 210", instrument_type = "Option",
    price = 14, delta = 0.60, underlying_id = "AAPL US"
  )

  update_security_data()

  expect_equal(eq$get_price(), 210)
  expect_equal(eq$get_delta(), 1) # non-options stay at delta 1
  expect_equal(opt$get_price(), 14)
  expect_equal(opt$get_delta(), 0.60) # option delta must NOT be reset to 1
  expect_equal(opt$get_underlying_price(), 210)
  expect_equal(opt$get_delta_price(), 0.60 * 210)
})

test_that("StaticDataProvider serves PX_LAST and OP006 through get_fields", {
  provider <- make_static_provider()
  fields <- provider$get_fields(
    c("aapl us equity", "aapl 12/18/26 c210 equity"),
    c("PX_LAST", "OP006", "GICS_SECTOR_NAME")
  )
  expect_equal(fields[["aapl us equity", "PX_LAST"]], 200)
  expect_equal(fields[["aapl 12/18/26 c210 equity", "OP006"]], 0.55)
  expect_equal(
    fields[["aapl us equity", "GICS_SECTOR_NAME"]], "Information Technology"
  )
})

test_that("StaticDataProvider vectorized accessors match the bdp contract", {
  provider <- make_static_provider()
  ids <- c("aapl us equity", "aapl 12/18/26 c210 equity")
  prices <- provider$get_prices(ids)
  expect_equal(unname(prices), c(200, 12.5))
  expect_equal(names(prices), ids)
  fields <- provider$get_fields(ids, "GICS_SECTOR_NAME")
  expect_equal(rownames(fields), ids)
  expect_equal(fields["aapl us equity", "GICS_SECTOR_NAME"], "Information Technology")
  expect_true(is.na(fields["aapl 12/18/26 c210 equity", "GICS_SECTOR_NAME"]))
})
