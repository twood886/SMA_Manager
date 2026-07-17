# Securities hydrated from the database carry Enfusion instrument types
# ("Listed Option", "OTC Option") rather than the provider's "Option".
# All option behavior (delta, underlying resolution) must fire for every
# option vocabulary, not just "Option" (see .is_option_type).

make_enfusion_provider <- function() {
  provider <- StaticDataProvider$new()
  provider$add_security(
    "aapl us equity",
    description = "Apple Inc",
    instrument_type = "Equity",
    price = 200
  )
  provider$add_security(
    "aapl 12/18/26 c210 equity",
    description = "AAPL Call Dec26 210",
    instrument_type = "Listed Option",
    price = 12.5,
    delta = 0.55,
    underlying_id = "AAPL US"
  )
  provider$add_security(
    "aapl 12/18/26 p150 equity",
    description = "AAPL OTC Put Dec26 150",
    instrument_type = "OTC Option",
    price = 4.5,
    delta = -0.25,
    underlying_id = "AAPL US"
  )
  provider
}

with_enfusion_provider <- function(env = parent.frame()) {
  pkg_state <- asNamespace("SMAManager")$.pkg_state
  old_provider <- pkg_state$security_data_provider
  provider <- make_enfusion_provider()
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

test_that(".is_option_type accepts every option vocabulary", {
  expect_true(SMAManager:::.is_option_type("Option"))
  expect_true(SMAManager:::.is_option_type("Listed Option"))
  expect_true(SMAManager:::.is_option_type("OTC Option"))
  expect_false(SMAManager:::.is_option_type("Equity"))
  expect_false(SMAManager:::.is_option_type("Bond"))
  expect_false(SMAManager:::.is_option_type(NULL))
  expect_false(SMAManager:::.is_option_type(NA_character_))
})

test_that("'Listed Option' securities keep a supplied underlying", {
  with_enfusion_provider()
  underlying <- .security("AAPL US Equity")
  opt <- Security$new(
    "aapl 12/18/26 c210 equity",
    description = "AAPL Call Dec26 210",
    instrument_type = "Listed Option",
    price = 12.5,
    delta = 0.55,
    underlying_security = underlying
  )
  expect_identical(opt$get_underlying_security(), underlying)
  expect_equal(opt$get_underlying_price(), 200)
  expect_equal(opt$get_delta_price(), 0.55 * 200)
})

test_that("'Listed Option' securities resolve their underlying and delta", {
  with_enfusion_provider()
  opt <- .security("AAPL 12/18/26 C210 Equity")
  expect_equal(opt$get_instrument_type(), "Listed Option")
  expect_equal(opt$get_delta(), 0.55)
  expect_equal(opt$get_underlying_security()$get_id(), "aapl us equity")
  expect_equal(opt$get_delta_price(), 0.55 * 200)
})

test_that("'OTC Option' securities resolve their underlying and delta", {
  with_enfusion_provider()
  opt <- .security("AAPL 12/18/26 P150 Equity")
  expect_equal(opt$get_instrument_type(), "OTC Option")
  expect_equal(opt$get_delta(), -0.25)
  expect_equal(opt$get_underlying_security()$get_id(), "aapl us equity")
  expect_equal(opt$get_delta_price(), -0.25 * 200)
})

test_that("update_security_data refreshes deltas for Enfusion option types", {
  provider <- with_enfusion_provider()
  opt <- .security("AAPL 12/18/26 C210 Equity")

  # Placeholder state, as left by load_securities_from_db()
  opt$set_price(1)
  opt$set_delta(1)

  update_security_data(update_fields = FALSE)
  expect_equal(opt$get_price(), 12.5)
  expect_equal(opt$get_delta(), 0.55)
})
