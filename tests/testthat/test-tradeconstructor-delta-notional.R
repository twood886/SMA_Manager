# End-to-end: the trade constructor weighs options by their delta-notional
# (|delta| * underlying price), not their premium. Under a GMV-relative rule the
# per-security limit depends on that valuation (via gamma = |price| / nav), so an
# equity and an option with the SAME delta-notional get the SAME limit — which
# would not hold if the option were weighed by its (different) premium.

# Swap in a provider and give the test a clean securities registry, restoring
# both (and any portfolios it registers) afterwards.
with_clean_registry <- function(provider, env = parent.frame()) {
  pkg_state <- asNamespace("replikit")$.pkg_state
  old_provider <- pkg_state$security_data_provider
  set_security_data_provider(provider)

  sec_reg <- get_registries()$securities
  saved_secs <- mget(ls(sec_reg), envir = sec_reg)
  rm(list = ls(sec_reg), envir = sec_reg)

  port_reg <- get_registries()$portfolios
  ports_before <- ls(port_reg)

  withr::defer(
    {
      pkg_state$security_data_provider <- old_provider
      rm(list = ls(sec_reg), envir = sec_reg)
      list2env(saved_secs, envir = sec_reg)
      new_ports <- setdiff(ls(port_reg), ports_before)
      if (length(new_ports)) rm(list = new_ports, envir = port_reg)
    },
    envir = env
  )
}

test_that("trade constructor weighs options by delta-notional, not premium", {
  provider <- StaticDataProvider$new()
  provider$add_security("u us equity", instrument_type = "Equity", price = 100)
  provider$add_security("a us equity", instrument_type = "Equity", price = 100)
  provider$add_security("e us equity", instrument_type = "Equity", price = 50)
  # Premium 12, but delta-notional = |0.5| * 100 = 50, same as the equity above.
  provider$add_security(
    "o us 12/18/26 c100 equity",
    instrument_type = "Option", price = 12, delta = 0.5, underlying_id = "U US"
  )
  with_clean_registry(provider)

  .security("e us equity")
  .security("o us 12/18/26 c100 equity")

  port <- .portfolio(
    "testbase_dn", "Test Base Delta-Notional",
    nav = 1000, positions = list(), create = TRUE
  )
  # A held anchor so the GMV denominator is non-zero.
  port$add_holding(.holding("a us equity", 1))
  port$add_rule(SMARulePosition$new(
    sma_name = "testbase_dn",
    name = "10pct gmv",
    scope = "position",
    definition = function(ids, nav) rep(1, length(ids)),
    max_threshold = 0.1,
    min_threshold = -0.1,
    relative_to = "gmv"
  ))

  max_e <- port$get_security_position_limits("e us equity")[["e us equity"]]$max
  max_o <- port$get_security_position_limits(
    "o us 12/18/26 c100 equity"
  )[["o us 12/18/26 c100 equity"]]$max

  # Both finite and positive, and equal: the option is valued at its
  # delta-notional (50), identical to the equity — not its premium (12), which
  # would make the two limits differ.
  expect_true(is.finite(max_e) && max_e > 0)
  expect_equal(max_o, max_e, tolerance = 1e-9)
})
