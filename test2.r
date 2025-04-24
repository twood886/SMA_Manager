library(SMAManager)
library(Rblpapi)
con <- blpConnect()

# Create Test Portfolio
test_ptfl <- SMAManager:::create_portfolio(
  long_name = "Test Portfolio",
  short_name = "test_ptfl",
  nav = 1000000
)
et <- SMAManager::create_position(
  portfolio_short_name = "test_ptfl",
  id = "et us equity",
  qty = 4000
)
test_ptfl$add_position(et)
xom <- SMAManager::create_position(
  portfolio_short_name = "test_ptfl",
  id = "xom us equity",
  qty = 277
)
test_ptfl$add_position(xom)

# Crerate Test SMA
test_sma <- SMAManager::create_sma(
  long_name = "Test SMA",
  short_name = "test_sma",
  nav = 100000,
  target_portfolio = "test_ptfl"
)
# Create Rule of Max Position of 5%
test_rule <- create_smarule(
  sma_name = "test_sma",
  rule_name = "position under 4.99% shares outstanding",
  scope = "position",
  definition = function(position) {
    security <- position$get_security()
    shares_out <- Rblpapi::bdp(position$get_id(), "DS381")$DS381
    qty <- position$get_qty()
    qty / shares_outstanding
  },
  threshold = function(x) x < 0.0499
)
