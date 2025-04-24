library(SMAManager)
library(Rblpapi)
con <- blpConnect()
bemap <- create_sma(
  long_name = "BEMAP SMA",
  short_name = "bemap",
  nav = 100000000,
  target_portfolio = "ccmf"
)

# Create bemap rules
create_smarule(
  sma_name = "bemap",
  rule_name = "position under 4.99% shares outstanding",
  scope = "position",
  definition = function(security_id) {
    1 / Rblpapi::bdp(security_id, "DS381")$DS381
  },
  max_threshold = 0.0499,
  min_threshold = -Inf
)

create_smarule(
  sma_name = "bemap",
  rule_name = "no mlp",
  scope = "position",
  definition = function(security_id) {
    Rblpapi::bdp(security_id, "DS213")$DS213 == "MLP"
  },
  max_threshold = 0,
  min_threshold = 0
)

create_smarule(
  sma_name = "bemap",
  rule_name = "no partnerships",
  scope = "position",
  definition = function(security_id) {
    Rblpapi::bdp(secruity_id, "DS674")$DS674 == "Partnership Shares"
  },
  max_threshold = 0,
  min_threshold = 0
)

create_smarule(
  sma_name = "bemap",
  rule_name = "only US",
  scope = "position",
  definition = function(security_id) {
    Rblpapi::bdp(security_id, "DS290")$DS290 == "US"
  },
  max_threshold = 0,
  min_threshold = 0
)

create_smarule(
  sma_name = "bemap",
  rule_name = "liquidity",
  scope = "position",
  definition = function(security_id) {
    1 / Rblpapi::bdp(position$get_id(), "HS020")$HS020
  },
  max_threshold = 1.83,
  min_threshold = -1.83
)