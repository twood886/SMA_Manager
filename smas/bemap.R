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
  definition = function(security_id, sma) {
    1 / Rblpapi::bdp(security_id, "DS381")$DS381
  },
  max_threshold = 0.0499,
  min_threshold = -Inf
)

create_smarule(
  sma_name = "bemap",
  rule_name = "no mlps except on swap",
  scope = "position",
  definition = function(security_id, sma) {
    Rblpapi::bdp(security_id, "DS213")$DS213 == "MLP"
  },
  swap_only = TRUE
)

create_smarule(
  sma_name = "bemap",
  rule_name = "no partnerships except on swap",
  scope = "position",
  definition = function(security_id, sma) {
    Rblpapi::bdp(security_id, "DS674")$DS674 == "Partnership Shares"
  },
  swap_only = TRUE
)

create_smarule(
  sma_name = "bemap",
  rule_name = "no etps except on swap",
  scope = "position",
  definition = function(security_id, sma) {
    Rblpapi::bdp(security_id, "DS213")$DS213 == "ETP"
  },
  swap_only = TRUE
)

create_smarule(
  sma_name = "bemap",
  rule_name = "no bdcs except on swap",
  scope = "position",
  definition = function(security_id, sma) {
    Rblpapi::bdp(security_id, "DS213")$DS213 == "ETN"
  },
  swap_only = TRUE
)



create_smarule(
  sma_name = "bemap",
  rule_name = "only US securities",
  scope = "position",
  definition = function(security_id, sma) {
    Rblpapi::bdp(security_id, "DS290")$DS290 != "US"
  },
  max_threshold = 0,
  min_threshold = 0
)

create_smarule(
  sma_name = "bemap",
  rule_name = "liquidity",
  scope = "position",
  definition = function(security_id, sma) {
    1 / Rblpapi::bdp(security_id, "HS020")$HS020
  },
  max_threshold = 1.83,
  min_threshold = -1.83
)

create_smarule(
  sma_name = "bemap",
  rule_name = "max position size 5%",
  scope = "position",
  definition = function(security_id, sma) {
    price <- Rblpapi::bdp(security_id, "PX_LAST")$PX_LAST
    price / sma$get_nav()
  },
  max_threshold = 0.05,
  min_threshold = -0.05
)
