library(SMAManager)
library(Rblpapi)

con <- blpConnect()

ccmf_url <- "https://webservices.enfusionsystems.com/mobile/rest/reportservice/exportReport?name=shared%2FTaylor%2FSMA_Mgr_Reports%2FCCMF+Consolidated+Position+Listing+-+Options.ppr" #nolint

ccmf <- create_portfolio_from_enfusion(
  long_name = "Callodine Capital Master Fund",
  short_name = "ccmf",
  enfusion_url = ccmf_url
)

source("smas/bemap.R")
rebalance_sma(bemap)

pos <- bemap$get_target_position()
rules <- bemap$get_sma_rules()

test <- lapply(bemap$get_sma_rules(), function(rule) rule$check_rule_target())


unlist(test)

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

test <- create_position("bemap", "aapl us equity", 0)
t1 <- Sys.time()
test <- create_position("bemap", "aapl us equity", 0)
t2 <- Sys.time()
a <- t2 - t1


t3 <- Sys.time()
test2 <- test$clone(deep = TRUE)
test2$set_qty(0)
t4 <- Sys.time()
b <- t4 - t3