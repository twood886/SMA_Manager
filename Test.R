library(SMAManager)
library(tidyverse)
library(enfusion)
library(Rblpapi)

con <- blpConnect()

ccmf <- create_portfolio_from_enfusion(
  long_name = "Callodine Capital Master Fund",
  short_name = "ccmf",
  enfusion_url = paste0(
    "https://webservices.enfusionsystems.com/mobile/",
    "rest/reportservice/exportReport?",
    "name=shared%2FTaylor%2FSMA_Mgr_Reports%2F",
    "CCMF+Consolidated+Position+Listing+-+Options.ppr"
  )
)

source("smas/bemap.R")
source("smas/fmap.R")


test <- create_proposed_trade_qty(
  portfolio_id = "ccmf",
  security_id = "isrg us equity",
  trade_qty = -5000,
  swap = FALSE,
  flow_to_derived = TRUE
)


trades <- create_trade_qty(
  portfolio_id = "ccmf",
  security_id = "zroz us equity",
  trade_qty = 10000,
  swap = FALSE
)

bemap$mimic_base_portfolio()
fmap$mimic_base_portfolio()

trades <- mget(
  ls(envir = registries$trades, all.names = TRUE),
  envir = registries$trades,
  inherits = TRUE
)

trade_df <- dplyr::bind_rows(lapply(trades, \(trade) trade$to_df()))
row.names(trade_df) <- NULL


out <- trade_df %>%
  dplyr::group_by(security_id, swap) %>%
  tidyr::pivot_wider(
    names_from = portfolio_short_name,
    values_from = c(shares, allocation_pct)
  ) %>%
  dplyr::mutate(across(contains("shares_"), ~replace_na(.x, 0)))%>%
  dplyr::mutate(`quantity` = rowSums(across(contains("shares_")))) %>%
  dplyr::select(
    security_id,
    swap,
    quantity,
    contains("allocation_pct_"),
  )


write.csv(
  out,
  file = "20250509_sma_trades.csv",
  row.names = FALSE
)


rules <- bemap$get_sma_rules()

securities <- sapply(ccmf$get_position(), \(pos) pos$get_id())
test <- get_security_position_limits(securities, rules)

base_pos <- 
  .position(
    short_name = "ccmf",
    id = "AAPL US Equity",
    qty = 1000,
    swap = FALSE
  )

base_portfolio <- ccmf
sma_portfolio <- bemap
security_id <- "et us equity"

base_positions <- ccmf$get_target_position()
sma <- bemap

