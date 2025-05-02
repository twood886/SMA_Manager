registries <- new.env(parent = emptyenv())
registries$portfolios <- new.env(parent = emptyenv())
registries$securities <- new.env(parent = emptyenv())
registries$smarules <- new.env(parent = emptyenv())
registries$trades <- new.env(parent = emptyenv())

library(SMAManager)
library(tidyverse)
library(enfusion)
library(Rblpapi)


con <- blpConnect()

ccmf_url <- "https://webservices.enfusionsystems.com/mobile/rest/reportservice/exportReport?name=shared%2FTaylor%2FSMA_Mgr_Reports%2FCCMF+Consolidated+Position+Listing+-+Options.ppr" #nolint

ccmf <- create_portfolio_from_enfusion(
  long_name = "Callodine Capital Master Fund",
  short_name = "ccmf",
  enfusion_url = ccmf_url
)

source("smas/bemap.R")
source("smas/fmap.R")

rebalance_sma(bemap)
rebalance_sma(fmap)

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
  dplyr::mutate(across(contains("shares_"), ~replace_na(.x, 0))) %>%
  dplyr::mutate(`quantity` = rowSums(across(contains("shares_")))) %>%
  dplyr::select(
    security_id,
    swap,
    quantity,
    contains("allocation_pct_"),
  )
write.csv(
  out,
  file = "sma_trades.csv",
  row.names = FALSE
)

add_trade("et us equity", "ccmf", -1, T, T)
