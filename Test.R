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

rebalance_sma(bemap)
rebalance_sma(fmap)

trades <- mget(
  ls(envir = registries$trades, all.names = TRUE),
  envir = registries$trades, 
  inherits = TRUE
)

trade_df <- dplyr::bind_rows(lapply(trades, \(trade) trade$to_df()))
row.names(trade_df) <- NULL