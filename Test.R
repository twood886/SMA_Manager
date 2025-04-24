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




cube_sma$add_replacement("et us equity", "xom us equity")
cube_sma$add_replacement("et us equity", "")
cube_sma$add_replacement("gel us equity", "xom us equity")