library(SMAManager)


1+1


ccmf_url <- "https://webservices.enfusionsystems.com/mobile/rest/reportservice/exportReport?name=shared%2FDaily+Reports+Callodine%2FConsolidated+Position+Listing.ppr" #nolint

ccmf <- SMAManager::create_portfolio("Callodine Capital Master Fund", "ccmf", ccmf_url)
?SMAManager::Security


ccmf <- create_portfolio_from_consolodated_position_report(
  "ccmf",
  "Callodine Capital Master Fund",
  "CCMF",
  ccmf_url
)


AAPL <- SMAManager::create_security("AAPL")


dates_all <- AAPL$dates
dates <- dates_all[which(dates_all < Sys.Date())]
volume <- AAPL$volume
volume[match(dates, sort(dates))]
