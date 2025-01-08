library(enfusion)
library(SMAManager)

ccmf_url <- "https://webservices.enfusionsystems.com/mobile/rest/reportservice/exportReport?name=shared%2FDaily+Reports+Callodine%2FConsolidated+Position+Listing.ppr" #nolint

ccmf <- create_portfolio_from_consolodated_position_report(
  "ccmf",
  "Callodine Capital Master Fund",
  "CCMF",
  ccmf_url
)



AAPL <- create_security("AAPL")
test <- get_security("AAPL")

MSFT <- create_security("MSFT")


.security_registry <- new.env(parent = emptyenv())

1+1
