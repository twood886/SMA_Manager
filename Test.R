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
  file = "20250508_sma_trades.csv",
  row.names = FALSE
)


rules <- bemap$get_sma_rules()

securities <- sapply(ccmf$get_position(), \(pos) pos$get_id())
test <- get_security_position_limits(securities, rules)

def <- rule$get_definition()

security_id <- c("AAPL US Equity", "MSFT US Equity")
exp <- def(security_id, bemap)



    apply_rule_definition = function(rule, security_id, sma) {
      exp <- setNames(
        as.list(rule$get_definition(security_id, sma)),
        security_id
      )
      if (length(exp) == 1) return(exp[[1]])
      exp
    }
