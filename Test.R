library(SMAManager)
library(Rblpapi)

con <- blpConnect()

ccmf_url <- "https://webservices.enfusionsystems.com/mobile/rest/reportservice/exportReport?name=shared%2FTaylor%2FSMA_Mgr_Reports%2FCCMF+Consolidated+Position+Listing+-+Options.ppr" #nolint

ccmf <- SMAManager::create_portfolio_from_enfusion(
  long_name = "Callodine Capital Master Fund",
  short_name = "ccmf",
  enfusion_url = ccmf_url
)

cube_sma <- SMAManager::create_sma(
  long_name = "Cube Test SMA",
  short_name = "cube",
  nav = 75000000,
  target_portfolio = "ccmf"
)

cube_sma$add_replacement("et us equity", "xom us equity")
cube_sma$add_replacement("gel us equity", "xom us equity")

rebalance_sma_position(cube_sma, "et us equity")
rebalance_sma(cube_sma)

test <- SMARulePosition$new(
  "cube",
  "test_rule",
  "position",
  definition = function(position) {
    security <- position$get_security()
    mlp_flag <- ifelse(
      Rblpapi::bdp(position$get_id(), "DS213")$DS213 == "MLP",
      TRUE,
      FALSE
    )
    security$set_data_item("mlp_flag" = mlp_flag)
    qty <- position$get_qty()
    mlp_flg <- security$get_data_item("mlp_flag")
    return(qty * mlp_flg)
  },
  threshold = function(x) abs(x) == 0
)
