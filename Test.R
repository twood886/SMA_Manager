library(SMAManager)

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




test <- SMARulePosition$new(
  "cube",
  "test_rule",
  "position",
  definition = function(position) {
    qty <- position$get_qty()
    security <- position$get_security()
    mlp_flg <- security$get_data_item("mlp_flag")
    return(qty * mlp_flg)
  },
  threshold = function(x) abs(x) == 0
)

x <- cat("test")
print(x)
