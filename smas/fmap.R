load_fmap <- function() {
  blpConnect()
  fmap <- SMAManager::create_sma_from_enfusion(
    long_name = "Citco Bank Canada Ref Blackstone CSP-MST FMAP Fund",
    short_name = "fmap",
    base_portfolio = "ccmf",
    holdings_url = paste0(
      "https://webservices.enfusionsystems.com/mobile/",
      "rest/reportservice/exportReport?",
      "name=shared%2FTaylor%2FSMA_Mgr_Reports%2F",
      "FMAP+Consolidated+Position+Listing+-+Options.ppr"
    ),
    trade_url = paste0(
      "https://webservices.enfusionsystems.com/mobile/",
      "rest/reportservice/exportReport?",
      "name=shared%2FTaylor%2FSMA_Mgr_Reports%2F",
      "FMAP_Trade_Detail.trb"
    )
  )

  # Create fmap rules
  fmap$add_rule(SMAManager::.sma_rule(
    sma_name = "fmap",
    rule_name = "position under 4.99% shares outstanding",
    scope = "position",
    bbfields = c("EX028", "DS381"),
    definition = function(security_id, sma) {
      sec_typ <- sapply(security_id, \(id) .security(id)$get_rule_data("EX028"))
      shares_out <- sapply(security_id, \(id) .security(id)$get_rule_data("DS381")) #nolint
      dplyr::case_when(
        sec_typ == "Equity" ~ 1 / shares_out,
        TRUE ~ 0
      )
    },
    max_threshold = 0.0499,
    min_threshold = -Inf
  ))

  fmap$add_rule(SMAManager::.sma_rule(
    sma_name = "fmap",
    rule_name = "no mlps except on swap",
    scope = "position",
    bbfields = "DS213",
    definition = function(security_id, sma) {
      sec_typ <- sapply(security_id, \(id) .security(id)$get_rule_data("DS213"))
      sec_typ == "MLP"
    },
    swap_only = TRUE
  ))

  fmap$add_rule(SMAManager::.sma_rule(
    sma_name = "fmap",
    rule_name = "no partnerships except on swap",
    scope = "position",
    bbfields = "DS674",
    definition = function(security_id, sma) {
      sec_typ <- sapply(security_id, \(id) .security(id)$get_rule_data("DS674"))
      sec_typ == "Partnership Shares"
    },
    swap_only = TRUE
  ))

  fmap$add_rule(SMAManager::.sma_rule(
    sma_name = "fmap",
    rule_name = "no etps except on swap",
    scope = "position",
    bbfields = "DS213",
    definition = function(security_id, sma) {
      sec_typ <- sapply(security_id, \(id) .security(id)$get_rule_data("DS213"))
      sec_typ == "ETP"
    },
    swap_only = TRUE
  ))

  fmap$add_rule(SMAManager::.sma_rule(
    sma_name = "fmap",
    rule_name = "no etns except on swap",
    scope = "position",
    bbfields = "DS213",
    definition = function(security_id, sma) {
      sec_typ <- sapply(security_id, \(id) .security(id)$get_rule_data("DS213"))
      sec_typ == "ETN"
    },
    swap_only = TRUE
  ))

  fmap$add_rule(SMAManager::.sma_rule(
    sma_name = "fmap",
    rule_name = "no bdcs except on swap",
    scope = "position",
    bbfields = "BI005",
    definition = function(security_id, sma) {
      bics_5 <- sapply(security_id, \(id) .security(id)$get_rule_data("BI005"))
      bics_5 == "BDCs"
    },
    swap_only = TRUE
  ))

  fmap$add_rule(SMAManager::.sma_rule(
    sma_name = "fmap",
    rule_name = "liquidity",
    scope = "position",
    bbfields = c("EX028", "HS020"),
    definition = function(security_id, sma) {
      sec_typ <- sapply(security_id, \(id) .security(id)$get_rule_data("EX028"))
      volume <- sapply(security_id, \(id) .security(id)$get_rule_data("HS020"))
      dplyr::case_when(
        sec_typ == "Equity" ~ 1 / volume,
        TRUE ~ 0
      )
    },
    max_threshold = 1.83,
    min_threshold = -1.83
  ))

  invisible(fmap)
}