  # 1.4	“Securities” shall mean all securities held as investments in the
  #    Investment Account, provided that, unless otherwise agreed by the parties
  #    in writing, only the following types of Securities may be purchased or
  #    sold for the Investment Account, subject to the Investment Guidelines and
  #    in accordance with any other terms of this Agreement: 
  
  # 1.4(a) publicly traded equity and convertible securities of issuers
  #    organized and traded in the United States, including exchange-traded funds
  #    (“ETFs”) of United States issuers;

  # 1.4(b) publicly traded equity and convertible securities of issuers
  #    organized in countries other than the United States are permitted only if
  #    both of the following conditions are met: 
  
  # 1.4(b)(i) such instruments are traded in the markets set forth on Schedule II
  #    to this Agreement, as may be amended from time to time with Client’s
  #    written approval, and such other markets in which Client has established
  #    execution and clearance capabilities as determined by Client in its sole
  #    discretion and communicated to Investment Manager in writing
  #    (collectively, “Other Permitted Markets”); and 

  # 1.4(b)(ii) any such securities of issuers organized in an Other Permitted
  #    Market are traded as a total return swap;

  # 1.4(c) equity-linked derivatives, including both listed and over-the-counter
  #    (“OTC”) options and swaps, on (i) equity securities described in the
  #    foregoing clauses (a) and (b) and (ii) indices; and

  # 1.4(d) futures instruments and options on futures (including commodities and
  #    commodity ETFs) (collectively, “Futures”).  

  # 3.5	Notwithstanding anything herein to the contrary:
  # 3.5(a) If Investment Manager plans to engage in any activity that would be
  #    reasonably expected to require the filing of a Schedule 13D with respect
  #    to any issuer pursuant to Section 13 of the Securities Exchange Act of
  #    1934, as amended, the Investment Account shall (i) not purchase any
  #    securities of such issuer and (ii) the Investment Manager will sell any
  #    such securities to the extent held in the Account prior to filing a
  #    Schedule 13D.

  # 3.5(b) No portion of the Investment Account will be invested in securities
  #    that are designated by either of Client or CAAM as restricted. Each of
  #    Client and CAAM may amend from time to time the list of securities
  #    designated as restricted by a communication in writing to Investment
  #    Manager. Investment Manager shall not otherwise effect any transactions
  #    on behalf of the Investment Account in securities that are designated by
  #    Client and/or CAAM as prohibited (and so communicated to Investment
  #    Manager in writing), in each case until Client and/or CAAM, as applicable,
  #    has notified Investment Manager in writing that such restriction/
  #    prohibition is waived or is no longer in effect. Upon written instruction
  #    from Client, Investment Manager shall cause the Investment Account to
  #    promptly dispose of securities which are designated as restricted or
  #    prohibited in accordance with the foregoing; provided, that Client shall
  #    not instruct Investment Manager to dispose of securities in violation of
  #    applicable law (e.g., for which Client or CAAM are in possession of
  #    material non-public information).

  # 3.5(c) Investment Manager shall not cause the Investment Account to invest
  #    in (i) any securities, companies or transactions that are the subject of
  #    any sanctions administered or enforced by the U.S. Department of
  #    Treasury’s Office of Foreign Assets Control (including, but not limited
  #    to, U.S. Executive Order 13959, as amended—Addressing the Threat From
  #    Securities Investments That Finance Communist Chinese Military Companies,
  #    U.S. Executive Order 13936, as amended—The President’s Executive Order on
  #    Hong Kong Normalization, U.S. Executive Order 14024—Blocking Property with
  #    respect to Specified Harmful Foreign Activities of the Government of the
  #    Russian Federation and related directives and U.S. Executive Order
  #    14065—Blocking Property of Certain Persons and Prohibiting Certain
  #    Transactions with respect to Continued Russian Efforts to Undermine the
  #    Sovereignty and Territorial Integrity of Ukraine), HM Treasury’s Office
  #    for Financial Sanctions Implementation, the United Nations Security
  #    Council, the European Union, the World Bank, or any other relevant
  #    sanctions authority (collectively, “Sanctions”);
  
  #3.5(c) or (ii) a country or territory that is, or becomes while this Agreement
  #    is in effect, the subject of comprehensive Sanctions (currently Crimea,
  #    Cuba, the so-called Donetsk People’s Republic, Iran, the so-called
  #    Luhansk People’s Republic, North Korea, and Syria).

load_caty <- function() {
  caty <- SMAManager::create_sma_from_enfusion(
    long_name = "Catenary SMA",
    short_name = "caty",
    base_portfolio = "ccmf",
    holdings_url = paste0(
      "https://webservices.enfusionsystems.com/mobile/",
      "rest/reportservice/exportReport?",
      "name=shared%2FTaylor%2FSMA_Mgr_Reports%2F",
      "CAT+-+Positions.ppr"
    )
  )
  
  caty$add_rule(.sma_rule(
    sma_name = "caty",
    rule_name = "Gross Exposure Under 250% of NAV",
    scope = "portfolio",
    bbfields = NULL,
    definition = function(security_id, portfolio) {
      security <- lapply(security_id, function(id) .security(id))
      price <- vapply(security, \(x) x$get_price(), numeric(1))
      und_p <- vapply(security, \(x) x$get_underlying_price(), numeric(1))
      type <- vapply(security, \(x) x$get_instrument_type(), character(1))
      price[type == "Option"] <- und_p[type == "Option"]
      delta <- vapply(security, function(x) x$get_delta(), numeric(1))
      nav <- portfolio$get_nav()
      exp <- (delta * price) / nav
      names(exp) <- security_id
      exp
    },
    swap_only = FALSE,
    max_threshold = 2.5,
    min_threshold = 0,
    gross_exposure = TRUE
  ))

  caty$add_rule(.sma_rule(
    sma_name = "caty",
    rule_name = "Net Exposure +/- 50% of NAV",
    scope = "portfolio",
    bbfields = NULL,
    definition = function(security_id, portfolio) {
      security <- lapply(security_id, \(id) .security(id))
      price <- vapply(security, \(x) x$get_price(), numeric(1))
      und_p <- vapply(security, \(x) x$get_underlying_price(), numeric(1))
      type <- vapply(security, \(x) x$get_instrument_type(), character(1))
      price[type == "Option"] <- und_p[type == "Option"]
      delta <- vapply(security, function(x) x$get_delta(), numeric(1))
      nav <- portfolio$get_nav()
      exp <- (delta * price) / nav
      names(exp) <- security_id
      exp
    },
    swap_only = FALSE,
    max_threshold = 0.5,
    min_threshold = -0.5,
    gross_exposure = FALSE
  ))

  caty$add_rule(SMAManager::.sma_rule(
    sma_name = "caty",
    rule_name = "Gross Position under 12% of NAV",
    scope = "position",
    bbfields = c(NULL),
    definition = function(security_id, sma) {
      nav <- sma$get_nav()
      price <- vapply(security_id, \(id) .security(id)$get_price(), numeric(1))
      price / nav
    },
    swap_only = FALSE,
    max_threshold = 0.12,
    min_threshold = -0.12
  ))

  caty$add_rule(SMAManager::.sma_rule(
    sma_name = "caty",
    rule_name = "liquidity",
    scope = "position",
    bbfields = c("HS013"),
    definition = function(security_id, sma) {
      volume <- sapply(security_id, \(id) .security(id)$get_rule_data("HS013"))
      1 / volume
    },
    max_threshold = 1,
    min_threshold = -1
  ))

  caty$add_rule(.sma_rule(
    sma_name = "caty",
    rule_name = "Positon under 2.5% of Shares Outstanding",
    scope = "position",
    bbfields = c("DS381"),
    definition = function(security_id, sma) {
      sec_type <- vapply(security_id, \(id) .security(id)$get_instrument_type(), character(1)) #nolint
      shares_out <- sapply(security_id, \(id) .security(id)$get_rule_data("DS381")) #nolint
      dplyr::case_when(
        sec_type == "Equity" ~ 1 / shares_out,
        TRUE ~ 0
      )
    },
    swap_only = FALSE,
    max_threshold = 0.025,
    min_threshold = -Inf
  ))

  caty$add_rule(.sma_rule(
    sma_name = "caty",
    rule_name = "Market Cap over $500MM",
    scope = "position",
    bbfields = c("RR902"),
    definition = function(security_id, sma) {
      except <- c("www us equity")
      mktcap <- vapply(
        security_id,
        \(id) .security(id)$get_rule_data("RR902"),
        numeric(1)
      )
      dplyr::case_when(
        mktcap < 500e6 & !tolower(security_id) %in% except ~ 1,
        TRUE ~ 0
      )
    },
    max_threshold = 0,
    min_threshold = 0,
    swap_only = FALSE
  ))

  caty$add_rule(.sma_rule(
    sma_name = "caty",
    rule_name = "Non-US Equity on Swap Only",
    scope = "position",
    bbfields = "DS290",
    definition = function(security_id, sma) {
      permitted_countries <- c("US")
      iss_cty <- sapply(security_id, \(id) .security(id)$get_rule_data("DS290"))
      !iss_cty %in% permitted_countries
    },
    swap_only = TRUE
  ))

  caty$add_rule(SMAManager::.sma_rule(
    sma_name = "caty",
    rule_name = "no mlps except on swap",
    scope = "position",
    bbfields = "DS213",
    definition = function(security_id, sma) {
      sec_typ <- sapply(security_id, \(id) .security(id)$get_rule_data("DS213"))
      sec_typ == "MLP"
    },
    swap_only = TRUE
  ))

  caty$add_rule(SMAManager::.sma_rule(
    sma_name = "caty",
    rule_name = "no partnerships except on swap",
    scope = "position",
    bbfields = "DS674",
    definition = function(security_id, sma) {
      sec_typ <- sapply(security_id, \(id) .security(id)$get_rule_data("DS674"))
      sec_typ == "Partnership Shares"
    },
    swap_only = TRUE
  ))
  invisible(caty)
}
