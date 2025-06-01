bamsf_url <- "https://webservices.enfusionsystems.com/mobile/rest/reportservice/exportReport?name=shared%2FTaylor%2FSMA_Mgr_Reports%2FBEMAP+Consolidated+Position+Listing+-+Options.ppr"

bamsf <- create_sma_from_enfusion(
  long_name = "Blackstone Alternative Multi-Strategy Fund",
  short_name = "bamsf",
  base_portfolio = "ccmf",
  enfusion_url = bamsf_url
)

.sma_rule(
  sma_name = "bamsf",
  rule_name = "200% Max Gross Exp",
  scope = "portfolio",
  definition = function(security_id, portfolio) {
    type <- Rblpapi::bdp(security_id, "EX028")$EX028
    price <- Rblpapi::bdp(security_id, "PX_LAST")$PX_LAST
    underlying_price <- Rblpapi::bdp(security_id, "OP004")$OP004
    delta <- Rblpapi::bdp(security_id, "OP006")$OP006
    price[type == "FixedIncome"] <- price[type == "FixedIncome"] / 100
    price[type == "Option"] <- underlying_price[type == "Option"]
    delta[type != "Option"] <- 1
    nav <- portfolio$get_nav()
    (delta * price) / nav
  },
  swap_only = FALSE,
  max_threshold = 2,
  min_threshold = -Inf,
  gross_exposure = TRUE
)

.bics_rule <- function(security_id, portfolio, tgt_bics = NULL) {
  security <- lapply(security_id, \(id) .security(id))
  type <- vapply(security, \(x) x$get_instrument_type(), character(1))
  bics_lvl2 <- vapply(security, \(x) x$get_bics_level_2(), character(1))
  bics_lvl3 <- vapply(security, \(x) x$get_bics_level_3(), character(1))
  bics <- bics_lvl3
  bics[type == "FixedIncome"] <- bics_lvl2[type == "FixedIncome"]
  price <- vapply(security, \(x) x$get_price(), numeric(1))
  underlying_price <- vapply(security, \(x) x$get_underlying_price(), numeric(1)) #nolint
  price[type == "Option"] <- underlying_price[type == "Option"]
  delta <- vapply(security, \(x) x$get_delta(), numeric(1))
  nav <- portfolio$get_nav()
  exp <- (delta * price) / nav
  exp[bics != tgt_bics] <- 0
  setNames(exp, security_id)
}


industry_limit <- list(
  c("101010", "EQ Advertising & Marketing", 0),
  c("101011", "EQ Cable & Satellite", 0.19),
  c("101012", "EQ Entertainment Content", 0.10),
  c("101013", "EQ Publishing & Broadcasting", 0.06),
  c("101014", "EQ Internet Media & Services", 0.10),
  c("101101", "EQ Telecommunications", 0.18),
  c("111010",	"EQ Apparel & Textile Prod",	0.20),
  c("111011",	"EQ Automotive", 0.18),
  c("111012",	"EQ Home Construction", 0.10),
  c("111013",	"EQ Home & Office Products", 0.06),
  c("111014",	"EQ Leisure Products", 0.13),
  c("111110",	"EQ Consumer Services", 0.08),
  c("111111",	"EQ Leisure Facilities & Services", 0.30),
  c("111210",	"EQ Wholesale - Discretionary", 0.06),
  c("111211",	"EQ Retail - Discretionary", 0.30),
  c("111212",	"EQ E-Commerce Discretionary", 0.08),
  c("121010",	"EQ Food", 0.12),
  c("121011",	"EQ Beverages", 0.12),
  c("121012",	"EQ Tobacco & Cannabis", 0.19),
  c("121013",	"EQ Household Products", 0.18),
  c("121110",	"EQ Wholesale - Consumer Staples", 0.09),
  c("121111",	"EQ Retail - Consumer Staples", 0.13),
  c("131010",	"EQ Oil & Gas Producers", 0.30),
  c("131011",	"EQ Oil & Gas Services & Equipment", 0.09),
  c("131110",	"EQ Renewable Energy", 0.12),
  c("141010",	"EQ Banking", 0.18),
  c("141110",	"EQ Asset Management", 0.47),
  c("141111",	"EQ Specialty Finance", 0.47),
  c("141112",	"EQ Institutional Financial Svcs", 0.20),
  c("141210",	"EQ Insurance", 0.14),
  c("151010",	"EQ Real Estate Owners & Developers", 0.05),
  c("151012",	"EQ Real Estate Services",	0.06),
  c("151011",	"EQ REIT", 0.22),
  c("161010",	"EQ Biotech & Pharma", 0.30),
  c("161011",	"EQ Health Care Facilities & Services", 0.16),
  c("161012",	"EQ Medical Equipment & Devices", 0.16),
  c("171010",	"EQ Diversified Industrials", 0),
  c("171011",	"EQ Aerospace & Defense", 0.10),
  c("171012",	"EQ Electrical Equipment", 0.09),
  c("171013",	"EQ Machinery", 0.11),
  c("171014",	"EQ Transportation Equipment", 0.06),
  c("171015",	"EQ Industrial Intermediate Production", 0),
  c("171110",	"EQ Industrial Support Services", 0.15),
  c("171111",	"EQ Commercial Support Services", 0.18),
  c("171112",	"EQ Transportation & Logistics", 0.10),
  c("171113",	"EQ Engineering & Construction", 0.09),
  c("181010",	"EQ Chemicals", 0.12),
  c("181011",	"EQ Construction Materials",	0.07),
  c("181012",	"EQ Containers & Packaging", 0),
  c("181013",	"EQ Forestry, Paper & Wood Products", 0),
  c("181014",	"EQ Steel", 0.09),
  c("181015",	"EQ Metals & Mining", 0.12),
  c("191010",	"EQ Technology Hardware", 0.11),
  c("191011",	"EQ Semiconductors", 0.14),
  c("191110",	"EQ Software", 0.13),
  c("191111",	"EQ Technology Services", 0.13),
  c("201010",	"EQ Electric Utilities", 0.19),
  c("201011",	"EQ Gas & Water Utilities", 0.14),
  c("201012",	"EQ Electric & Gas Marketing & Trading", 0),
  c("501010",	"EQ Supranationals", 0),
  c("501110",	"EQ Sovereign Government", 0),
  c("501111",	"EQ Sovereign Agencies", 0),
  c("501112",	"EQ Governmental Banks", 0),
  c("501210",	"EQ Regional", 0),
  c("501211",	"EQ Local", 0)
)

lapply(
  industry_limit,
  \(x) {
    bamsf$add_rule(
      .sma_rule(
        sma_name = "bamsf",
        rule_name = paste0(x[2], " ", as.numeric(x[3]) * 100, "% Gross Exp"),
        scope = "portfolio",
        definition = function(security_id, portfolio) {
          .bics_rule(security_id, portfolio, x[1])
        },
        max_threshold = as.numeric(x[3]),
        min_threshold = 0,
        gross_exposure = TRUE
      )
    )
    invisible(TRUE)
  }
)
