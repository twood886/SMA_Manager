library(SMAManager)
library(tidyverse)
library(enfusion)
library(Rblpapi)


ccmf <- create_portfolio_from_enfusion(
  long_name = "Callodine Capital Master Fund",
  short_name = "ccmf",
  holdings_url = paste0(
    "https://webservices.enfusionsystems.com/mobile/",
    "rest/reportservice/exportReport?",
    "name=shared%2FTaylor%2FSMA_Mgr_Reports%2F",
    "CCMF+Consolidated+Position+Listing+-+Options.ppr"
  ),
  trade_url = paste0(
    "https://webservices.enfusionsystems.com/mobile/",
    "rest/reportservice/exportReport?",
    "name=shared%2FTaylor%2FSMA_Mgr_Reports%2F",
    "CCMF_Trade_Detail.trb"
  )
)


source("smas/cat.R")
caty <- load_caty()
caty$add_rule(.sma_rule(
  sma_name = "caty",
  rule_name = "Gross Position under 6% of NAV",
  scope = "position",
  bbfields = c(NULL),
  definition = function(security_id, sma) {
    nav <- sma$get_nav()
    price <- vapply(security_id, \(id) .security(id)$get_price(), numeric(1))
    price / nav
  },
  swap_only = FALSE,
  max_threshold = 0.06,
  min_threshold = -0.06
))

caty$add_replacement("www us equity", "aapl us equity")
update_bloomberg_fields()
sma <- caty

out <- caty$get_trade_constructor()$optimize_sma(caty)
plot(
  out$target_weights,
  out$weights,
  xlab = "Target Shares",
  ylab = "Final Shares",
  main = "SMA Optimization Result"
)

self <- list()
private <- list()

self$get_security_position_limits = function(
      portfolio,
      security_id = NULL,
      position_only = FALSE
    ) {
      if (is.null(security_id)) stop("Security ID must be supplied")
      rules <- Filter(\(r) !r$get_swap_only(), portfolio$get_rules())
      
      if (position_only) {
        rules <- Filter(\(r) r$get_scope() == "position", rules)
      }
      
      limits <- lapply(rules, \(r) r$get_security_limits(security_id))

      setNames(
        lapply(
          security_id,
          \(sec) {
            sec_limit <- lapply(limits, \(limit) limit[[sec]])
            if (length(sec_limit) == 0) return(list(max = Inf, min = -Inf))
            max_limit <- min(sapply(sec_limit, \(x) x$max), na.rm = TRUE)
            min_limit <- max(sapply(sec_limit, \(x) x$min), na.rm = TRUE)
            list(max = max_limit, min = min_limit)
          }
        ),
        security_id
      )
    }

self$get_scale_ratio = function(base_portfolio, sma_portfolio) {
      base_nav <- base_portfolio$get_nav()
      sma_nav  <- sma_portfolio$get_nav()
      if (base_nav == 0) return(0)
      sma_nav / base_nav
    }

self$get_scale_qty = function(base_portfolio, sma_portfolio, base_security_id) {
      base_pos_qty <- tryCatch(
        {base_portfolio$get_target_position(base_security_id)$get_qty()},
        error = function(e) 0
      )
      nav_ratio <- sma_portfolio$get_nav() / base_portfolio$get_nav()
      base_pos_qty * nav_ratio
    }

self$calc_target_quantities <- function(sma) {
  base <- sma$get_base_portfolio()
  scale_ratio <- self$get_scale_ratio(base, sma)
  # Get base positions
  base_positions <- private$.extract_qty(base$get_target_position())
  # Scale all base positions by NAV ratio
  target_quantities <- base_positions * scale_ratio
  # Clean up non-finite values
  if (any(!is.finite(target_quantities))) {
    target_quantities[!is.finite(target_quantities)] <- 0
  }

  # Include replacement securities
  replacements <- sma$get_replacement_security()
  replacement_secs <- unlist(replacements, use.names = FALSE)
  for (sec in replacement_secs) {
    if (!(sec %in% names(target_quantities))) {
      target_quantities[sec] <- 0
    }
  }
  
  target_quantities
}

private$.extract_qty = function(positions) {
      if (length(positions) == 0) return(numeric(0))
      
      ids <- vapply(positions, function(x) x$get_id(), character(1))
      qty <- vapply(positions, function(x) x$get_qty(), numeric(1))
      
      # Clean up non-finite values
      if (any(!is.finite(qty))) {
        qty[!is.finite(qty)] <- 0
      }
      
      setNames(qty, ids)
    }

plot(
(target_vec[names(sh_final)] * price_vec[names(sh_final)]) / nav,
(sh_final * price_vec[names(sh_final)]) / nav
)

sum(abs(sh_final)*price_vec[names(sh_final)]) / nav 
round(((sh_final * price_vec[names(sh_final)]) / nav) /
((target_vec[names(sh_final)] * price_vec[names(sh_final)]) / nav), 1)

want <- sma$get_trade_constructor()$calc_want_qty(sma)
prices <- vapply(
  names(want),
  function(id) .security(id)$get_price(),
  numeric(1)
)


rules_securities_idx <- vapply(
  sma$get_rules(),
  \(r) {
    swap <- r$get_swap_only()
    scope <- r$get_scope()
    !swap && scope == "position"
  },
  logical(1)
)

rules_securities <- sma$get_rules()[rules_securities_idx]
sec <- "www us equity"
test <- sapply(rules_securities, \(rule) {
  rule$get_security_limits(sec)
})
min <- max(vapply(test, \(limit) limit$min, numeric(1)), na.rm = TRUE)
max <- min(vapply(test, \(limit) limit$max, numeric(1)), na.rm = TRUE)



optimizer_cvxr <- SMAOptimizerCVXR4$new()

result <- optimizer_cvxr$optimize_sma(
  sma = caty,
  lambda_tracking = 1.0,
  lambda_overflow = 0.001,
  verbose = TRUE
)


