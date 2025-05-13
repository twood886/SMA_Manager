#' @include utils.R
#' @include api-functions.R
#' @export
create_trade_qty <- function(portfolio_id = NULL, security_id = NULL, trade_qty = NULL, swap) {
  assert_string(portfolio_id, "portfolio_id")
  assert_string(security_id, "security_id")
  security <- .security(security_id)
  security_id <- security$get_id()
  portfolio <- .portfolio(portfolio_id, create = FALSE)
  swap <- tryCatch(
    {portfolio$get_position(security_id)$get_swap()},
    error = function(e) swap
  )
  assert_bool(swap, "swap")
  
  trade <- .trade(
    security_id = security_id,
    portfolio_id = portfolio_id,
    qty = trade_qty,
    create = TRUE,
    swap = swap
  )

  tryCatch(
    {derived_portfolios <- get_tracking_smas(portfolio)},
    error = function(e) {
      return(invisible(trade))
    }
  )
  if (length(derived_portfolios) > 0) {
    for (derived_portfolio in derived_portfolios) {
      derived_portfolio$mimic_base_portfolio(security_id = security_id) 
    }
  }  
  invisible(trade)
}

#' @include utils.R
#' @include api-functions.R
#' @export
create_trade_tgt_weight <- function(portfolio_id = NULL, security_id = NULL, tgt_weight = NULL, swap) {
  assert_string(portfolio_id, "portfolio_id")
  assert_string(security_id, "security_id")
  security <- .security(security_id)
  security_id <- security$get_id()
  assert_number(tgt_weight, "tgt_weight")

  portfolio <- .portfolio(portfolio_id, create = FALSE)
  swap <- tryCatch(
    {portfolio$get_position(security_id)$get_swap()},
    error = function(e) swap
  )
  assert_bool(swap, "swap")
  security <- .security(security_id)
  target_qty <- portfolio$get_nav() * tgt_weight / security$get_price()
  current_pos <- portfolio$get_target_position(security_id)
  current_qty <- current_pos$get_qty()
  trade_qty <- target_qty - current_qty
  
  create_trade_qty(
    portfolio_id = portfolio_id,
    security_id = security_id,
    trade_qty = trade_qty,
    swap = swap
  )
}