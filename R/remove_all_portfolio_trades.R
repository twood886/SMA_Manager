#' @title Remove all trades for a portfolio
#' @param portfolio_id The portfolio ID of the trade.
remove_all_portfolio_trades <- function(portfolio_id = NULL) {
  assert_string(portfolio_id, "portfolio_id")
  if (!exists(portfolio_id, envir = registries$portfolios, inherits = FALSE)) {
    stop("Specified portfolio has not been created")
  }
  trades <- mget(
    ls(envir = registries$trades, all.names = TRUE),
    envir = registries$trades, 
    inherits = TRUE
  )
  lapply(trades, function(x) x$remove_trade_qty(portfolio_id))
  invisible(NULL)
}
