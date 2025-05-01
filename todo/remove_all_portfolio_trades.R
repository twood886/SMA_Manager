#' @title Remove all trades for a portfolio
#' @param portfolio_id The portfolio ID of the trade.
remove_all_portfolio_trades <- function(portfolio_id = NULL) {
  if (is.null(portfolio_id)) stop("portfolio_id is required")
  if (!exists(portfolio_id, envir = .portfolio_registry, inherits = FALSE)) {
    stop("Specified portfolio has not been created")
  }
  trades <- mget(
    ls(envir = .trade_registry, all.names = TRUE),
    envir = .trade_registry, 
    inherits = TRUE
  )
  lapply(trades, function(x) x$remove_trade_qty(portfolio_id))
  invisible(NULL)
}
