#' @title Create Position
#' @description
#' Create an R6 Position object
#' @param ticker Security Ticker
#' @param desc Security Description
#' @param stock_qty Stock Quantity
#' @param delta_qty Delta Quantity
#' @param total_qty Total Quantity
#' @param mkt_val Market Value
#' @param delta_val Delta Value
#' @param stock_pct_nav Stock Percent of NAV
#' @param delta_pct_nav Delta Percent of NAV
#' @return A \code{Position} object.
#' @include class-position.R
#' @export
create_position <- function(
  ticker, desc,
  stock_qty, delta_qty, total_qty,
  mkt_val, delta_val,
  stock_pct_nav, delta_pct_nav
) {
  Position$new(
    ticker = ticker,
    desc = desc,
    stock_qty = stock_qty,
    delta_qty = delta_qty,
    total_qty = total_qty,
    mkt_val = mkt_val,
    delta_val = delta_val,
    stock_pct_nav = stock_pct_nav,
    delta_pct_nav = delta_pct_nav
  )
}
