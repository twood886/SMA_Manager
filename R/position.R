# Object Definition ------------------------------------------------------------
#' @title Position (S4 Object)
#' @description An S4 Class to represent a portoflio position
setClass(
  "position",
  representation(
    ticker = "character",
    desc = "character",
    stock_qty = "numeric",
    delta_qty = "numeric",
    total_qty = "numeric",
    mkt_val = "numeric",
    delta_val = "numeric",
    stock_pct_nav = "numeric",
    delta_pct_nav = "numeric",
    adv_days = "numeric",
    security = "security"
  )
)

# Create Object ----------------------------------------------------------------
create_position <- function(
  ticker, desc,
  stock_qty, delta_qty, total_qty,
  mkt_val, delta_val,
  stock_pct_nav, delta_pct_nav
) {
  new("position",
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