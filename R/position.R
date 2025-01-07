#' @title Position (S4 Object)
#' @description An S4 Class to represent a portoflio position
#' @include security.R
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