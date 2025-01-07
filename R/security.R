#' @title Security (S4 Object)
#' @description An S4 Class to represent a security
#' @slot ticker Security Ticker
#' @slot yahoo_match Boolean if found on yahoo finance
#' @slot long_name Security Long Name
#' @slot short_name Security Short Name
#' @slot adv Average Daily Volume
#' @slot dates Dates corresponding to data
#' @slot open Open Prices
#' @slot close Close Prices
#' @slot low Low Prices
#' @slot high High Prices
#' @slot volume Trading Volume
#' @slot adj_close Adjusted Close Price
setClass(
  "security",
  representation(
    ticker = "character",
    yahoo_match = "logical",
    long_name = "character",
    short_name = "character",
    adv = "numeric",
    dates = "Date",
    open = "numeric",
    close = "numeric",
    low = "numeric",
    high = "numeric",
    volume = "numeric",
    adj_close = "numeric"
  )
)