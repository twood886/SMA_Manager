#' @title SMA (S4 Object)
#' @description S4 Class representing a separetly managed account
#' @slot id Account Id
#' @slot long_name SMA Long Name
#' @slot short_name SMA Short Name
#' @slot positions List of Positions
setClass(
  "portfolio",
  contains = "portfolio",
  representation(
    portfolio_target = "character",
    sma_rules = ""
  )
)