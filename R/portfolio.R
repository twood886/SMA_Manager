#' @title Portfolio (S4 Object)
#' @description S4 Class representing a portfolio object
#' @slot id Account Id
#' @slot long_name Portfolio Long Name
#' @slot short_name Portfolio Short Name
#' @slot enfusion_url Enfusion Web URL to Consolidated Position Listing Report
#' @slot positions List of Positions
#' @include position.R
setClass(
  "portfolio",
  representation(
    id = "character",
    long_name = "character",
    short_name = "character",
    nav = "numeric",
    enfusion_url = "character",
    positions_current = "list",
    positions_target = "list",
    linked_smas = "character"
  )
)