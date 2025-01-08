# nolint start
#' @title SMA (R6 Object)
#'
#' @description R6 Class representing a seperately managed account.
#'  A seperately managed account is linked to a portfolio and contains
#'  Sma_Rules.
#'
#' @import R6
#' @import enfusion
#' @include create_position.R
SMA <- R6::R6Class(
  #nolint end
  "SMA",
  public = list(
    #' @field id SMA Id
    id = NULL,
    #' @field long_name SMA Long Name
    long_name = NULL,
    #' @field short_name SMA Short Name
    short_name = NULL,
    #' @field nav SMA NAV
    nav = NULL,
    #' @field enfusion_url Enfusion Web URL to
    #'  Consolidated Position Listing Report
    enfusion_url = NULL,
    #' @field positions List of Positions
    positions = NULL,
    #' @field target_portfolio Target Portfolio
    target_portfolio = NULL,
    #' @field sma_rules SMA Rules
    sma_rules = NULL,


    initialize = function(
      long_name, short_name, enfusion_url, target_portfolio
    ) {
      
    }
  )
)