#' @title Position (R6 Object)
#' @description
#' A position
#' @import R6
#' @include get_or_create_security.R
Position <- R6::R6Class(  #nolint
  "PoisitionR6",
  public = list(
    #' @field ticker Security Ticker
    ticker = NULL,
    #' @field desc Security Description
    desc = NULL,
    #' @field stock_qty Stock Quantity
    stock_qty = NULL,
    #' @field delta_qty Delta Quantity
    delta_qty = NULL,
    #' @field total_qty Total Quantity
    total_qty = NULL,
    #' @field mkt_val Market Value
    mkt_val = NULL,
    #' @field delta_val Delta Value
    delta_val = NULL,
    #' @field stock_pct_nav Stock Percent of NAV
    stock_pct_nav = NULL,
    #' @field delta_pct_nav Delta PErcent of NAV
    delta_pct_nav = NULL,
    #' @field security SecurityR6 Objct
    security = NULL,

    #' @description
    #' Create New PositionR6 object
    #' @param ticker Security Ticker
    #' @param desc Security Description
    #' @param stock_qty Stock Quantity
    #' @param delta_qty Delta Quantity
    #' @param total_qty Total Quantity
    #' @param mkt_val Market Value
    #' @param delta_val Delta Value
    #' @param stock_pct_nav Stock Percent of NAV
    #' @param delta_pct_nav Delta PErcent of NAV
    initialize = function(
      ticker, desc,
      stock_qty, delta_qty, total_qty,
      mkt_val, delta_val,
      stock_pct_nav, delta_pct_nav
    ) {
      self$ticker <- ticker
      self$desc <- desc
      self$stock_qty <- stock_qty
      self$delta_qty <- delta_qty
      self$total_qty <- total_qty
      self$mkt_val <- mkt_val
      self$delta_val <- delta_val
      self$stock_pct_nav <- stock_pct_nav
      self$delta_pct_nav <- delta_pct_nav
      self$security <- get_or_create_security(ticker)
    }
  )
)
