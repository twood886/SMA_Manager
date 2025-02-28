#' @title Position (R6 Object)
#' @description
#' R6 Class representing a position object.
#' A position object contains information about the size of the
#'  position (in relation to a portfolio or SMA object) as well as
#'  underlying security R6 object.
#' @import R6
#' @include get_or_create_security.R
Position <- R6::R6Class(  #nolint
  "Position",
  public = list(
    #' @field portfolio_short_name Portfolio Short Name
    portfolio_short_name = NULL,
    #' @field id Security Ticker
    id = NULL,
    #' @field qty Stock Quantity
    qty = NULL,
    #' @field delta_qty Delta Quantity
    delta_qty = NULL,
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
    #' @param portfolio_short_name Portfolio_Short_name
    #' @param id Security Ticker
    #' @param qty Stock Quantity
    #' @param delta_qty Delta Quantity
    #' @param mkt_val Market Value
    #' @param delta_val Delta Value
    #' @param stock_pct_nav Stock Percent of NAV
    #' @param delta_pct_nav Delta PErcent of NAV
    #' @param sec Security
    initialize = function(
      portfolio_short_name,
      id,
      qty, delta_qty,
      mkt_val, delta_val,
      stock_pct_nav, delta_pct_nav,
      sec
    ) {
      self$portfolio_short_name <- portfolio_short_name
      self$id <- id
      self$qty <- qty
      self$delta_qty <- delta_qty
      self$mkt_val <- mkt_val
      self$delta_val <- delta_val
      self$stock_pct_nav <- stock_pct_nav
      self$delta_pct_nav <- delta_pct_nav
      self$security <- sec
    },

    # Getter Functions ---------------------------------------------------------
    #' @description Get position ticker
    get_id = function() self$id,

    #' @description Get position security
    get_security = function() self$security,

    #' @description Get position Quantity
    get_qty = function() self$qty,

    # Setter Functions ---------------------------------------------------------
    #' @description Set Qty
    #' @param new_qty todo
    set_qty = function(new_qty) {
      self$qty <- new_qty
    },

    # Calculators --------------------------------------------------------------
    #' @description Calc stock_pct_nav
    #' @param nav Portfolio NAV
    calc_stock_pct_nav = function(nav = NULL) {
      if (is.null(nav)) {
        nav <- get_portfolio(self$portfolio_short_name)$get_nav()
        self$stock_pct_nav <- self$mkt_val / nav
      } else {
        self$stock_pct_nav <- self$mkt_val / nav
      }
    },
    #' @description Calc delta_pct_nav
    #' @param nav Portfolio NAV
    calc_delta_pct_nav = function(nav = NULL) {
      if (is.null(nav)) {
        nav <- get_portfolio(self$portfolio_short_name)$get_nav()
        self$stock_pct_nav <- self$mkt_val / nav
      } else {
        self$delta_pct_nav <- self$delta_val / nav
      }
    }
  )
)
