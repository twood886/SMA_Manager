#' @title Position (R6 Object)
#' @description
#' R6 Class representing a position object.
#' A position object contains information about the size of the
#'  position (in relation to a portfolio or SMA object) as well as
#'  underlying security R6 object.
#' @import R6
#' @include get_or_create_security.R
#' @include get_portfolio.R
#' @export
Position <- R6::R6Class(  #nolint
  "Position",
  public = list(
    #' @description
    #' Create New PositionR6 object
    #' @param portfolio_short_name Portfolio_Short_name
    #' @param id Security Ticker
    #' @param qty Stock Quantity
    #' @param sec Security
    initialize = function(portfolio_short_name, id, qty, sec) {
      private$portfolio_short_name_ <- portfolio_short_name
      private$id_ <- id
      private$qty_ <- qty
      private$security_ <- sec
    },

    # Getter Functions ---------------------------------------------------------
    #' @description Get position ticker
    get_id = function() private$id_,
    #' @description Get position security
    get_security = function() private$security_,
    #' @description Get position Quantity
    get_qty = function() private$qty_,
    #' @description Get position Delta Quantity
    get_delta_qty = function() {
      if (is.null(private$delta_qty_)) {
        self$calc_delta_qty()
      }
      private$delta_qty_
    },
    #' @description Get position Market Value
    get_mkt_val = function() {
      if (is.null(private$mkt_val_)) {
        self$calc_mkt_val()
      }
      private$mkt_val_
    },
    #' @description Get position Delta Value
    get_delta_val = function() {
      if (is.null(private$delta_val_)) {
        self$calc_delta_val()
      }
      private$delta_val_
    },

    # Setter Functions ---------------------------------------------------------
    #' @description Set Qty
    #' @param new_qty New Quantity
    set_qty = function(new_qty) {
      private$qty_ <- new_qty
      self$calc_delta_qty()
      self$calc_mkt_val()
      self$calc_delta_val()
      self$calc_stock_pct_nav()
      self$calc_delta_pct_nav()
    },
    #' @description Set Delta Qty
    #' @param new_delta_qty New Delta Quantity
    set_delta_qty = function(new_delta_qty) {
      private$delta_qty_ <- new_delta_qty
    },
    #' @description Set Market Value
    #' @param new_mkt_val New Market Value
    set_mkt_val = function(new_mkt_val) {
      private$mkt_val_ <- new_mkt_val
    },
    #' @description Set Delta Value
    #' @param new_delta_val New Delta Value
    set_delta_val = function(new_delta_val) {
      private$delta_val_ <- new_delta_val
    },
    #' @description Set Stock Percent of NAV
    #' @param new_stock_pct_nav New Stock Percent of NAV
    set_stock_pct_nav = function(new_stock_pct_nav) {
      private$stock_pct_nav_ <- new_stock_pct_nav
    },
    #' @description Set Delta Percent of NAV
    #' @param new_delta_pct_nav New Delta Percent of NAV
    set_delta_pct_nav = function(new_delta_pct_nav) {
      private$delta_pct_nav_ <- new_delta_pct_nav
    },


    # Calculators --------------------------------------------------------------
    #' @description Calc delta_qty
    calc_delta_qty = function() {
      private$delta_qty_ <- private$qty_ * private$security_$get_delta()
    },
    #' @description Calc mkt_val
    calc_mkt_val = function() {
      private$mkt_val_ <- private$qty_ * private$security_$get_price()
    },
    #' @description Calc delta_val
    calc_delta_val = function() {
      private$delta_val_ <- private$delta_qty_ * private$security_$get_price()
    },
    #' @description Calc stock_pct_nav
    #' @param nav Portfolio NAV
    calc_stock_pct_nav = function(nav = NULL) {
      if (is.null(nav)) {
        nav <- get_portfolio(private$portfolio_short_name_)$get_nav()
      }
      private$stock_pct_nav_ <- self$get_mkt_val() / nav
    },
    #' @description Calc delta_pct_nav
    #' @param nav Portfolio NAV
    calc_delta_pct_nav = function(nav = NULL) {
      if (is.null(nav)) {
        nav <- get_portfolio(private$portfolio_short_name_)$get_nav()
      }
      private$delta_pct_nav_ <- self$get_delta_val() / nav
    }
  ),

  private = list(
    portfolio_short_name_ = NULL,
    id_ = NULL,
    qty_ = NULL,
    delta_qty_ = NULL,
    mkt_val_ = NULL,
    delta_val_ = NULL,
    stock_pct_nav_ = NULL,
    delta_pct_nav_ = NULL,
    security_ = NULL
  )
)
