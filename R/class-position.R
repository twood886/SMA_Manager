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
  private = list(
    portfolio_short_name_ = NULL,
    id_ = NULL,
    qty_ = NULL,
    security_ = NULL,
    swap_ = NULL
  ),
  public = list(
    #' @description
    #' Create New PositionR6 object
    #' @param portfolio_short_name Portfolio_Short_name
    #' @param qty Stock Quantity
    #' @param security Security
    #' @param swap Swap Flag
    initialize = function(portfolio_short_name, security, qty, swap = FALSE) {
      assert_inherits(security, "Security", "security")
      assert_numeric(qty, "qty")
      assert_bool(swap, "swap")
      private$portfolio_short_name_ <- portfolio_short_name
      private$security_ <- security
      private$id_ <- security$get_id()
      private$qty_ <- qty
      private$swap_ <- swap
    },

    # Getter Functions ---------------------------------------------------------
    #' @description Get position ticker
    get_id = function() private$id_,
    #' @description Get position security
    get_security = function() private$security_,
    #' @description Get position Quantity
    get_qty = function() private$qty_,
    #' @description Get Swap flag
    get_swap = function() private$swap_,
    #' @description Get position Delta Quantity
    get_delta_qty = function() private$qty_ * private$security_$get_delta(),
    #' @description Get position Market Value
    get_mkt_val = function() private$qty_ * private$security_$get_price(),
    #' @description Get position Delta Value
    get_delta_val = function() self$get_delta_qty * private$security_$get_price(),
    #' @decscription Get position Stock Percent of NAV
    #' @param nav Portfolio NAV
    get_mkt_pct_nav = function(nav = NULL) {
      tryCatch(
        {
          assert_numeric(nav, "nav")
          self$get_mkt_val() / nav
        },
        error = function(e) {
          self$get_mkt_val() / get_portfolio(private$portfolio_short_name_)$get_nav()
        }
      )
    },
    #' @description Get position Delta Percent of NAV
    #' @param nav Portfolio NAV
    get_delta_pct_nav = function(nav = NULL) {
      tryCatch(
        {
          assert_numeric(nav, "nav")
          self$get_delta_val() / nav
        },
        error = function(e) {
          self$get_delta_val() / get_portfolio(private$portfolio_short_name_)$get_nav()
        }
      )
    },
    # Setter Functions ---------------------------------------------------------
    #' @description Set Qty
    #' @param qty New Quantity
    set_qty = function(qty) {
      assert_numeric(qty, "qty")
      private$qty_ <- qty
    }
  )
)