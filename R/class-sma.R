#' @title SMA (R6 Object)
#'
#' @description R6 Class representing a seperately managed account.
#'  A seperately managed account is linked to a portfolio and contains
#'  Rules.
#'
#' @import R6
#' @import enfusion
#' @import parallel
#' @include class-portfolio.R
#' @include class-position.R
#' @include class-security.R
#' @include utils.R
#' @include api-functions.R
#' @include class-tradeconstructor.R
#' @export
SMA <- R6::R6Class(   #nolint
  "SMA",
  inherit = Portfolio,
  private = list(
    base_portfolio_ = NULL
  ),
  public = list(
    #' @description
    #' Create a new SMA R6 object.
    #' @param long_name Character. SMA Long Name.
    #' @param short_name Character. SMA Short Name.
    #' @param nav Numeric. SMA Net Asset Value.
    #' @param positions Optional. SMA Positions. Default is NULL.
    #' @param base_portfolio An object representing the base portfolio.
    #' @return A new instance of the SMA class.
    initialize = function(
      long_name, short_name, nav, positions = NULL, base_portfolio = NULL
    ) {
      private$long_name_ <- long_name
      private$short_name_ <- short_name
      private$nav_ <- nav
      private$base_portfolio_ <- base_portfolio
      private$positions_ <- positions
      private$target_positions_ <- positions
      private$rules_ <- list()
      private$replacements_ <- list()
      private$trade_constructor <- SMAConstructor$new()
    },


    # Getters ------------------------------------------------------------------
    #' Get Base Portfolio
    #' @description Get the tagret portfolio
    get_base_portfolio = function() private$base_portfolio_,

    #' Get Base Portfolio Position
    #' @description Get a position in the Base portfolio
    #' @param security_id Security ID
    get_base_portfolio_position = function(security_id = NULL) {
      self$get_base_portfolio()$get_position(security_id)
    },

    #' @description Calculate the rebalance trade quantity for a given security
    #' @param security_id Security ID
    #' @param base_trade_qty Base trade quantity (default: 0)
    calc_proposed_rebalance_trade = function(security_id = NULL, base_trade_qty = 0) {
      self$get_trade_constructor()$calc_rebalance_qty(self, security_id, base_trade_qty)
    },

    #' @description mimic the base portfolio target position
    #' @param security_id Security ID
    #' @param assign_to_registry Assign to registry (default: TRUE)
    mimic_base_portfolio = function(security_id = NULL, assign_to_registry = TRUE) {
      constructor <- self$get_trade_constructor()
      rebal <- constructor$calc_rebalance_qty(self, security_id)
      if (length(rebal$trade_qty) != 0) {
        trades <- list()
        swap <- constructor$get_swap_flag_position_rules(self, names(rebal$trade_qty))
        for (i in seq_along(rebal$trade_qty)) {
          t <- .trade(
            security_id = names(rebal$trade_qty)[i],
            portfolio_id = self$get_short_name(),
            qty = rebal$trade_qty[[i]],
            swap = swap[[names(rebal$trade_qty)[i]]],
            create = TRUE,
            assign_to_registry = assign_to_registry
          )
          trades[[t$get_id()]] <- t
        }
      }
      for (i in seq_along(rebal$unfilled_qty)) {
        warning(
          paste0(
            "Unfilled quantity for ", names(rebal$unfilled_qty)[i], ": ",
            rebal$unfilled_qty[[i]]
          )
        )
      }
      invisible(trades)
    }
  )
)