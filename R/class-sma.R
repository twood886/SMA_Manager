#' @title SMA (R6 Object)
#'
#' @description R6 Class representing a seperately managed account.
#'  A seperately managed account is linked to a portfolio and contains
#'  Rules.
#'
#' @import R6
#' @import enfusion
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
    #' @param holdings_url Character. URL to Enfusion Holdings Report.
    #' @param trade_url Character. URL to Enfusion Trade Report.
    #' @param nav Numeric. SMA Net Asset Value.
    #' @param positions Optional. SMA Positions. Default is NULL.
    #' @param base_portfolio An object representing the base portfolio.
    #' @return A new instance of the SMA class.
    initialize = function(
      long_name, short_name, holdings_url, trade_url, nav, positions = NULL, base_portfolio = NULL
    ) {
      private$long_name_ <- long_name
      private$short_name_ <- short_name
      private$holdings_url_ <- holdings_url
      private$trade_url_ <- trade_url
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
    #' @param update_bbfields Update Bloomberg fields (default: TRUE)
    #' @param as.data.frame Return as data frame (default: TRUE)
    #' @importFrom tibble rownames_to_column
    #' @importFrom tibble column_to_rownames
    #' @importFrom dplyr full_join
    calc_proposed_rebalance_trade = function(
      security_id = NULL, 
      base_trade_qty = 0, 
      update_bbfields = TRUE,
      as.data.frame = TRUE
    ) {
      assert_bool(update_bbfields, "update_bbfields")
      if (update_bbfields) update_bloomberg_fields()
      constructor <- self$get_trade_constructor()
      rebal <- constructor$calc_rebalance_qty(self, security_id, base_trade_qty)

      .join_cols <- function(x, y) {
        df <- tibble::column_to_rownames(
          dplyr::full_join(
            tibble::rownames_to_column(x, "security_id"),
            tibble::rownames_to_column(y, "security_id"),
            by = "security_id"
          ),
          "security_id"
        )
      }

      .array_2_df <- function(x, name = NULL) {
        if (length(x) == 0) {
          setNames(data.frame(matrix(nrow=0,ncol=1), row.names = NULL), name)
        } else {
          setNames(data.frame(x), name)
        }
      }

      .list_array_2_df <- function(list) {
        lapply(
          seq_along(list),
          \(i) .array_2_df(list[[i]], names(list)[i])
        )
      }

      rebal_df <- Reduce(.join_cols, .list_array_2_df(rebal))
      if (as.data.frame) return(rebal_df)
      invisible(rebal_df)
    },

    #' @description mimic the base portfolio target position
    #' @param security_id Security ID
    #' @param assign_to_registry Assign to registry (default: TRUE)
    #' @param update_bbfields Update Bloomberg fields (default: FALSE)
    mimic_base_portfolio = function(
      security_id = NULL,
      update_bbfields = TRUE,
      assign_to_registry = TRUE 
    ) {

      rebal <- self$calc_proposed_rebalance_trade(
        security_id = security_id,
        update_bbfields = update_bbfields,
        as.data.frame = FALSE
      )

      if (assign_to_registry & length(rebal$trade_qty) != 0) {
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
      invisible(rebal_df)
    }
  )
)