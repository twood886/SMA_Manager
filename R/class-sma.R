#' @title SMA (R6 Object)
#'
#' @description R6 Class representing a seperately managed account.
#'  A seperately managed account is linked to a portfolio and contains
#'  Rules.
#'
#' @import R6
#' @include class-portfolio.R
#' @include class-position.R
#' @include class-security.R
#' @include utils.R
#' @include api-functions.R
#' @include class-tradeconstructor.R
#' @include enfusion_loading.R
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
    #' @param nav Numeric. SMA Net Asset Value.
    #' @param positions Optional. SMA Positions. Default is NULL.
    #' @param base_portfolio An object representing the base portfolio.
    #' @return A new instance of the SMA class.
    initialize = function(
      long_name,
      short_name,
      holdings_url,
      nav,
      positions = NULL,
      base_portfolio = NULL
    ) {
      private$long_name_ <- long_name
      private$short_name_ <- short_name
      private$holdings_url_ <- holdings_url
      private$nav_ <- nav
      private$base_portfolio_ <- base_portfolio
      private$positions_ <- positions
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
    # Checkers -----------------------------------------------------------------
    #' Check Rule Compliance
    #' @description Check if the SMA is compliant with all its rules.
    #' @param update_bbfields Logical. Whether to update Bloomberg data before
    #'  checking rules. Defaults to TRUE.
    #' @param verbose Logical. Whether to print compliance results. Defaults to
    #'  FALSE.
    #' @import checkmate
    #' @return A list of rule compliance results.
    check_rule_compliance = function(update_bbfields = TRUE, verbose = TRUE) {
      checkmate::assert_logical(update_bbfields)
      checkmate::assert_logical(verbose)
      rules <- self$get_rules()
      if (length(rules) == 0) return(list())
      if (update_bbfields) SMAManager::update_bloomberg_fields()
      positions <- self$get_position()

      ids <- vapply(positions, \(p) p$get_id(), character(1))
      qty <- vapply(positions, \(p) p$get_qty(), numeric(1))
      is_swap <- vapply(positions, \(p) p$get_swap(), logical(1))
      nav <- self$get_nav()

      results <- lapply(
        rules,
        \(r) r$check_compliance(ids = ids, qty = qty, nav = nav, is_swap = is_swap) #nolint
      )
      if (verbose) return(results)

      non_comply_results <- Filter(\(x) !x$pass, results)

      if (length(non_comply_results) == 0) {
        return(list("pass" = TRUE))
      } else {
        non_comply <- lapply(non_comply_results, \(x) x$non_comply)
        return(list(
          "pass" = FALSE,
          "non_compliant" = non_comply
        ))
      }
    },
    # Replicators --------------------------------------------------------------
    #' Replicate a trade from the base portfolio
    #' @description
    #' Analyzes the effect of a trade in the base portfolio on the SMA,
    #' identifying the required trade.
    #' @param security_id The ID of the security traded in the base portfolio.
    #' @param base_trade_qty The quantity of the trade in the base portfolio.
    #' @param update_bbfields Logical. Update Bloomberg fields (default: TRUE).
    replicate_trade = function(
      security_id, base_trade_qty, update_bbfields = TRUE
    ) {
      checkmate::assert_character(security_id, len = 1)
      checkmate::assert_numeric(base_trade_qty, len = 1)
      checkmate::assert_flag(update_bbfields)
      if (update_bbfields) SMAManager::update_bloomberg_fields()
      self$get_trade_constructor()$replicate_trade(
        security_id = security_id,
        base_trade_qty = base_trade_qty,
        portfolio = self
      )
    }
  )
)