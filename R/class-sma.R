#' @title SMA (R6 Object)
#'
#' @description R6 Class representing a seperately managed account.
#'  A seperately managed account is linked to a portfolio and contains
#'  Sma_Rules.
#'
#' @import R6
#' @import enfusion
#' @import parallel
#' @include class-portfolio.R
#' @include class-position.R
#' @include class-security.R
#' @include utils.R
#' @include api-functions.R
#' @include class-smaconstructor.R
#' @export
SMA <- R6::R6Class(   #nolint
  "SMA",
  inherit = Portfolio,
  private = list(
    base_portfolio_ = NULL,
    sma_rules_ = list(),
    replacements_ = list(),
    portfolio_constructor = SMAConstructor$new()
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
      private$sma_rules_ <- list()
      private$replacements_ <- list()
    },

    #' Add SMA Rule
    #' @description Create SMA Rule and Add to SMA
    #' @param rule An object of class SMARule
    add_rule = function(rule) {
      assert_inherits(rule, "SMARule", "rule")
      private$sma_rules_[[rule$get_name()]] <- rule
      invisible(self)
    },

    #' Add Replacement
    #' @description Add replacement securitity
    #' @param original_security The original Security id
    #' @param replacement_security The replacement Security id
    add_replacement = function(
      original_security = NULL, replacement_security = NULL
    ) {
      if (is.null(original_security) | is.null(replacement_security)) {
        stop("Securities must be provided")
      }
      original_security <- tolower(original_security)
      replacement_security <- tolower(replacement_security)
      private$replacements_[[original_security]] <- replacement_security
      invisible(self)
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

    #' @description Get the SMA Rules
    #' @return A list of SMA rules
    get_sma_rules = function() {
      if (length(private$sma_rules_) == 0) stop("No SMA rules defined")
      private$sma_rules_
    },

    #' @description Get the Portfolio Constructor
    #' @return The portfolio constructor object
    get_portfolio_constructor = function() {
      private$portfolio_constructor
    },

    #' @description Get replacement security for a given replaced security
    #' @param replaced_security_id Security ID of the replaced security (in base ptfl) #nolint
    get_replacement_security = function(replaced_security_id = NULL) {
      if (is.null(replaced_security_id)) return(private$replacements_)
      if (!replaced_security_id %in% names(private$replacements_)) {
        return(replaced_security_id)
      }
      private$replacements_[[replaced_security_id]]
    },

    #' @description Get replaced security for a given replacement security
    #' @param replacement_security_id Security ID of the replacement security (in SMA) #nolint
    get_replaced_security = function(replacement_security_id = NULL) {
      if (is.null(replacement_security_id)) names(private$replacements_)
      u <- unlist(private$replacements_, use.names = TRUE)
      idx <- which(u == replacement_security_id)
      if (length(idx) == 0) return(NULL)
      names(u)[idx]
    },

    #' @description Check SMA rules against the target positions
    check_sma_rules_target = function() {
      lapply(self$get_sma_rules_, function(x) x$check_rule_target())
    },

    #' @description Get Max and Min Value of the security given all SMA Rules
    #' @param security_id Security ID
    get_security_position_limits = function(security_id = NULL) {
      private$portfolio_constructor$get_security_position_limits(self, security_id)
    },

    #' @description Get Swap Flag for a given security
    #' @param security_id Security ID
    get_swap_flag_position_rules = function(security_id = NULL) {
      private$portfolio_constructor$get_swap_flag_position_rules(self, security_id)
    },

    #' @description Calculate the rebalance quantity for a given security
    #' @param security_id Security ID
    mimic_base_portfolio = function(security_id = NULL) {
      constructor <- self$get_portfolio_constructor()
      rebal <- constructor$calc_rebalance_qty(self$get_base_portfolio(), self, security_id)
      if (length(rebal$trade_qty) != 0) {
        swap <- constructor$get_swap_flag_position_rules(self, names(rebal$trade_qty))
        for (i in seq_along(rebal$trade_qty)) {
          t <- .trade(
            security_id = names(rebal$trade_qty)[i],
            portfolio_id = self$get_short_name(),
            qty = rebal$trade_qty[[i]],
            swap = swap[[names(rebal$trade_qty)[i]]],
            create = TRUE
          )
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
      invisible(self)
    }
  )
)