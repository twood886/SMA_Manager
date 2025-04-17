#' @title SMA (R6 Object)
#'
#' @description R6 Class representing a seperately managed account.
#'  A seperately managed account is linked to a portfolio and contains
#'  Sma_Rules.
#'
#' @import R6
#' @import enfusion
#' @include create_position.R
#' @include class-portfolio.R
#' @include get_portfolio.R
#'
SMA <- R6::R6Class(   #nolint
  "SMA",
  inherit = Portfolio,
  public = list(

    #' @description
    #' Create a new SMA R6 object.
    #' @param long_name Character. SMA Long Name.
    #' @param short_name Character. SMA Short Name.
    #' @param nav Numeric. SMA Net Asset Value.
    #' @param target_portfolio An object representing the target portfolio.
    #' @param positions Optional. SMA Positions. Default is NULL.
    #' @return A new instance of the SMA class.
    initialize = function(
      long_name, short_name, nav, target_portfolio = NULL, positions = NULL
    ) {
      private$id_ <- length(ls(envir = .sma_registry)) + 1
      private$long_name_ <- long_name
      private$short_name_ <- short_name
      private$nav_ <- nav
      private$target_portfolio_ <- target_portfolio
      private$positions_ <- positions
      private$target_positions_ <- positions
      private$sma_rules_ <- list()
    },

    #' Add SMA Rule
    #' @description Create SMA Rule and Add to SMA
    #' @param rule_name Character representing identifing name of rule
    #' @param rule_scope Character representing the scope of the rule
    #' @param rule_formula formula representing the rule
    add_rule = function(rule_name, rule_scope, rule_formula) {
      if (is.null(rule_name)) {
        stop("rule_name must be supplied")
      }
      if (is.null(rule_scope)) {
        stop("rule_scope must be supplied")
      }
      if (is.null(rule_formula) || class(rule_formula) != "function") {
        stop("rule_formula must be supplied")
      }
      rule <- new(
        "sma_rule",
        sma_id = self$id,
        rule_name = rule_name,
        rule_scope = rule_scope,
        rule_formula = rule_formula
      )
      private$sma_rules_[[rule_name]] <- rule
      invisible(self)
    },

    #' Add Replacement
    #' @description Add replacement securitity
    #' @param original_security The original Security id
    #' @param replacement_security The replacement Security id
    add_replacement = function(
      original_security = NULL, replacement_security = NULL
    ) {
      if (is.null(original_security) | is.null(replacment_security)) {
        stop("Securities must be provided")
      }
      invisible(self)
    },

    #' Get Target Portfolio
    #' @description Get the tagret portfolio
    get_target_portfolio = function() {
      get_portfolio(private$target_portfolio_)
    },

    #' Get Target Portfolio Position
    #' @description Get a position in the target portfolio
    #' @param security_id Security ID
    get_target_portfolio_position = function() {
      get_portfolio(private$target_portfolio_)$get_position(security_id)
    },

    #' Compare Weights to Target Portfolio
    #' @description Compare the current weight in the sma
    #' @param security_id Security ID
    #' to the current weight in the target portfolio
    compare_weight_current = function(security_id = NULL) {
      self$get_position(security_id)
    }

  ),
  private = list(
    target_portfolio_ = NULL,
    sma_rules_ = list(),
    replacements_ = list()
  )
)