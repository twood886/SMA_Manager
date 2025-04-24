#' @title SMA Rule for Positions
#'
#' @description R6 Class that encapsulates rule for SMAs
#'
#' @import R6
#' @include class-smarule.R
#'
#' @export
SMARulePosition <- R6::R6Class( #nolint
  "SMARulePosition",
  inherit = SMARule,
  public = list(
    #' Check Rule Against Current Holdings
    #' @description Check the rule against the current holdings
    check_rule_current = function() {
      private$check_rule_multi_position_(self$get_sma()$get_position())
    },
    #' Check Rule Against Target Holdings
    #' @description Check the rule against the target holdings
    check_rule_target = function() {
      private$check_rule_multi_position_(self$get_sma()$get_target_position())
    },
    #' Get Security Max Value
    #' @description Get the maximum value of the security based on the rule
    #' @param security_id Security ID
    #' @return Number of Shares
    get_security_max_value = function(security_id) {
      exp <- private$definition_(security_id)
      if (is.logical(exp)) {
        if (exp) {
          return(private$max_threshold_)
        } else {
          return(Inf)
        }
      }
      exp * max_threshold_
    },
    #' Get Security Min Value
    #' @description Get the minimum value of the security based on the rule
    #' @param security_id Security ID
    #' @return Number of Shares
    get_security_min_value = function(security_id) {
      exp <- private$definition_(security_id)
      if (is.logical(exp)) {
        if (exp) {
          return(private$min_threshold_)
        } else {
          return(-Inf)
        }
      }
      exp * min_threshold_
    },
  ),
  private = list(
    get_sma = function() {
      get(private$sma_name_, envir = .portfolio_registry, inherits = FALSE)
    },
    check_rule_position_ = function(position) {
      secruity_id <- position$get_security()$get_id()
      qty <- position$get_qty()
      exp <- qty * private$definition_(security_id)
      exp <= private$max_threshold_ & exp >= private$min_threshold_
    },
    check_rule_multi_position_ = function(positions) {
      comply <- sapply(positions, private$check_rule_position_)
      sapply(positions[which(!comply)], function(x) x$get_id())
    },
  )
)
