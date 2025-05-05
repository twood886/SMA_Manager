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
    #' @description Check the rule against the current holdings
    check_rule_current = function() {
      positions <- private$get_sma_()$get_position()
      private$check_rule_multi_position_(positions)
    },
    #' @description Check the rule against the target holdings
    check_rule_target = function() {
      positions <- private$get_sma_()$get_target_position()
      private$check_rule_multi_position_(positions)
    },
    #' @description Check the swap rule against the current holdings
    check_swap_current = function() {
      positions <- private$get_sma_()$get_position()
      private$check_swap_multi_position_(positions)
    },
    #' @description Check the swap rule against the target holdings
    check_swap_target = function() {
      positions <- private$get_sma_()$get_position()
      private$check_swap_multi_position_(positions)
    },

    #' @description Get the Max and Min Value of the security based on the rule
    #' @param security_id Security ID
    #' @return List of Max and Min Value
    get_security_limits = function(security_id) {
      exp <- private$definition_(security_id, private$get_sma_())
      if (is.logical(exp)) {
        if (exp) {
          return(list(max = private$max_threshold_, min = private$min_threshold_))
        } else {
          return(list(max = Inf, min = -Inf))
        }
      }
      list(
        max = private$max_threshold_ / exp,
        min = private$min_threshold_ / exp
      )
    },
    #' @description Get the swap flag for a given security
    #' @param security_id Security ID
    check_swap_security = function(security_id) {
      if (!private$swap_only_) {
        return(FALSE)
      }
      private$definition_(security_id)
    }
  ),
  private = list(
    get_sma_ = function() {
      get(private$sma_name_, envir = registries$portfolios, inherits = FALSE)
    },
    check_rule_position_ = function(position, sma) {
      security_id <- position$get_security()$get_id()
      qty <- position$get_qty()
      exp <- qty * private$definition_(security_id, sma)
      exp <= private$max_threshold_ & exp >= private$min_threshold_
    },
    check_rule_multi_position_ = function(positions) {
      sma <- private$get_sma_()
      comply <- sapply(positions, private$check_rule_position_, sma = sma)
      sapply(positions[which(!comply)], function(x) x$get_id())
    },
    check_swap_position_ = function(position) {
      security_id <- position$get_security()$get_id()
      self$check_swap_security_(security_id)
    },
    check_swap_multi_position_ = function(positions) {
      swap_only <- sapply(positions, private$check_swap_position_)
      sapply(positions[which(swap_only)], function(x) x$get_id())
    }
  )
)
