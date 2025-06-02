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
    #' @return List of security IDs that do not comply with the rule
    check_rule_current = function() {
      portfolio <- private$get_portfolio_()
      positions <- portfolio$get_position()
      private$check_rule_multi_position_(positions)
    },
    #' @description Check the rule against the target holdings
    check_rule_target = function() {
      portfolio <- private$get_portfolio_()
      positions <- portfolio$get_target_position()
      private$check_rule_multi_position_(positions)
    },
    #' @description Check the swap rule against the current holdings
    check_swap_current = function() {
      portfolio <- private$get_portfolio_()
      positions <- portfolio$get_position()
      private$check_swap_multi_position_(positions)
    },
    #' @description Check the swap rule against the target holdings
    check_swap_target = function() {
      portfolio <- private$get_portfolio_()
      positions <- portfolio$get_target_position()
      private$check_swap_multi_position_(positions)
    },

    #' @description Get the Max and Min Value of the security based on the rule
    #' @param security_id Security ID
    #' @return List of Max and Min Value
    get_security_limits = function(security_id) {
      exp <- private$apply_rule_definition_(security_id, private$get_portfolio_()) #nolint
      .set_ind_sec_limits <- function(exp) {
        if (is.logical(exp)) {
          if (exp) {
            max  <- private$max_threshold_
            min  <- private$min_threshold_
          } else {
            max <- +Inf
            min <- -Inf
          }
          return(list("max" = max, "min" = min))
        } else {
          max <- private$max_threshold_ / exp
          min <- private$min_threshold_ / exp
        }
        list("max" = max, "min" = min)
      }
      lapply(exp, .set_ind_sec_limits)
    },
    #' @description Get the swap flag for a given security
    #' @param security_id Security ID
    check_swap_security = function(security_id) {
      if (!private$swap_only_) {
        return(setNames(sapply(security_id, \(x) FALSE), security_id))
      }
      private$apply_rule_definition_(security_id, private$get_portfolio_())
    }
  ),
  private = list(
    get_portfolio_ = function() {
      get(private$sma_name_, envir = registries$portfolios, inherits = FALSE)
    },
    check_rule_multi_position_ = function(positions) {
      portfolio <- private$get_portfolio_()
      security_ids <- vapply(positions, \(x) x$get_security()$get_id(), character(1)) #nolint
      qtys <- vapply(positions, \(x) x$get_qty(), numeric(1))
      exp <- qtys * private$definition_(security_ids, portfolio)
      comply <- exp <= private$max_threshold_ & exp >= private$min_threshold_
      non_comply <- sapply(positions[which(!comply)], function(x) x$get_id())
      if (length(non_comply) == 0) {
        result <- list("pass" = TRUE)
      } else {
        result <- list("pass" = FALSE, "non_comply" = non_comply)
      }
      result
    },
    check_swap_multi_position_ = function(positions) {
      security_ids <- vapply(positions, \(x) x$get_security()$get_id(), character(1)) #nolint
      swap_rule <- self$check_swap_security(security_ids)
      is_swap <- vapply(positions, \(x) x$get_swap(), logical(1))
      non_comply <- security_ids[which(swap_rule & !is_swap)]
      if (length(non_comply) == 0) {
        result <- list("pass" = TRUE)
      } else {
        result <- list("pass" = FALSE, "non_comply" = non_comply)
      }
      result
    },
    apply_rule_definition_ = function(security_id, portfolio) {
      setNames(
        private$definition_(security_id, portfolio),
        security_id
      )
    }
  )
)