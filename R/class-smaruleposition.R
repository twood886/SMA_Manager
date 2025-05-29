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
      exp <- private$apply_rule_definition(security_id, private$get_sma_())
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
        return(setNames(lapply(security_id, \(x) FALSE), security_id))
      }
      private$apply_rule_definition(security_id, private$get_sma_())
    }
  ),
  private = list(
    get_sma_ = function() {
      get(private$sma_name_, envir = registries$portfolios, inherits = FALSE)
    },
    check_rule_multi_position_ = function(positions) {
      sma <- private$get_sma_()
      comply <- sapply(positions, private$check_rule_position_, sma = sma)
      non_comply <- sapply(positions[which(!comply)], function(x) x$get_id())
      if (length(non_comply) == 0) {
        result <- list("pass" = TRUE)
      } else {
        result <- list("pass" = FALSE, "non_comply" = not_comply)
      }
      result
    },
    check_swap_multi_position_ = function(positions) {
      swap_only <- sapply(positions, private$check_swap_position_)
      sapply(positions[which(swap_only)], function(x) x$get_id())
    },
    apply_rule_definition = function(security_id, sma) {
      setNames(
        as.list(private$definition_(security_id, sma)),
        security_id
      )
    }
  )
)
