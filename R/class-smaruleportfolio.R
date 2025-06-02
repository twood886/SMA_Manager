#' @title SMA Rule for Portfolio
#' @import R6
#' @include class-smarule.R
#' @export
SMARulePortfolio <- R6::R6Class( #nolint
  "SMARulePortfolio",
  inherit = SMARule,
  public = list(
    #' @description Check the rule against the current portfolio
    check_rule_current = function() {
      private$check_rule_position_(private$get_sma_()$get_position())
    },
    #' @description Check the rule against the target portfolio
    check_rule_target = function() {
      private$check_rule_position_(private$get_sma_()$get_target_position())
    },
    #' @description Check the swap rule against the current portfolio
    check_swap_current = function() {
      list("pass" = TRUE)
    },
    #' @description Check the swap rule against the target portfolio
    check_swap_target = function() {
      list("pass" = TRUE)
    },
    #' @description Get the Max and Min value of the security based on the rule
    #' @param security_id Security ID
    #' @return List of Max and Min Value
    get_security_limits = function(security_id) {
      positions <- private$get_sma_()$get_position()
      rule_applied_pos <- private$apply_rule_definition_positions_(positions)
      rule_applied_pos[security_id] <- 0
      v <- sum(rule_applied_pos, na.rm = TRUE)
      exp <- private$apply_rule_definition_(security_id)

      remain_max <- (private$max_threshold_ - v) / length(security_id)
      remain_min <- (private$min_threshold_ - v) / length(security_id)

      if (private$gross_exposure_) {
        .set_ind_sec_limits <- function(exp) {
          if (exp == 0) {
            max <- +Inf
          } else {
            max <- max(0, remain_max) / exp
          }
          list("max" = max, "min" = -max)
        }
      } else {
        .set_ind_sec_limits <- function(exp) {
          if (exp == 0) {
            max <- +Inf
            min <- -Inf
          } else {
            max <- max(0, remain_max / exp)
            min <- min(0, remain_min / exp)
          }
          list("max" = max, "min" = min)
        }
      }
      setNames(lapply(exp, .set_ind_sec_limits), security_id)
    }
  ),
  private = list(
    get_sma_ = function() {
      get(private$sma_name_, envir = registries$portfolios, inherits = TRUE)
    },
    apply_rule_definition_ = function(security_id) {
      private$definition_(security_id, private$get_sma_())
    },
    apply_rule_definition_positions_  = function(positions) {
      security_ids <- sapply(positions, \(x) x$get_id())
      pos_qty <- vapply(positions, \(x) x$get_qty(), numeric(1))
      if (private$gross_exposure_) {
        pos_qty <- abs(pos_qty)
      }
      rule_applied <- pos_qty * private$apply_rule_definition_(security_ids)
      setNames(rule_applied, security_ids)
    },
    check_rule_position_ = function(positions) {
      rule_applied <- private$apply_rule_definition_positions_(positions)
      v <- sum(rule_applied, na.rm = TRUE)
      comply <- v <= private$max_threshold_ && v >= private$min_threshold_
      list("pass" = comply)
    }
  )
)