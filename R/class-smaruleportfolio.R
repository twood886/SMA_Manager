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
      private$check_rule_position_(self$get_sma()$get_position())
    },
    #' @description Check the rule against the target portfolio
    check_rule_target = function() {
      private$check_rule_position_(self$get_sma()$get_target_position())
    },
    #' @description Check the swap rule against the current portfolio
    check_swap_current = function() list("pass" = TRUE),
    #' @description Check the swap rule against the target portfolio
    check_swap_target = function() list("pass" = TRUE),

    #' @description Get the swap flag for a given security
    #' @param security_id Security ID
    check_swap_security = function(security_id) {
      setNames(sapply(security_id, \(x) FALSE), security_id)
    },
    #' @description Get the Max and Min value of the security based on the rule
    #' @param security_id Security ID
    #' @return List of Max and Min Value
    get_security_limits = function(security_id) {
      positions <- self$get_sma()$get_position()
      applied_pos <- private$apply_rule_definition_positions_(positions)
      applied_pos[security_id] <- 0
      current_group_exp <- sum(applied_pos, na.rm = TRUE)

      exp_factors <- replace_na(self$apply_rule_definition(security_id), 0)

      remain_max_total <- private$max_threshold_ - current_group_exp
      remain_min_total <- private$min_threshold_ - current_group_exp

      limits <- lapply(exp_factors, function(e) {
        if (e == 0) return(list(max = Inf, min = -Inf))
        if (private$gross_exposure_) {
          cap <- max(0, remain_max_total) / e
          list(max = cap, min = -cap)
        } else {
          max_q <- max(0, remain_max_total / e)
          min_q <- min(0, remain_min_total / e)
          list(max = max_q, min = min_q)
        }
      })
      setNames(limits, security_id)
    },

    #' Check if a position set would violate this rule
    #' @param shares Named vector of shares
    #' @param tolerance Numerical tolerance for constraint checking
    check_violations = function(shares, tolerance = 1e-6) {
      factors <- self$apply_rule_definition(names(shares))
      factors[is.na(factors)] <- 0

      value <- sum(factors * shares)
      if (private$gross_exposure_) {
        value <- sum(factors * abs(shares))
      }
      max_t <- private$max_threshold_
      min_t <- private$min_threshold_

      list(
        rule_name = private$name_,
        scope = private$scope_,
        value = value,
        min_threshold = min_t,
        max_threshold = max_t,
        violates_max = is.finite(max_t) && value > max_t + tolerance,
        violates_min = is.finite(min_t) && value < min_t - tolerance,
        slack_to_max = if (is.finite(max_t)) max_t - value else Inf,
        slack_to_min = if (is.finite(min_t)) value - min_t else Inf,
        is_gross = private$gross_exposure_
      )
    }
  ),
  private = list(
    apply_rule_definition_positions_  = function(positions) {
      if (length(positions) == 0) return(numeric(0))
      security_ids <- sapply(positions, \(x) x$get_id())
      pos_qty <- vapply(positions, \(x) x$get_qty(), numeric(1))
      if (private$gross_exposure_) pos_qty <- abs(pos_qty)
      rule_applied <- pos_qty * self$apply_rule_definition(security_ids)
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