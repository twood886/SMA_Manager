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
      private$check_rule_multi_position_(self$get_sma()$get_position())
    },
    #' @description Check the rule against the target holdings
    check_rule_target = function() {
      private$check_rule_multi_position_(self$get_sma()$get_target_position())
    },
    #' @description Check the swap rule against the current holdings
    check_swap_current = function() {
      private$check_swap_multi_position_(self$get_sma()$get_position())
    },
    #' @description Check the swap rule against the target holdings
    check_swap_target = function() {
      private$check_swap_multi_position_(self$get_sma()$get_target_position())
    },
    #' @description Get the Max and Min Value of the security based on the rule
    #' @param security_id Security ID
    #' @return List of Max and Min Value
    get_security_limits = function(security_id) {
      exp <- self$apply_rule_definition(security_id)
      max_t <- self$get_max_threshold()
      min_t <- self$get_min_threshold()

      .set_ind_sec_limits <- function(exp) {
        if (is.logical(exp)) {
          if (exp) {
            max  <- max_t
            min  <- min_t
          } else {
            max <- +Inf
            min <- -Inf
          }
          return(list("max" = max, "min" = min))
        }

        if (is.na(exp) || exp == 0) {
          return(list(max = Inf, min = -Inf))
        }

        list(
          max = if (is.finite(max_t)) max_t / exp else Inf,
          min = if (is.finite(min_t)) min_t / exp else -Inf
        )
      }

      setNames(lapply(exp, .set_ind_sec_limits), security_id)
    },
    #' @description Get the swap flag for a given security
    #' @param security_id Security ID
    check_swap_security = function(security_id) {
      if (!private$swap_only_) {
        return(setNames(sapply(security_id, \(x) FALSE), security_id))
      }
      self$apply_rule_definition(security_id)
    },
    #' Check violations for specific securities
    #' @param shares Named vector of shares
    #' @param tolerance Numerical tolerance
    check_violations = function(shares, tolerance = 1e-6) {
      factors <- self$apply_rule_definition(names(shares))
      max_t <- private$max_threshold_
      min_t <- private$min_threshold_
      violations <- list()
      for (i in seq_along(shares)) {
        sec <- names(shares)[i]
        if (is.na(factors[i]) || factors[i] == 0) next
        value <- factors[i] * shares[i]
        violates_max <- is.finite(max_t) && value > max_t + tolerance
        violates_min <- is.finite(min_t) && value < min_t - tolerance
        if (violates_max || violates_min) {
          violations[[sec]] <- list(
            security = sec,
            shares = shares[i],
            factors = factors[i],
            value = value,
            max_threshold = max_t,
            min_threshold = min_t,
            violates_max = violates_max,
            violates_min = violates_min,
            excess = if (violates_max) value - max_t else 0,
            shortfall = if (violates_min) min_t - value else 0
          )
        }
      }
      if (length(violations) > 0) {
        attr(violations, "rule_name") <- private$name_
        attr(violations, "scope") <- "position"
      }
      violations
    }
  ),
  private = list(
    check_rule_multi_position_ = function(positions) {
      portfolio <- self$get_sma()
      security_ids <- vapply(positions, \(x) x$get_security()$get_id(), character(1)) #nolint
      qtys <- vapply(positions, \(x) x$get_qty(), numeric(1))
      exp <- qtys * private$definition_(security_ids, portfolio)
      comply <- exp <= private$max_threshold_ & exp >= private$min_threshold_
      non_comply <- sapply(positions[which(!comply)], function(x) x$get_id())
      if (length(non_comply) == 0) {
        list("pass" = TRUE)
      } else {
        list("pass" = FALSE, "non_comply" = non_comply)
      }
    },
    check_swap_multi_position_ = function(positions) {
      security_ids <- vapply(positions, \(x) x$get_security()$get_id(), character(1)) #nolint
      swap_rule <- self$check_swap_security(security_ids)
      is_swap <- vapply(positions, \(x) x$get_swap(), logical(1))
      non_comply <- security_ids[which(swap_rule & !is_swap)]
      if (length(non_comply) == 0) {
        list("pass" = TRUE)
      } else {
        list("pass" = FALSE, "non_comply" = non_comply)
      }
    }
  )
)