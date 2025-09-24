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
    #' @param positions List of Position objects
    #' @return List of security IDs that do not comply with the rule
    check_compliance = function(positions) {
      # Get the security ids from the positions
      security_ids <- vapply(
        positions,
        \(x) x$get_security()$get_id(),
        character(1)
      )
      # Find non-compliant securities
      if (private$swap_only_) {
        need_swap <- self$check_swap_security(security_ids)
        is_swap <- vapply(positions, \(x) x$get_swap(), logical(1))
        non_comply <- security_ids[which(need_swap & !is_swap)]
      } else {
        qtys <- vapply(positions, \(x) x$get_qty(), numeric(1))
        exp <- qtys * self$get_definition()(security_ids, self$get_sma())
        comply <- (
          exp <= self$get_max_threshold() & exp >= self$get_min_threshold()
        )
        non_comply <- sapply(positions[which(!comply)], function(x) x$get_id())
      }
      # Return results
      if (length(non_comply) == 0) {
        list("pass" = TRUE)
      } else {
        list("pass" = FALSE, "non_comply" = non_comply)
      }
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
      lim <- lapply(exp, .set_ind_sec_limits)
      names(lim) <- security_id
      lim
    },
    #' @description Get the swap flag for a given security
    #' @param security_id Security ID
    check_swap_security = function(security_id) {
      if (!private$swap_only_) {
        swap <- sapply(security_id, \(x) FALSE)
        names(swap) <- security_id
        return(swap)
      }
      self$apply_rule_definition(security_id)
    },
    #' Check violations for specific securities
    #' @param shares Named vector of shares
    #' @param tolerance Numerical tolerance
    check_violations = function(shares, tolerance = 1e-6) {
      factors <- self$apply_rule_definition(names(shares))
      max_t <- self$get_max_threshold()
      min_t <- self$get_min_threshold()
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
    },
    #' @description Build the constraints for the optimization model
    #' @param ctx Context object with optimization variables and parameters
    build_constraints = function(ctx) {
      f <- self$apply_rule_definition(ctx$ids)
      if (is.logical(f)) return(list())
      f <- as.numeric(f)
      f[!is.finite(f)] <- NA_real_

      max_t <- self$get_max_threshold()
      min_t <- self$get_min_threshold()

      wmin <- rep(-Inf, length(ctx$ids))
      wmax <- rep(Inf, length(ctx$ids))
      hasf <- which(!is.na(f) & f != 0)
      if (length(hasf) > 0) {
        ratio <- (ctx$price[hasf] / ctx$nav) / f[hasf]
        if (is.finite(max_t)) wmax[hasf] <- max_t * ratio
        if (is.finite(min_t)) wmin[hasf] <- min_t * ratio
      }
      cons <- list()
      lo <- which(is.finite(wmin))
      hi <- which(is.finite(wmax))
      if (length(lo)) {
        cons <- c(cons, list(ctx$w[lo] >= wmin[lo]))
      }
      if (length(hi)) {
        hi <- which(is.finite(wmax))
        cons <- c(cons, list(ctx$w[hi] <= wmax[hi]))
      }
      cons
    }
  )
)