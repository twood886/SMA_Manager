#' @title SMA Rule for Portfolio
#' @import R6
#' @include class-smarule.R
#' @export
SMARulePortfolio <- R6::R6Class( #nolint
  "SMARulePortfolio",
  inherit = SMARule,
  public = list(
    #' @description Check the rule against the current portfolio
    #' @param positions List of Position objects
    #' @param tolerance Numerical tolerance for constraint checking
    check_compliance = function(positions, tolerance = 1e-6) {
      ids   <- vapply(positions, \(p) p$get_id(),  character(1))
      qty   <- vapply(positions, \(p) p$get_qty(), numeric(1))
      sma   <- self$get_sma()
      nav   <- sma$get_nav()

      price <- vapply(ids, \(id) .security(id)$get_price(), numeric(1))
      price[!is.finite(price) | price <= 0] <- 1
      w <- qty * price / nav

      f <- self$apply_rule_definition(ids); f[!is.finite(f)] <- 0
      gamma <- as.numeric(f) / (price / nav)  # so gamma * w = f * qty

      lhs <- if (isTRUE(self$get_gross_exposure())) {
        sum(abs(w * gamma))
      } else {
        sum(w * gamma)
      }

      d <- self$get_divisor()
      denom <- d$value(sma, ids, shares = qty)

      max_t <- self$get_max_threshold()
      min_t <- self$get_min_threshold()

      violates_max <- is.finite(max_t) && (lhs > max_t * denom + tolerance)
      violates_min <- (!isTRUE(self$get_gross_exposure())) &&
        is.finite(min_t) &&
        (lhs < min_t * denom - tolerance)

      if (!(violates_max || violates_min)) {
        list(pass = TRUE)
      } else {
        violation_ids <- ids[which(f != 0)]
        list(
          pass          = FALSE,
          violates_max  = violates_max,
          violates_min  = violates_min,
          non_comply    = violation_ids,
          value         = lhs,
          divisor_kind  = d$kind,
          divisor_value = denom
        )
      }
    },
    #' @description Get the swap flag for a given security
    #' @param security_id Security ID
    check_swap_security = function(security_id) {
      vapply(security_id, \(x) FALSE, logical(1))
    },
    #' @description Get the Max and Min value of the security based on the rule
    #' @param security_id Security ID
    #' @return List of Max and Min Value
    get_security_limits = function(security_id) {
      d      <- self$get_divisor()
      sma    <- self$get_sma()
      nav    <- sma$get_nav()
      max_t  <- self$get_max_threshold()
      min_t  <- self$get_min_threshold()
      is_gross <- isTRUE(self$get_gross_exposure())

      # NAV: simple closed form
      if (identical(d$kind, "nav")) {
        exp_i <- self$apply_rule_definition(security_id)
        .set <- function(e) {
          if (is.logical(e)) {
            if (isTRUE(e)) return(list(max = max_t, min = min_t))
            return(list(max = Inf, min = -Inf))
          }
          if (is.na(e) || e == 0) return(list(max = Inf, min = -Inf))
          list(
            max = if (is.finite(max_t)) max_t / e else Inf,
            min = if (is.finite(min_t)) min_t / e else -Inf
          )
        }
        out <- lapply(exp_i, .set); names(out) <- security_id; return(out)
      }

      # GMV / long / short: others-frozen local caps
      pos       <- sma$get_position()
      ids_all   <- vapply(pos, \(p) p$get_id(),  character(1))
      qty_all   <- vapply(pos, \(p) p$get_qty(), numeric(1))
      price_all <- vapply(ids_all, \(id) .security(id)$get_price(), numeric(1))
      price_all[!is.finite(price_all) | price_all <= 0] <- 1
      w_all     <- qty_all * (price_all / nav)

      contrib   <- d$contrib_vec(w_all)
      denom_all <- sum(contrib)

      f_all <- self$apply_rule_definition(ids_all)
      f_all[!is.finite(f_all)] <- 0
      gamma_all <- as.numeric(f_all) / (price_all / nav)

      s_num_abs_all <- abs(gamma_all * w_all)

      c_star <- if (is_gross) {
        max_t
      } else {
        max(abs(max_t), abs(min_t), na.rm = TRUE)
      }

      out <- lapply(security_id, function(sec) {
        i <- match(sec, ids_all)
        contrib_i <- if (is.na(i)) 0 else contrib[i]
        s_den <- max(denom_all - contrib_i, 0)

        p_i <- .security(sec)$get_price()
        if (!is.finite(p_i) || p_i <= 0) p_i <- 1
        sc_i <- p_i / nav
        f_i  <- as.numeric(self$apply_rule_definition(sec))
        if (!is.finite(f_i)) f_i <- 0
        gamma_i <- if (sc_i != 0) f_i / sc_i else 0

        if (!is.finite(c_star) || c_star <= 0 || c_star >= 1 || gamma_i == 0) {
          cap_w <- Inf
        } else if (is_gross) {
          s_num_excl <- if (is.na(i)) {
            sum(s_num_abs_all)
          } else {
            sum(s_num_abs_all) - s_num_abs_all[i]
          }
          num <- c_star * s_den - s_num_excl
          den <- abs(gamma_i) - c_star
          cap_w <- if (den > 0) max(0, num / den) else if (num >= 0) Inf else 0
        } else {
          s_num_excl_abs <- if (is.na(i)) {
            sum(s_num_abs_all)
          } else {
            sum(s_num_abs_all) - s_num_abs_all[i]
          }
          num <- c_star * s_den - s_num_excl_abs
          den <- abs(gamma_i) - c_star
          cap_w <- if (den > 0) max(0, num / den) else if (num >= 0) Inf else 0
        }

        list(
          max =  cap_w * nav / p_i,
          min = -cap_w * nav / p_i
        )
      })
      names(out) <- security_id
      out
    },
    #' @description Build the constraints for the optimization model
    #' @param ctx Context object with optimization variables and parameters
    build_constraints = function(ctx) {
      f <- self$apply_rule_definition(ctx$ids)
      f <- as.numeric(f)
      f[!is.finite(f)] <- 0

      gamma <- f / (ctx$price / ctx$nav)
      idx <- which(abs(gamma) > 1e-12)
      if (length(idx) == 0) return(list())

      gross <- isTRUE(self$get_gross_exposure())
      lhs <- if (gross) {
        CVXR::sum_entries(abs(ctx$w[idx] * gamma[idx]))
      } else {
        CVXR::sum_entries(ctx$w[idx] * gamma[idx])
      }

      d <- self$get_divisor()
      dres <- d$expr(ctx)
      cons <- dres$cons
      max_t <- self$get_max_threshold()
      min_t <- self$get_min_threshold()

      if (is.finite(max_t)) {
        cons <- c(cons, list(lhs <= max_t * dres$expr))
      }

      if (!gross && is.finite(min_t)) {
        cons <- c(cons, list(lhs >= min_t * dres$expr))
      }
      cons
    }
  ),
  private = list(
    apply_rule_definition_positions_  = function(positions) {
      if (length(positions) == 0) return(numeric(0))
      security_ids <- sapply(positions, \(x) x$get_id())
      pos_qty <- vapply(positions, \(x) x$get_qty(), numeric(1))
      if (private$gross_exposure_) pos_qty <- abs(pos_qty)
      rule_applied <- pos_qty * self$apply_rule_definition(security_ids)
      names(rule_applied) <- security_ids
      rule_applied
    }
  )
)