#' @title SMA Rule for Portfolio
#' @import R6
#' @include class-smarule.R
#' @export
SMARulePortfolio <- R6::R6Class( #nolint
  "SMARulePortfolio",
  inherit = SMARule,
  public = list(
    #' @description Check the rule against raw portfolio data
    #' @param ids Character vector of security IDs
    #' @param qty Numeric vector of quantities
    #' @param nav Numeric NAV value
    #' @param prices Optional numeric vector of prices. If NULL, fetched.
    #' @param tolerance Numerical tolerance for constraint checking
    #' @param ... Additional arguments (not used)
    check_compliance = function(
      ids, qty, nav, prices = NULL, tolerance = 1e-6, ...
    ) {
      if (is.null(prices)) {
        prices <- vapply(ids, \(id) .security(id)$get_price(), numeric(1))
      }
      prices[!is.finite(prices) | prices <= 0] <- 1
      w <- qty * prices / nav

      f <- self$apply_rule_definition(ids, nav)
      f[!is.finite(f)] <- 0
      gamma <- as.numeric(f) / (prices / nav)

      lhs <- if (isTRUE(self$get_gross_exposure())) {
        sum(abs(w * gamma))
      } else {
        sum(w * gamma)
      }

      d <- self$get_divisor()
      denom <- d$value_from_data(ids, qty, nav, prices)

      max_t <- self$get_max_threshold()
      min_t <- self$get_min_threshold()

      violates_max <- is.finite(max_t) && (lhs > max_t * denom + tolerance)
      violates_min <- is.finite(min_t) && (lhs < min_t * denom - tolerance)

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
    #' @description Get limits for securities based on raw portfolio data
    #' @param security_id Vector of security IDs to calculate limits for
    #' @param ids_all Character vector of all security IDs (must include
    #'  security_id). For new securities not in portfolio, include with qty=0.
    #' @param qty_all Numeric vector of quantities corresponding to ids_all
    #' @param nav Numeric NAV value
    #' @param prices_all Optional numeric vector of prices corresponding to
    #'  ids_all. If NULL, fetched via .security().
    #' @param f_all Optional numeric vector of rule values for security_id.
    #'  If NULL, computed via apply_rule_definition.
    #' @return List of Max and Min Value for each security
    get_security_limits = function(
      security_id, ids_all, qty_all, nav, prices_all = NULL, f_all = NULL
    ) {
      d <- self$get_divisor()

      if (is.null(f_all)) {
        f_all <- as.numeric(self$apply_rule_definition(ids_all, nav))
        f_all[!is.finite(f_all)] <- 0
      }

      prices_all <- if (!is.null(prices_all)) {
        prices_all
      } else {
        vapply(ids_all, \(id) {
          p <- .security(id)$get_price()
          if (!is.finite(p) || p <= 0) 1 else p
        }, numeric(1))
      }

      w_all <- qty_all * prices_all / nav

      denom_current <- d$value_from_data(ids_all, qty_all, nav, prices_all)
      denom_contrib_all <- d$contrib_vec(w_all)

      if (isTRUE(self$get_gross_exposure())) {
        num_contrib_all <- abs(f_all * qty_all)
      } else {
        num_contrib_all <- f_all * qty_all
      }
      num_current <- sum(num_contrib_all)

      max_t <- self$get_max_threshold()
      if (isTRUE(self$get_gross_exposure())) {
        min_t <- -max_t
      } else {
        min_t <- self$get_min_threshold()
      }

      out <- lapply(
        seq_along(security_id),
        function(i) {
          sec <- security_id[i]
          idx <- which(ids_all == sec)

          # Security must be in ids_all (caller's responsibility)
          if (!length(idx)) {
            stop("Security '", sec, "' not found in ids_all")
          }

          p_sec <- prices_all[idx]
          f_sec <- f_all[idx]
          if (f_sec == 0 | !is.finite(f_sec)) {
            return(list(max = Inf, min = -Inf))
          }

          gamma_vals <- d$gamma(p_sec, nav)
          gamma_pos <- gamma_vals$gamma_pos
          gamma_neg <- gamma_vals$gamma_neg

          num_sec <- num_contrib_all[idx]
          num_excl <- num_current - num_sec

          if (d$kind == "nav") {
            denom_excl <- 1
          } else {
            denom_sec <- denom_contrib_all[idx]
            denom_excl <- denom_current - denom_sec
          }

          list(
            max = (denom_excl - 1/max_t * num_excl) / (1/max_t * f_sec - gamma_pos), #nolint
            min = (denom_excl - 1/min_t * num_excl) / (1/min_t * f_sec - gamma_neg) #nolint
          )
        }
      )
      names(out) <- security_id
      out
    },
    #' @description Build the constraints for the optimization model
    #' @param ctx Context object with optimization variables and parameters
    #' @param nav Portfolio NAV
    build_constraints = function(ctx, nav) {
      f <- self$apply_rule_definition(ctx$ids, nav)
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
  )
)