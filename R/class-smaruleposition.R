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
    check_compliance = function(positions, tolerance = 1e-6) {
      ids <- vapply(positions, \(p) p$get_id(), character(1))
      qty <- vapply(positions, \(p) p$get_qty(), numeric(1))
      sma <- self$get_sma()
      d <- self$get_divisor()
      max_t <- self$get_max_threshold()
      min_t <- self$get_min_threshold()

      if (isTRUE(self$get_swap_only())) {
        need_swap <- self$check_swap_security(ids)
        is_swap <- vapply(positions, \(x) x$get_swap(), logical(1))
        non_comply <- ids[which(need_swap & !is_swap)]
        return(
          if (!length(non_comply)) {
            list("pass" = TRUE)
          } else {
            list("pass" = FALSE, "non_comply" = non_comply)
          }
        )
      }

      f <- self$apply_rule_definition(ids)
      f[!is.finite(f)] <- 0

      exp_i <- qty * as.numeric(f)
      denom <- d$value(self$get_sma(), ids, shares = qty)

      violates_max <- is.finite(max_t) & (exp_i > max_t * denom + tolerance)
      violates_min <- is.finite(min_t) & (exp_i < min_t * denom - tolerance)

      non_comply <- ids[which(violates_max | violates_min)]
      if (!length(non_comply)) {
        list("pass" = TRUE)
      } else {
        list(
          "pass" = FALSE,
          "violates_max" = any(violates_max),
          "violates_min" = any(violates_min),
          "non_comply" = non_comply,
          "divisor_kind" = d$kind,
          "divisor_value" = denom
        )
      }
    },
    #' @description Get the swap flag for a given security
    #' @param security_id Security ID
    check_swap_security = function(security_id) {
      if (!isTRUE(self$get_swap_only())) {
        swap <- vapply(security_id, \(x) FALSE, logical(1))
        return(swap)
      }
      self$apply_rule_definition(security_id)
    },
    #' @description Get the Max and Min Value of the security based on the rule
    #' @param security_id Security ID
    #' @return List of Max and Min Value
    get_security_limits = function(security_id) {
      d <- self$get_divisor()
      sma <- self$get_sma()
      nav <- sma$get_nav()
      max_t <- self$get_max_threshold()
      min_t <- self$get_min_threshold()

      if (d$kind == "nav") {
        exp <- self$apply_rule_definition(security_id)
        .set_ind_sec_limits <- function(e) {
          if (is.logical(e)) {
            if (isTRUE(e)) {
              return(list("max" = max_t, "min" = min_t))
            }
            return(list("max" = Inf, "min" = -Inf))
          }
          if (is.na(e) || e == 0) return(list("max" = Inf, "min" = -Inf))
          list(
            "max" = if (is.finite(max_t)) max_t / e else Inf,
            "min" = if (is.finite(min_t)) min_t / e else -Inf
          )
        }
        lim <- lapply(exp, .set_ind_sec_limits)
        names(lim) <- security_id
        return(lim)
      }

      pos <- sma$get_position()
      ids_all <- vapply(pos, \(p) p$get_id(), character(1))
      qty_all <- vapply(pos, \(p) p$get_qty(), numeric(1))
      price_all <- vapply(ids_all, \(id) .security(id)$get_price(), numeric(1))
      price_all[!is.finite(price_all) | price_all <= 0] <- 1
      w_all <- qty_all * (price_all / nav)
      contrib <- d$contrib_vec(w_all)
      denom_all <- sum(contrib)

      f_all <- self$apply_rule_definition(ids_all)
      f_all[!is.finite(f_all)] <- 0
      gamma_all <- as.numeric(f_all) / (price_all / nav)
      s_num_abs_all <- abs(gamma_all * w_all)

      # choose conservative c for net rules
      is_gross <- isTRUE(self$get_gross_exposure())
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
          "max" =  cap_w * nav / p_i,
          "min" = -cap_w * nav / p_i
        )
      })
      names(out) <- security_id
      out
    },
    #' @description Build the constraints for the optimization model
    #' @param ctx Context object with optimization variables and parameters
    build_constraints = function(ctx) {
      d <- self$get_divisor()
      kind <- d$kind %||% "nav"
      max_t <- self$get_max_threshold()
      min_t <- self$get_min_threshold()

      f <- self$apply_rule_definition(ctx$ids)

      if (is.logical(f)) {
        fnum <- ifelse(f, ctx$price / ctx$nav, 0)
      } else {
        fnum <- as.numeric(f)
        fnum[!is.finite(fnum)] <- 0
      }

      gamma <- ifelse(
        ctx$price > 0 & is.finite(ctx$price),
        fnum / (ctx$price / ctx$nav),
        0
      )

      idx <- which(abs(gamma) > 1e-12)
      if (!length(idx)) return(list())

      if (kind == "nav") {
        cons <- list()
        if (is.finite(max_t)) cons <- c(cons, list(gamma[idx] * ctx$w[idx] <= max_t))
        if (is.finite(min_t)) cons <- c(cons, list(gamma[idx] * ctx$w[idx] >= min_t))
        return(cons)
      }

      t_ <- CVXR::Variable(1, name = paste0("T_", self$get_name(), "_", kind))
      denom_cons <- switch(kind,
        "gmv" = list(CVXR::sum_entries(abs(ctx$w)) <= t_),
        "long_gmv" = list(CVXR::sum_entries(CVXR::pos(ctx$w)) <= t_),
        "short_gmv" = list(CVXR::sum_entries(CVXR::pos(-ctx$w)) <= t_),
        stop("Unknown divisor kind: ", kind)
      )
      cons <- denom_cons
      if (is.finite(max_t)) cons <- c(cons, list(gamma[idx] * ctx$w[idx] <= max_t * t_))
      if (is.finite(min_t)) cons <- c(cons, list(gamma[idx] * ctx$w[idx] >= min_t * t_))
      cons
    }
  )
)