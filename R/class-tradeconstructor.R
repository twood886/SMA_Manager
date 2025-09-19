#' @title Trade Constructor Parent Class
#' @import CVXR
#' @import R6
#' @include class-portfolio.R
#' @include class-smarule.R
#' @include class-smaruleposition.R
#' @include class-sma.R
#' @include class-security.R
#' @export
TradeConstructor <- R6::R6Class( #nolint
  "TradeConstructor",
  public = list(
    #' @description Calculate target quantities for the trade constructor
    #' @param portfolio An object of class Portfolio
    #' @param security_id A character vector of security IDs
    #' @param position_only Logical, if TRUE only consider position rules
    get_security_position_limits = function(
      portfolio,
      security_id = NULL,
      position_only = FALSE
    ) {
      if (is.null(security_id)) stop("Security ID must be supplied")
      rules <- Filter(\(r) !r$get_swap_only(), portfolio$get_rules())

      if (position_only) {
        rules <- Filter(\(r) r$get_scope() == "position", rules)
      }

      limits <- lapply(rules, \(r) r$get_security_limits(security_id))

      setNames(
        lapply(
          security_id,
          \(sec) {
            sec_limit <- lapply(limits, \(limit) limit[[sec]])
            if (length(sec_limit) == 0) return(list(max = Inf, min = -Inf))
            max_limit <- min(sapply(sec_limit, \(x) x$max), na.rm = TRUE)
            min_limit <- max(sapply(sec_limit, \(x) x$min), na.rm = TRUE)
            list(max = max_limit, min = min_limit)
          }
        ),
        security_id
      )
    },
    #' @description Identify securities that are part of any swap rule
    #' @param portfolio An object of class Portfolio
    #' @param security_id A character vector of security IDs
    get_swap_flag_position_rules = function(portfolio, security_id = NULL) {
      if (is.null(security_id)) stop("Security ID must be supplied")
      rules <- portfolio$get_rules()
      swaps <- lapply(rules, \(rule) rule$check_swap_security(security_id))
      setNames(
        lapply(
          security_id,
          \(sec) any(vapply(swaps, \(swap) swap[[sec]], logical(1)))
        ),
        security_id
      )
    },
    #' @description Build the Optimzation Model Context (called by optimize_sma)
    #' @param ids Character vector of security IDs
    #' @param price_vec Numeric vector of security prices
    #' @param nav Numeric, portfolio NAV
    #' @param t_w Numeric vector of target weights (+/-/0)
    #' @param params List of parameters (lambda_alpha, tau_rel, etc.)
    make_model_context = function(ids, price_vec, nav, t_w, params) {
      n  <- length(ids)
      vf <- VariableFactory$new()
      w  <- CVXR::Variable(n, name = "w")
      alpha <- CVXR::Variable(1, name = "alpha")
      index_of <- function(id) match(id, ids)
      ModelContext$new(
        n = n,
        ids = ids,
        price = price_vec,
        nav = nav,
        t_w = t_w,
        sgn = sign(t_w),
        w = w,
        alpha = alpha,
        params = params,
        index_of = index_of,
        var_factory = vf
      )
    },


    #' Main optimization using CVXR
    #' @param portfolio An object of class Portfolio
    #' @param lambda_alpha Regularization parameter for alpha
    #' @param tau_rel Small denominator to stabilize relative error
    #' @param beta_free Extra weight for "unaffected" names
    #' @param alpha_min Minimum value for alpha
    #' @param alpha_max Maximum value for alpha
    #' @param verbose Print progress
    #' @importFrom stats setNames
    optimize_sma = function(
      portfolio,
      lambda_alpha   = 10,
      tau_rel        = 1e-3,
      beta_free      = 5.0,
      alpha_min      = 0.8,
      alpha_max      = 5.0
    ) {
      tgt_qty <- self$calc_target_quantities(portfolio)
      current_pos <- private$.extract_qty(portfolio$get_position())
      replacements <- portfolio$get_replacement_security()
      s_ids <- names(replacements)
      t_ids <- if (length(replacements)) {
        unique(unlist(replacements, use.names = FALSE))
      } else {
        character(0)
      }

      sec_ids <- unique(c(names(tgt_qty), names(current_pos), t_ids))
      n <- length(sec_ids)

      price_vec <- vapply(sec_ids, \(s) .security(s)$get_price(), numeric(1))
      price_vec[!is.finite(price_vec) | price_vec <= 0] <- 1
      nav <- portfolio$get_nav()

      tgt_qty <- vapply(sec_ids, \(s) tgt_qty[s] %||% 0, numeric(1))
      t_w <- (price_vec * tgt_qty) / nav
      sgn <- sign(t_w)

      # Context ----------------------------------------------------------------
      params <- list(
        lambda_alpha = lambda_alpha,
        tau_rel = tau_rel,
        beta_free = beta_free,
        alpha_min = alpha_min,
        alpha_max = alpha_max
      )
      ctx <- self$make_model_context(sec_ids, price_vec, nav, t_w, params)
      w <- ctx$w
      alpha <- ctx$alpha

      # --- Base constraints (global box on alpha) -----------------------------
      cons <- list(alpha >= alpha_min, alpha <= alpha_max)
      # Zero-target names that are not overflow targets -> clamp to 0
      zero_not_targets <- which(abs(t_w) < 1e-12 & !(sec_ids %in% t_ids))
      if (length(zero_not_targets)) {
        cons <- c(cons, list(w[zero_not_targets] == 0))
      }

      # --- Let rules contribute constraints -----------------------------------
      rules <- portfolio$get_rules()
      if (length(replacements) > 0) {
        rules <- c(rules, list(OverflowRule$new(replacements)))
      }
      rule_cons  <- unlist(
        lapply(rules, \(r) r$build_constraints(ctx)),
        recursive = FALSE
      )
      cons <- c(cons, rule_cons)

      # --- Objective ----------------------------------------------------------
      denom <- pmax(abs(t_w), tau_rel)
      base_err <- (w - alpha * t_w) / denom

      affected <- (sec_ids %in% s_ids) | (sec_ids %in% t_ids)
      free_idx <- which(!affected)

      term_base <- CVXR::sum_squares(base_err)
      term_free <- if (length(free_idx)) {
        beta_free * CVXR::sum_squares(base_err[free_idx])
      } else {
        0
      }
      net_tgt <- sum(t_w)
      rule_terms <- unlist(
        lapply(rules, \(r) r$objective_terms(ctx)),
        recursive = FALSE
      )
      rule_obj_sum <- if (length(rule_terms)) Reduce(`+`, rule_terms) else 0

      objective <- CVXR::Minimize(
        term_base
        + term_free
        + rule_obj_sum
        + lambda_alpha
        * CVXR::square(alpha - 1)
        + 10 * CVXR::square(CVXR::sum_entries(w) - net_tgt)
      )

      prob <- CVXR::Problem(objective, cons)

      # --- Solve --------------------------------------------------------------
      res <- tryCatch({
        CVXR::solve(
          prob,
          solver = "OSQP",
          verbose = verbose,
          eps_abs = 1e-8,
          eps_rel = 1e-8,
          max_iter = 200000,
          polish = TRUE
        )
      }, error = function(e) {
        if (verbose) cat("OSQP failed; trying ECOS...\n")
        CVXR::solve(
          prob,
          solver = "ECOS",
          verbose = verbose,
          abstol = 1e-8,
          reltol = 1e-8,
          feastol = 1e-8
        )
      })

      if (!(res$status %in% c("optimal", "optimal_inaccurate", "solved")))
        stop(sprintf("Optimization failed with status: %s", res$status))

      w_hat     <- setNames(as.numeric(res$getValue(w)), sec_ids)
      alpha_hat <- as.numeric(res$getValue(alpha))
      sh        <- setNames((w_hat * nav) / price_vec,  sec_ids)
      sh_final  <- sh
      sh_final[sh > 0] <- floor(sh[sh > 0])
      sh_final[sh < 0] <- ceiling(sh[sh < 0])

      sf <- setNames(rep(NA_real_, n), sec_ids)
      nz <- which(abs(t_w) > 1e-12)
      sf[nz] <- w_hat[nz] / t_w[nz]

      list(
        shares          = sh_final,
        weights         = w_hat,
        target_shares   = tgt_qty,
        target_weights  = t_w,
        alpha_hat       = alpha_hat,
        scaling_factors = sf,
        objective_value = res$value,
        status          = res$status
      )


    }
  ),
  private = list(
    .extract_qty = function(positions) {
      if (length(positions) == 0) return(numeric(0))
      ids <- vapply(positions, function(x) x$get_id(), character(1))
      qty <- vapply(positions, function(x) x$get_qty(), numeric(1))
      # Clean up non-finite values
      if (any(!is.finite(qty))) {
        qty[!is.finite(qty)] <- 0
      }
      setNames(qty, ids)
    }
  )
)




#' @title SMA Trade Constructor Class
#' @import dplyr
#' @import purrr
#' @import tibble
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na
#' @importFrom R6 R6Class
#' @include class-security.R
#' @include class-portfolio.R
#' @include class-smarule.R
#' @include class-smaruleposition.R
#' @export
SMAConstructor <- R6::R6Class( #nolint
  "SMAConstructor",
  inherit = TradeConstructor,
  public = list(
    #' @description Get the scale quantity for a security in the SMA
    #' @param base_portfolio Base portfolio object
    #' @param sma_portfolio SMA portfolio object
    #' @param base_security_id Security ID in the base portfolio
    get_scale_qty = function(base_portfolio, sma_portfolio, base_security_id) {
      base_pos_qty <- tryCatch(
        {base_portfolio$get_position(base_security_id)$get_qty()},
        error = function(e) 0
      )
      nav_ratio <- sma_portfolio$get_nav() / base_portfolio$get_nav()
      base_pos_qty * nav_ratio
    },
    #' @description Get the scale ratio for the SMA
    #' @param base_portfolio Base portfolio object
    #' @param sma_portfolio SMA portfolio object
    get_scale_ratio = function(base_portfolio, sma_portfolio) {
      base_nav <- base_portfolio$get_nav()
      sma_nav  <- sma_portfolio$get_nav()
      if (base_nav == 0) return(0)
      sma_nav / base_nav
    },
    #' Calculate target quantities for the SMA based on scaled base portfolio
    #' @param sma SMA object
    calc_target_quantities = function(sma) {
      base <- sma$get_base_portfolio()
      scale_ratio <- self$get_scale_ratio(base, sma)
      # Get base positions
      base_positions <- private$.extract_qty(base$get_position())
      # Scale all base positions by NAV ratio
      target_quantities <- base_positions * scale_ratio
      # Clean up non-finite values
      if (any(!is.finite(target_quantities))) {
        target_quantities[!is.finite(target_quantities)] <- 0
      }
      # Include replacement securities
      replacements <- sma$get_replacement_security()
      replacement_secs <- unlist(replacements, use.names = FALSE)
      for (sec in replacement_secs) {
        if (!(sec %in% names(target_quantities))) {
          target_quantities[sec] <- 0
        }
      }
      target_quantities
    }
  )
)