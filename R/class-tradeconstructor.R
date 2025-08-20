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

    #' Main optimization using CVXR
    #' @param sma SMA object
    #' @param lambda_alpha Regularization parameter for alpha
    #' @param tau_rel Small denominator to stabilize relative error
    #' @param beta_free Extra weight for "unaffected" names
    #' @param alpha_min Minimum value for alpha
    #' @param alpha_max Maximum value for alpha
    #' @param verbose Print progress
    optimize_sma = function(
      sma,
      lambda_alpha   = 10,
      tau_rel        = 1e-3,
      beta_free      = 5.0,
      alpha_min      = 0.8,
      alpha_max      = 5.0,
      verbose        = TRUE
    ) {
      if (verbose) {
        cat("\n=== SMA Optimization (relative-L2 in weight space) ===\n\n")
      }

      `%||%` <- function(x, y) if (is.null(x)) y else x

      # --- Data
      target_quantities <- self$calc_target_quantities(sma)
      current_positions <- private$.extract_qty(sma$get_target_position())
      replacements <- sma$get_replacement_security()

      all_securities <- unique(c(
        names(target_quantities),
        names(current_positions),
        unlist(replacements, use.names = FALSE)
      ))
      n <- length(all_securities)

      prices <- setNames(
        vapply(all_securities, \(s) .security(s)$get_price(), numeric(1)),
        all_securities
      )
      prices[!is.finite(prices) | prices <= 0] <- 1
      nav <- sma$get_nav()

      # align
      target_vec <- setNames(numeric(n), all_securities)
      for (sec in all_securities) {
        target_vec[sec] <- target_quantities[sec] %||% 0
      }

      price_vec <- setNames(as.numeric(prices[all_securities]), all_securities)
      t_w <- (price_vec * target_vec) / nav   # target weights (+/-/0)
      sgn <- sign(t_w)

      # share limits -> weight limits
      w_min <- rep(-Inf, n); names(w_min) <- all_securities
      w_max <- rep( Inf, n); names(w_max) <- all_securities
      for (i in seq_len(n)) {
        sec  <- all_securities[i]
        lims <- self$get_security_position_limits(sma, sec, TRUE)
        sh_max <- as.numeric((lims$max %||% (lims[[1]]$max %||% NA_real_)))
        sh_min <- as.numeric((lims$min %||% (lims[[1]]$min %||% NA_real_)))
        if (is.finite(sh_max)) w_max[i] <- (prices[sec] * sh_max) / nav
        if (is.finite(sh_min)) w_min[i] <- (prices[sec] * sh_min) / nav
      }

      # --- Variables
      w     <- CVXR::Variable(n, name = "w")      # weights
      alpha <- CVXR::Variable(1, name = "alpha")  # global scale

      # --- Constraints
      cons <- list(alpha >= alpha_min, alpha <= alpha_max)
      for (i in seq_len(n)) {
        cons <- c(cons, list(w[i] >= w_min[i], w[i] <= w_max[i]))
      }

      # zero-target names that are not overflow targets -> clamp to 0
      S_ids <- names(replacements)
      T_ids <- if (length(replacements)) {
        unique(unlist(replacements, use.names = FALSE)) 
      } else{
        character()
      }
      zero_idx <- which(abs(t_w) < 1e-12)
      if (length(zero_idx)) {
        zero_not_targets <- setdiff(zero_idx, match(T_ids, all_securities))
        zero_not_targets <- zero_not_targets[!is.na(zero_not_targets)]
        if (length(zero_not_targets)) {
          cons <- c(cons, list(w[zero_not_targets] == 0))
        }
      }

      # helper: convert rule f (shares-space %NAV) to gamma (weight space)
      .gamma_from_f <- function(f_raw, names_vec, nav, price_vec) {
        f <- setNames(numeric(length(names_vec)), names_vec)
        if (!is.null(names(f_raw))) {
          ov <- intersect(names(f_raw), names_vec)
          if (length(ov)) f[ov] <- as.numeric(f_raw[ov])
        } else if (length(f_raw) == length(names_vec)) {
          f <- as.numeric(f_raw)
          names(f) <- names_vec
        }
        f[!is.finite(f)] <- 0
        as.numeric((nav / price_vec) * f)
      }

      # portfolio rules
      rules <- sma$get_rules()
      rules <- Filter(function(r) !r$get_swap_only(), rules)
      rules_ptfl <- Filter(\(r) r$get_scope() == "portfolio", rules)

      # collect subset indices (true subsets only) to know who's "affected"
      subset_union <- logical(n)
      for (rule in Filter(\(r) isTRUE(r$get_gross_exposure()), rules_ptfl)) {
        f_raw <- rule$apply_rule_definition(all_securities)
        gamma <- .gamma_from_f(f_raw, all_securities, nav, price_vec)
        idx   <- which(abs(gamma) > 0)
        if (length(idx) > 0 && length(idx) < n) subset_union[idx] <- TRUE
        max_t <- rule$get_max_threshold()
        if (is.finite(max_t) && max_t > 0)
          cons <- c(cons, list(CVXR::sum_entries(abs(gamma * w)) <= max_t))
      }

      # net exposure rules
      for (rule in Filter(\(r) !isTRUE(r$get_gross_exposure()), rules_ptfl)) {
        f_raw <- rule$apply_rule_definition(all_securities)
        gamma <- .gamma_from_f(f_raw, all_securities, nav, price_vec)
        max_t <- rule$get_max_threshold(); min_t <- rule$get_min_threshold()
        port_val <- CVXR::sum_entries(gamma * w)
        if (is.finite(max_t)) cons <- c(cons, list(port_val <= max_t))
        if (is.finite(min_t)) cons <- c(cons, list(port_val >= min_t))
      }

      # overflow: directions + conservation (in weight space)
      if (length(replacements) > 0) {
        for (src in names(replacements)) {
          src_idx <- match(src, all_securities)
          tgt_ids <- as.character(replacements[[src]])
          tgt_idx <- match(tgt_ids, all_securities)
          tgt_idx <- tgt_idx[!is.na(tgt_idx)]
          if (is.na(src_idx) || length(tgt_idx)==0) next

          if (t_w[src_idx] >= 0) {
            cons <- c(cons, list(w[src_idx] <= alpha * t_w[src_idx]))
          } else {
            cons <- c(cons, list(w[src_idx] >= alpha * t_w[src_idx]))
          }
          for (j in tgt_idx) {
            if (t_w[j] >= 0) {
              cons <- c(cons, list(w[j] >= alpha * t_w[j]))
            } else {
              cons <- c(cons, list(w[j] <= alpha * t_w[j]))
            }
          }
          cons <- c(
            cons,
            list((alpha * t_w[src_idx] - w[src_idx]) == CVXR::sum_entries(w[tgt_idx] - alpha * t_w[tgt_idx]))
          )
        }
      }

      # --- Objective: relative-error L2; extra weight for "unaffected" names
      denom <- pmax(abs(t_w), tau_rel)  # vector (constants)
      base_err <- (w - alpha * t_w) / denom

      # "unaffected" = not in overflow sets and not in any subset gross rule
      affected <- (names(all_securities) %in% S_ids) | (names(all_securities) %in% T_ids) | subset_union
      free_idx <- which(!affected)

      term_base <- CVXR::sum_squares(base_err)
      term_free <- if (length(free_idx)) {
        beta_free * CVXR::sum_squares(base_err[free_idx])
      } else {
        0
      }
      net_tgt   <- sum(t_w)

      objective <- CVXR::Minimize(
        term_base + term_free +
        lambda_alpha * CVXR::square(alpha - 1) +
        10 * CVXR::square(CVXR::sum_entries(w) - net_tgt)   # soft net anchor
      )

      prob <- CVXR::Problem(objective, cons)

      # solve
      res <- tryCatch({
        CVXR::solve(
          prob, 
          solver = "OSQP",
          verbose = FALSE,
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
          verbose = FALSE,
          abstol = 1e-8, 
          reltol = 1e-8,
          feastol = 1e-8
        )
      })

      if (!(res$status %in% c("optimal","optimal_inaccurate","solved")))
        stop(sprintf("Optimization failed with status: %s", res$status))

      w_hat     <- setNames(as.numeric(res$getValue(w)), all_securities)
      alpha_hat <- as.numeric(res$getValue(alpha))
      sh  <- setNames((w_hat * nav) / price_vec,  all_securities)
      sh_final <- sh
      sh_final[sh > 0] <- floor(sh[sh > 0])
      sh_final[sh < 0] <- ceiling(sh[sh < 0])

      sf <- setNames(rep(NA_real_, n), all_securities)
      nz <- which(abs(t_w) > 1e-12)
      sf[nz] <- w_hat[nz] / t_w[nz]

      list(
        shares          = sh_final,
        weights         = w_hat,
        target_shares   = target_vec,
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
        {base_portfolio$get_target_position(base_security_id)$get_qty()},
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

    #' Calculate the target quantity for the SMA based on the base portfolio
    #' @param sma SMA object
    #' @param target_sec_id Security ID to calculate for
    #' @param base_trade_qty Base trade quantity to adjust
    calc_want_qty = function(
      sma, target_sec_id = NULL, base_trade_qty = 0
    ) {
      base <- sma$get_base_portfolio()
      base_qty <- private$.extract_qty(base$get_target_position())
      if (!is.null(target_sec_id) && length(target_sec_id) == 1) {
        base_qty[target_sec_id] <- base_qty[target_sec_id] + base_trade_qty
      }

      sma_qty <- private$.extract_qty(sma$get_target_position())
      if (is.null(target_sec_id)) target_sec_id <- names(base_qty)

      scale_ratio <- self$get_scale_ratio(base, sma)
      replacements <- sma$get_replacement_security()
      all_secs_id <- unique(c(
        names(base_qty),
        names(sma_qty),
        unlist(replacements, use.names = FALSE)
      ))

      want <- setNames(
        vapply(
          all_secs_id,
          function(s) {
            if (s %in% target_sec_id) {
              base_pos_qty <- tryCatch(base_qty[[s]], error = function(e) 0)
              return(scale_ratio * (base_qty[[s]] %||% 0))
            }
            if (s %in% names(sma_qty)) return(sma_qty[s])
            0
          },
          numeric(1)
        ),
        all_secs_id
      )
    },
    #' Calculate target quantities for the SMA based on scaled base portfolio
    #' @param sma SMA object
    calc_target_quantities = function(sma) {
      base <- sma$get_base_portfolio()
      scale_ratio <- self$get_scale_ratio(base, sma)

      # Get base positions
      base_positions <- private$.extract_qty(base$get_target_position())
      
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