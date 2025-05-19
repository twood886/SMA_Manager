#' Portfolio Constructor
#' @export
TradeConstructor <- R6::R6Class( #nolint
  "TradeConstructor",
  public = list(
    #' @param portfolio An object of class Portfolio
    #' @param security_id A character vector of security IDs
    get_security_position_limits = function(portfolio, security_id = NULL) {
      if (is.null(security_id)) stop("Security ID must be supplied")
      rules <- portfolio$get_rules()
      non_swap_rules <- rules[!vapply(rules, \(rule) rule$get_swap_only(), logical(1))]

      limits <- lapply(non_swap_rules, \(rule) rule$get_security_limits(security_id))

      setNames(
        lapply(
          security_id,
          \(sec) {
            sec_limit <- lapply(limits, \(limit) limit[[sec]])
            if (length(sec_limit) == 0) return(list(max = Inf, min = -Inf))
            max_limit <- min(sapply(sec_limit, \(x) x$max))
            min_limit <- max(sapply(sec_limit, \(x) x$min))
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
    #' @param portfolio An object of class Portfolio
    #' @param target_sec_id A character vector of target security IDs
    #' @param target_trade_qty A numeric vector of target trade quantities
    calc_trade_qty = function(portfolio, target_sec_id = NULL, target_trade_qty = 0) {
      assert_string(target_sec_id, "target_sec_id")
      assert_numeric(target_trade_qty, "target_trade_qty")
      if(length(target_sec_id) != length(target_trade_qty)) {
        stop("target_sec_id and target_trade_qty must be of same length")
      }
      names(target_trade_qty) <- target_sec_id
      portfolio_qty <- private$.extract_qty(portfolio$get_target_position())
      replacements <- portfolio$get_replacement_security()
      
      all_secs_id <- unique(c(target_sec_id, names(portfolio_qty), unlist(replacements, use.names = FALSE)))
      want <- setNames(
        vapply(
          all_secs_id,
          function(s) {
            if (s %in% target_sec_id) {
              existing_qty <- tryCatch(portfolio_qty[[s]], error = function(e) 0)
              return(existing_qty + target_trade_qty[s])
            } 
            if (s %in% names(portfolio_qty)) return(portfolio_qty[s])
            0
          },
          numeric(1)
        ),
        all_secs_id
      )
      private$.calc_flow_from_want(portfolio, want)
    }
  ),
  private = list(
    .extract_qty = function(pos_list) {
      ids <- vapply(pos_list, function(x) x$get_id(), character(1))
      qty <- vapply(pos_list, function(x) x$get_qty(), numeric(1))
      setNames(qty, ids)
    },

    .run_side_ = function(side, ov_val, clamped, prices, rules, repls) {
      sign <- if (side == "long") +1 else -1
      sel  <- if (sign == +1) ov_val > 0 else ov_val < 0
      secs <- names(ov_val)[sel]
      secs_with_repl <- secs[lengths(repls[secs]) > 0]
      supply <- abs(ov_val[secs])

      if (length(secs) == 0) {
        return(list(adj = numeric(0), leftover = numeric(0)))
      }

      # build edge‐list frames
      s2o <- purrr::map_dfr(secs, function(i) tibble::tibble(
        from = "SRC", to = paste0("o_", i), cap = supply[i]
      ))

      o2r <- purrr::map_dfr(secs_with_repl, function(i) tibble::tibble(
        from = paste0("o_", i),
        to   = paste0("r_", repls[[i]]),
        cap  = supply[i]
      ))

      rep_secs <- unique(unlist(repls[secs], FALSE))
      rep_secs <- rep_secs[!is.na(rep_secs)]
      capfun   <- function(r) {
        lim <- rules[[r]]
        x   <- if (sign == +1) lim$max - clamped[r] else clamped[r] - lim$min
        max(x * prices[r], 0)
      }
      r2s <- purrr::map_dfr(rep_secs, function(r) tibble::tibble(
        from = paste0("r_", r), to = "SNK", cap = capfun(r)
      ))

      edges <- dplyr::bind_rows(s2o, o2r, r2s)
      verts <- c("SRC", paste0("o_", secs), paste0("r_", rep_secs), "SNK")

      g <- igraph::graph_from_data_frame(edges, directed = TRUE, vertices = verts)
      igraph::E(g)$capacity <- edges$cap
      mf <- igraph::max_flow(g, "SRC", "SNK")

      edf   <- igraph::as_data_frame(g, what = "edges")
      flows <- mf$flow

      # parse out “adj” and “leftover”
      adj <- purrr::set_names(numeric(length(rep_secs)), rep_secs)
      purrr::walk2(edf$from, edf$to, ~{
        if (startsWith(.x, "o_") && startsWith(.y, "r_")) {
          sec <- sub("^r_", "", .y)
          adj[sec] <<- adj[sec] + flows[which(edf$from == .x & edf$to == .y)] / prices[sec]
        }
      })

      leftover <- purrr::set_names(numeric(length(secs)), secs)
      purrr::walk2(edf$from, edf$to, ~{
        if (.x == "SRC") {
          sec <- sub("^o_", "", .y)
          idx <- which(edf$from == .x & edf$to == .y)
          leftover[sec] <<- supply[sec] - flows[idx]
        }
      })

      list(
        adj = adj * sign,
        leftover = leftover
      )
    },

    .calc_flow_from_want = function(portfolio, want) {
      all_secs_ids <- names(want)
      all_secs <- setNames(lapply(all_secs_ids, .security), all_secs_ids)
      prices <- setNames(vapply(all_secs, function(x) x$get_price(), numeric(1)), all_secs_ids)
      rules <- self$get_security_position_limits(portfolio, all_secs_ids)
      replacements <- portfolio$get_replacement_security()
      clamp_to_rule <- function(s, want) {
        lim <- rules[[s]]
        pmin(pmax(want[[s]], lim$min), lim$max)
      }

      clamped  <- vapply(all_secs_ids, clamp_to_rule, numeric(1), want = want)
      derived <- clamped
      ov_val <- (want[all_secs_ids] - clamped[all_secs_ids]) * prices[all_secs_ids]

      long_adj <- private$.run_side_("long", ov_val, clamped, prices, rules, replacements)
      short_adj <- private$.run_side_("short", ov_val, clamped, prices, rules, replacements)
      
      derived[names(long_adj$adj)]  <- derived[names(long_adj$adj)]  + long_adj$adj
      derived[names(short_adj$adj)] <- derived[names(short_adj$adj)] + short_adj$adj

      existing_qty <- private$.extract_qty(portfolio$get_target_position())
      trade_qty <- derived - tidyr::replace_na(existing_qty[names(derived)], 0)
      trade_qty <- trade_qty[trade_qty != 0]
      unfilled  <- c(long_adj$leftover, short_adj$leftover) %>% purrr:::keep(~ .x > 0)
      unfilled_qty <- unfilled / prices[names(unfilled)]

      swap_flag <- unlist(self$get_swap_flag_position_rules(portfolio, names(trade_qty)))

      list(
        position_qty = derived,
        trade_qty = trade_qty,
        swap_flag = swap_flag,
        unfilled_qty  = unfilled_qty
      )
    }
  )
)






# SMAConstrucor
#' @import dplyr
#' @import purrr
#' @import tibble
#' @importFrom igraph graph_from_data_frame E max_flow as_data_frame
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
    get_scale_qty = function(base_portfolio, sma_portfolio, base_security_id) {
      base_pos_qty <- tryCatch(
        {base_portfolio$get_target_position(base_security_id)$get_qty()}
        , error = function(e) 0
      )
      nav_ratio <- sma_portfolio$get_nav() / base_portfolio$get_nav()
      base_pos_qty * nav_ratio
    },

    get_scale_ratio = function(base_portfolio, sma_portfolio) {
      base_nav <- base_portfolio$get_nav()
      sma_nav  <- sma_portfolio$get_nav()
      if (base_nav == 0) return(0)
      sma_nav / base_nav
    },

    calc_rebalance_qty = function(sma, target_sec_id = NULL, base_trade_qty = 0) {
      base <- sma$get_base_portfolio()
      base_qty <- private$.extract_qty(base$get_target_position())
      if (!is.null(target_sec_id) && length(target_sec_id) == 1 && length(target_sec_id) == 1) {
        base_qty[target_sec_id] <- sum(base_qty[target_sec_id], base_trade_qty, na.rm = TRUE)
      }

      sma_qty <- private$.extract_qty(sma$get_target_position())
      if(is.null(target_sec_id)) target_sec_id <- names(base_qty)
      
      replacements <- sma$get_replacement_security()
      all_secs_id <- unique(c(names(base_qty), names(sma_qty), unlist(replacements, use.names = FALSE)))
      
      want <- setNames(
        vapply(
          all_secs_id,
          function(s) {
            if (s %in% c(target_sec_id)) {
              scale_ratio <- self$get_scale_ratio(base, sma)
              base_pos_qty <- tryCatch(base_qty[[s]], error = function(e) 0)
              return(scale_ratio * base_pos_qty)
            } 
            if (s %in% names(sma_qty)) return(sma_qty[s])
            0
          },
          numeric(1)  
        ),
        all_secs_id
      )

      private$.calc_flow_from_want(portfolio, want)
    }
  )
)
