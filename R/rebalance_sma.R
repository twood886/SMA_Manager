#' @title Rebalance SMA
#' @description
#' Function to rebalance an SMA portfolio to match the base portfolio
#' @param sma SMA Portfolio object
#' @return Null
#' @import parallel
#' @import Rblpapi
#' @include remove_all_portfolio_trades.R
#' @include create_trade.R
#' @export
rebalance_sma <- function(sma = NULL) {
  assert_inherits(sma, "SMA", "sma")

  sma$remove_target_position()
  remove_all_portfolio_trades(sma$get_short_name())

  base_ids <- vapply(
    sma$get_base_portfolio()$get_target_position(),
    function(x) x$get_id(),
    character(1)
  )
  
  replacements <- vapply(base_ids, sma$get_replacement_security, character(1))
  has_replacement <- replacements != base_ids

  safe_rebalance <- function(sec_id) {
    tryCatch({
      sma$rebalance_position(sec_id, assign_position = TRUE)
    }, error = function(e) {
      warning(paste0("Rebalance failed for ", sec_id, ": ", e$message))
      sec_id
    })
  }

  ncores <- max(1L, parallel::detectCores() - 1L)
  cl <- parallel::makeCluster(ncores - 1)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterExport(cl, c("sma", "registries"), envir = environment())
  parallel::clusterEvalQ(cl, {
    library(Rblpapi)
    library(SMAManager)
    blpConnect()
    assign(sma$get_short_name(), sma, envir = registries$portfolios)
    invisible(NULL)
  })

  results_noreplace <- parallel::parLapply(cl, base_ids[!has_replacement], safe_rebalance)

  noreplace_success <- vapply(unlist(results_noreplace), inherits, logical(1), what = "Position")
  lapply(unlist(results_noreplace[noreplace_success]), sma$add_target_position)

  results_replace <- lapply(base_ids[has_replacement], safe_rebalance)
  replace_success <- vapply(unlist(results_replace), inherits, logical(1), what = "Position")

  errors <- c(
    results_noreplace[!noreplace_success],
    results_replace[!replace_success]
  )

  tgt_trade <- sma$get_target_trade_qty(security_id = NULL)

  lapply(
    tgt_trade,
    function(t) {
      add_trade(
        security_id = t$security_id,
        portfolio_id = t$portfolio_id,
        qty = t$amt,
        swap = t$swap
      )
    }
  )


  if (length(errors) > 0) {
    warning(paste0("Rebalance failed for: ", paste(unlist(errors), collapse = ", ")))
  }
  invisible(c(results_replace[replace_success], results_noreplace[noreplace_success]))
}
