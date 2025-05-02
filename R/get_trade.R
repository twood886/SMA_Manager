#' @title Get Trade
#' @description Get trade from trade environment
#' @param security_id Security ID
#' @param swap Is trade on swap? Default is FALSE
get_trade <- function(security_id = NULL, swap = FALSE) {
  if (is.null(security_id)) stop("security_id is required", call. = FALSE)
  if (!is.logical(swap)) stop("swap must be logical", call. = FALSE)
  all_trades <- mget(
    ls(envir = registries$trades, all.names = TRUE),
    envir = registries$trades, 
    inherits = TRUE
  )
  all_trades_ids <- vapply(all_trades, function(x) x$get_security_id(), character(1))
  sec_trades <- all_trades[all_trades_ids == security_id]
  if (length(sec_trades) == 0) {
    stop("No trades found for the specified security ID", call. = FALSE)
  }
  sec_trades_swap_flag <- vapply(sec_trades, function(x) x$get_swap_flag(), logical(1))
  trade <- sec_trades[sec_trades_swap_flag == swap]
  if (length(trade) == 0) {
    stop("No trades found for the specified security ID and swap flag", call. = FALSE)
  }
  trade[[1]]
}
