#' @title Create a new object of class \code{Trade}
#' @description
#' Function to create a new object of class \code{Trade} from a list of arguments.
#' @param security_id The security ID of the trade.
#' @param portfolio_id The portfolio ID of the trade.
#' @include get_trade.R
#' @include class-trade.R
add_trade <- function(
  security_id, portfolio_id, qty, swap = FALSE,
  update_portfolio = FALSE
) {
  if (is.null(security_id)) stop("security_id is required")
  if (is.null(portfolio_id)) stop("portfolio_id is required")
  if (!is.logical(swap)) stop("swap must be logical")
  if (!exists(portfolio_id, envir = registries$portfolios, inherits = FALSE)) {
    stop("Specified portfolio has not been created")
  }

  trade <- tryCatch(
    {
      get_trade(security_id, swap)
    }, error = function(e) {
      t <- Trade$new(security_id, swap)
      assign(security_id, t, envir = registries$trades)
      t
    }
  )

  trade$add_trade_qty(portfolio_id, qty)

  if (update_portfolio) {
    portfolio <- .portfolio(portfolio_id, create = FALSE)
    tgt_pos <- portfolio$get_target_position(security_id)
    tgt_pos$set_qty(tgt_pos$get_qty() + qty)
    portfolio$add_target_position(tgt_pos, overwrite = TRUE)
  }

  invisible(trade)
}
