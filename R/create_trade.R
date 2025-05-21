#' Create Proposed Trade Quantity
#'
#' This function calculates the proposed trade quantities for a given portfolio and securities.
#' It also supports propagating the proposed trades to derived portfolios if specified.
#'
#' @param portfolio_id A string representing the ID of the portfolio. Default is `NULL`.
#' @param security_id A vector of strings representing the IDs of the securities. Default is `NULL`.
#' @param trade_qty A numeric vector representing the trade quantities for the securities. Default is `NULL`.
#' @param swap A named logical vector indicating whether to swap for each security. The names should match the `security_id`.
#' @param flow_to_derived A logical value indicating whether to propagate the proposed trades to derived portfolios. Default is `TRUE`.
#'
#' @return A data frame containing the proposed trade quantities with the following columns:
#' \describe{
#'   \item{portfolio_id}{The ID of the portfolio.}
#'   \item{security_id}{The ID of the security.}
#'   \item{trade_qty}{The proposed trade quantity for the security.}
#'   \item{swap}{A logical value indicating whether to swap for the security.}
#' }
#'
#' @details
#' The function validates the input parameters and retrieves the necessary portfolio and security information.
#' It calculates the proposed trades for the specified portfolio and optionally for its derived portfolios.
#' The results are returned as a consolidated data frame.
#'
#' @examples
#' \dontrun{
#' create_proposed_trade_qty(
#'   portfolio_id = "portfolio_123",
#'   security_id = c("sec_1", "sec_2"),
#'   trade_qty = c(100, 200),
#'   swap = c(sec_1 = TRUE, sec_2 = FALSE),
#'   flow_to_derived = TRUE
#' )
#' }
#'
#' @include utils.R
#' @include api-functions.R
#' @include class-portfolio.R
#' @include class-security.R
#' @export
create_proposed_trade_qty <- function(
  portfolio_id = NULL, 
  security_id = NULL, 
  trade_qty = NULL, 
  swap,
  flow_to_derived = TRUE
) {
  assert_string(portfolio_id, "portfolio_id")
  assert_string(security_id, "security_id")
  security <- lapply(security_id, \(sec) .security(sec))
  security_id <- vapply(security, \(x) x$get_id(), character(1))
  portfolio <- .portfolio(portfolio_id, create = FALSE)
  swap <- vapply(
    security_id,
    \(sec) tryCatch(portfolio$get_position(sec)$get_swap(), error = function(e) swap[[sec]]),
    logical(1)
  )
  assert_bool(swap, "swap")
  names(swap) <- security_id

  t <- list()
  t[[portfolio_id]] <- portfolio$calc_proposed_trade(security_id, trade_qty)
  
  if(flow_to_derived) {
    derived_portfolios <- tryCatch(get_tracking_smas(portfolio), error = function(e) NULL)
    if (length(derived_portfolios) > 0) {
      for (derived_portfolio in derived_portfolios) {
        t[[derived_portfolio$get_short_name()]] <- derived_portfolio$calc_proposed_rebalance_trade(security_id, trade_qty)
      }
    }
  }

  .get_info_from_t <- function(t_, name) {
    trades_qty <- t_$trade_qty
    swap <- t_$swap
    data.frame(
      "Portfolio" = rep(name, length(trades_qty)),
      "Security" = names(trades_qty),
      "Trade Quantity" = trades_qty,
      "Swap" = swap[names(trades_qty)],
      "Unfilled" = t_$unfilled_qty[names(trades_qty)],
      row.names = NULL,
      check.names = FALSE
    )
  }

  proposed_trade_df <- bind_rows(
    lapply(seq_along(t), function(x) .get_info_from_t(t[[x]], names(t)[x]))
  )
  return(invisible(proposed_trade_df))
}



#' Create Proposed Trade Target Weight
#'
#' This function calculates the proposed trade quantity based on the target weight 
#' for a given security in a portfolio and creates a proposed trade data frame.
#'
#' @param portfolio_id [character] The ID of the portfolio. Must be a non-empty string.
#' @param security_id [character] The ID of the security. Must be a non-empty string.
#' @param tgt_weight [numeric] The target weight for the security in the portfolio. Must be a valid number.
#' @param swap [logical] Indicates whether the trade involves a swap. 
#' @param flow_to_derived [logical] (Default: TRUE) Determines if the flow should propagate to derived positions.
#'
#' @return [invisible(data.frame)] A data frame containing the proposed trade details.
#'
#' @details
#' The function calculates the target quantity of the security based on the portfolio's 
#' net asset value (NAV) and the security's price. It then computes the trade quantity 
#' required to achieve the target weight by comparing it with the current quantity. 
#' Finally, it creates a proposed trade data frame using the calculated trade quantity.
#'
#' @examples
#' \dontrun{
#' create_proposed_trade_tgt_weight(
#'   portfolio_id = "portfolio123",
#'   security_id = "security456",
#'   tgt_weight = 0.05,
#'   swap = FALSE,
#'   flow_to_derived = TRUE
#' )
#' }
#'
#' @seealso \code{\link{create_proposed_trade_qty}}
#' @include utils.R
#' @include api-functions.R
#' @export
create_proposed_trade_tgt_weight <- function(
  portfolio_id = NULL, 
  security_id = NULL, 
  tgt_weight = NULL, 
  swap,
  flow_to_derived = TRUE
) {
  assert_string(portfolio_id, "portfolio_id")
  assert_string(security_id, "security_id")
  assert_numeric(tgt_weight, "tgt_weight")

  portfolio <- .portfolio(portfolio_id, create = FALSE)
  security <- .security(security_id)
  security_id <- security$get_id()
  target_qty <- portfolio$get_nav() * tgt_weight / security$get_price()
  
  current_qty <- tryCatch(portfolio$get_position(security_id)$get_qty(), error = function(e) 0)
  trade_qty <- round(target_qty - current_qty, 0)
  
  proposed_trade_df <- create_proposed_trade_qty(
    portfolio_id = portfolio_id,
    security_id = security_id,
    trade_qty = trade_qty,
    swap = swap,
    flow_to_derived = flow_to_derived
  )
  return(invisible(proposed_trade_df))
}


#' Execute Proposed Trades
#'
#' This function processes a data frame of proposed trades and executes them
#' by calling an internal `.trade` function for each row in the data frame.
#'
#' @param proposed_trade_df A data frame containing the proposed trades. 
#'   The data frame must have the following columns:
#'   \describe{
#'     \item{portfolio_id}{A string representing the portfolio ID.}
#'     \item{security_id}{A string representing the security ID.}
#'     \item{trade_qty}{A numeric value representing the trade quantity.}
#'     \item{swap}{A boolean indicating whether the trade is a swap.}
#'   }
#'
#' @return Returns `TRUE` invisibly after successfully processing all trades.
#'
#' @details
#' The function validates the input data frame to ensure it has the required
#' structure and types for each column. It then iterates over each row of the
#' data frame and executes the trade using the `.trade` function.
#'
#' @examples
#' \dontrun{
#' proposed_trades <- data.frame(
#'   portfolio_id = c("P1", "P2"),
#'   security_id = c("S1", "S2"),
#'   trade_qty = c(100, 200),
#'   swap = c(TRUE, FALSE)
#' )
#' proposed_to_trade(proposed_trades)
#' }
#' @include utils.R
#' @include api-functions.R
#' @export
proposed_to_trade <- function(proposed_trade_df) {
  assert_inherits(proposed_trade_df, "data.frame", "proposed_trade_df")
  assert_string(proposed_trade_df$'Portfolio', "portfolio_id")
  assert_string(proposed_trade_df$'Security', "security_id")
  assert_numeric(proposed_trade_df$'Trade Quantity', "trade_qty")
  assert_bool(proposed_trade_df$'Swap', "swap")

  for (i in seq_len(nrow(proposed_trade_df))) {
    .trade(
      security_id = proposed_trade_df$Security[i],
      portfolio_id = proposed_trade_df$Portfolio[i],
      qty = proposed_trade_df[i, "Trade Quantity"],
      swap = proposed_trade_df$Swap[i],
      create = TRUE
    )
  }
  return(invisible(TRUE))
}