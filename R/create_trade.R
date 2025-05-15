#' Create a Trade with Specified Quantity
#'
#' This function creates a trade for a given portfolio and security with a specified quantity. 
#' It also handles swap logic and updates derived portfolios if applicable.
#'
#' @param portfolio_id A string representing the ID of the portfolio. Default is `NULL`.
#' @param security_id A string representing the ID of the security. Default is `NULL`.
#' @param trade_qty A numeric value specifying the quantity of the trade. Default is `NULL`.
#' @param swap A boolean indicating whether the trade involves a swap. If not provided, it is inferred from the portfolio's position.
#' @param assign_to_registry A boolean indicating whether to assign the trade to the registry. Default is `TRUE`.
#'
#' @return An invisible trade object created with the specified parameters.
#' 
#' @details
#' The function validates the input parameters and retrieves the security and portfolio objects. 
#' If the `swap` parameter is not explicitly provided, it attempts to infer it from the portfolio's position. 
#' After creating the trade, the function checks for any derived portfolios and ensures they mimic the base portfolio's 
#' position for the given security.
#'
#' @examples
#' \dontrun{
#' create_trade_qty(
#'   portfolio_id = "portfolio123",
#'   security_id = "security456",
#'   trade_qty = 100,
#'   swap = TRUE
#' )
#' }
#'
#' @include utils.R
#' @include api-functions.R
#' @export
create_trade_qty <- function(portfolio_id = NULL, security_id = NULL, trade_qty = NULL, swap, assign_to_registry = TRUE) {
  assert_string(portfolio_id, "portfolio_id")
  assert_string(security_id, "security_id")
  security <- .security(security_id)
  security_id <- security$get_id()
  portfolio <- .portfolio(portfolio_id, create = FALSE)
  swap <- tryCatch(
    {portfolio$get_position(security_id)$get_swap()},
    error = function(e) swap
  )
  assert_bool(swap, "swap")

  existing_trade_ids <- ls(envir = registries$trades, all.names = TRUE)

  .trade(
    security_id = security_id,
    portfolio_id = portfolio_id,
    qty = trade_qty,
    create = TRUE,
    swap = swap,
    assign_to_registry = assign_to_registry
  )

  derived_portfolios <- tryCatch(
    {get_tracking_smas(portfolio)},
    error = function(e) {
      NULL
    }
  )
  if(is.null(derived_portfolios)) {
    return(invisible(trades))
  }
  if (length(derived_portfolios) > 0) {
    for (derived_portfolio in derived_portfolios) {
      derived_portfolio$mimic_base_portfolio(security_id = security_id) 
    }
  }
  new_trade_ids <- setdiff(ls(envir = registries$trades, all.names = TRUE), existing_trade_ids)
  trades <- mget(new_trade_ids, envir = registries$trades, inherits = TRUE)
  invisible(trades)
}


#' Create a Trade Based on Target Weight
#'
#' This function creates a trade for a given portfolio and security based on a target weight.
#'
#' @param portfolio_id [character] The ID of the portfolio. Must be a non-empty string.
#' @param security_id [character] The ID of the security. Must be a non-empty string.
#' @param tgt_weight [numeric] The target weight for the security in the portfolio. Must be a valid number.
#' @param swap [logical] Indicates whether the trade involves a swap. If not provided, it will be inferred from the portfolio's current position.
#' @param assign_to_registry [logical] Indicates whether to assign the trade to the registry. Default is `TRUE`.
#'
#' @details
#' The function calculates the target quantity of the security based on the portfolio's net asset value (NAV) and the security's price.
#' It then determines the difference between the target quantity and the current quantity to compute the trade quantity.
#' Finally, it creates a trade with the calculated trade quantity.
#'
#' @return Creates a trade and returns the result of the `create_trade_qty` function.
#'
#' @examples
#' \dontrun{
#' create_trade_tgt_weight(
#'   portfolio_id = "portfolio123",
#'   security_id = "security456",
#'   tgt_weight = 0.05,
#'   swap = TRUE
#' )
#' }
#'
#' @seealso \code{\link{create_trade_qty}}, \code{\link{.portfolio}}, \code{\link{.security}}
#'
#' @include utils.R
#' @include api-functions.R
#' @export
create_trade_tgt_weight <- function(portfolio_id = NULL, security_id = NULL, tgt_weight = NULL, swap, assign_to_registry = TRUE) {
  assert_string(portfolio_id, "portfolio_id")
  assert_string(security_id, "security_id")
  security <- .security(security_id)
  security_id <- security$get_id()
  assert_number(tgt_weight, "tgt_weight")

  portfolio <- .portfolio(portfolio_id, create = FALSE)
  swap <- tryCatch(
    {portfolio$get_position(security_id)$get_swap()},
    error = function(e) swap
  )
  assert_bool(swap, "swap")
  security <- .security(security_id)
  target_qty <- portfolio$get_nav() * tgt_weight / security$get_price()
  current_pos <- portfolio$get_target_position(security_id)
  current_qty <- current_pos$get_qty()
  trade_qty <- target_qty - current_qty
  
  trades <- create_trade_qty(
    portfolio_id = portfolio_id,
    security_id = security_id,
    trade_qty = trade_qty,
    swap = swap,
    assign_to_registry = assign_to_registry
  )
  return(invisible(trades))
}