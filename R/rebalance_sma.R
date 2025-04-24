#' @title Rebalance SMA
#' @description
#' Function to rebalance an SMA portfolio to match the target portfolio
#' @param sma SMA Portfolio object
#' @return Null
#' @export
rebalance_sma <- function(sma = NULL) {
  # Check that SMA is supplied
  if (is.null(sma)) stop("SMA Id must be supplied")
  if (!inherits(sma, "SMA")) stop("SMA must be of class SMA")

  tgt_portfolio <- sma$get_target_portfolio()
  tgt_position_id <- sapply(
    tgt_portfolio$get_position(),
    function(x) x$get_id()
  )

  lapply(tgt_position_id, rebalance_sma_position, sma = sma)
  invisible(NULL)
}

#' @title Rebalance SMA Position
#' @description
#' Function to rebalance a position in an SMA portfolio to match
#'  the target portfolio
#' @param sma SMA Portfolio object
#' @param position_id Position ID to rebalance
#' @return Null
rebalance_sma_position <- function(sma, tgt_position_id) {
  tgt_portfolio <- sma$get_target_portfolio()

  # Get the SMA & Target Portfolio NAVs to Calc Scaling Factor
  sma_nav <- sma$get_nav()
  tgt_nav <- tgt_portfolio$get_nav()
  sma_scale_factor <- sma_nav / tgt_nav

  # Check SMA replacements
  sma_replacement_security <- sma$get_replacement_security(tgt_position_id)
  if (!is.null(sma_replacement_security)) {
    sma_position_id <- c(sma_position_id, sma_replacement_security)
    tgt_position_id <- sma$get_replaced_security(sma_position_id)
  } else {
    sma_position_id <- tgt_position_id
  }

  # Get current positions
  sma_position <- tryCatch(
    sma$get_position(sma_position_id),
    error = function(e) {
      pos <- create_position(sma$get_short_name(), sma_position_id, 0)
      sma$add_position(pos)
      pos
    }
  )

  tgt_position <- lapply(tgt_position_id, tgt_portfolio$get_position)

  # Calculate the rebalance quantity
  if (length(tgt_position) > 1) {
    tgt_val <- sum(sapply(tgt_position, function(x) x$get_mkt_val()))
    pos_price <- sma_position$get_security()$get_price()
    rebal_qty <- (tgt_val * sma_scale_factor) / pos_price
  } else if (sma_position$get_id() == tgt_position[[1]]$get_id()) {
    rebal_qty <- tgt_position[[1]]$get_qty() * sma_scale_factor
  } else {
    tgt_val <- tgt_position[[1]]$get_mkt_val()
    pos_price <- sma_position$get_security()$get_price()
    rebal_qty <- (tgt_val * sma_scale_factor) / pos_price
  }

  # Create new target position for SMA
  new_position <- create_position(
    portfolio_short_name = sma$get_short_name(),
    id = sma_position_id,
    qty = rebal_qty
  )
  # Remove existing postion and add new target position
  sma$remove_target_position(sma_position_id)
  sma$add_target_position(new_position)
  invisible(NULL)
}
