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
  # Create a new target position for each position in the target portfolio
  new_pos <- lapply(tgt_position_id, rebalance_sma_position, sma = sma)

  # Check new_positions against SMA rules
  errors <- lapply(sma$get_sma_rules(), function(rule) rule$check_rule_target())

  # Find positions that pass the rules
  pass_pos <- new_pos[which(!sapply(new_pos, \(pos) pos$get_id()) %in% unlist(errors, use.names = FALSE))]

  # Remove existing postion and add new target position
  lapply(pass_pos)
  sma$remove_target_position(sma_position_id)
  sma$add_target_position(new_position)
  # Check SMA target positions pass rules

  invisible(NULL)
}

#' @title Rebalance SMA Position
#' @description
#' Function to rebalance a position in an SMA portfolio to match
#'  the target portfolio
#' @param sma SMA Portfolio object
#' @param position_id Position ID to rebalance
#' @return Null
rebalance_sma_position <- function(sma, base_position_id) {
  base_portfolio <- sma$get_target_portfolio()

  # Get the SMA & Target Portfolio NAVs to Calc Scaling Factor
  sma_nav <- sma$get_nav()
  base_nav <- base_portfolio$get_nav()
  sma_scale_factor <- sma_nav / base_nav

  # START NEW ------------------------------------------------------------------

  base_position <- base_portfolio$get_position(base_position_id)

  sma_position_id <- base_position_id
  replacement_securities <- sma$get_replacement_security(base_position_id)
  if (!is.null(replacement_securities)) {
    sma_position_id <- replacement_securities
  }

  sma_tgt_position <- lapply(
    sma_position_id,
    function(sec, sma) {
      tryCatch(
        {sma$get_tatget_position(sec)},
        error = function(e) {
          pos <- create_position(sma$get_short_name(), sec, 0)
          sma$add_target_position(pos)
          pos
        }
      )
    },
    sma = sma
  )

  tgt_position <- create_position(
    sma$get_short_name(),
    base_position_id,
    base_position$get_qty() * sma_scale_factor
  )

  for (pos in sma_tgt_position) {
    max_shares <- floor(min(sapply(
      sma$get_rules(),
      \(sec, rule) rule$get_security_max_value(sec),
      sec = pos$get_id()
    )))
    min_shares <- ceiling(max(sapply(
      sma$get_rules(),
      \(sec, rule) rule$get_security_min_value(sec),
      sec = pos$get_id()
    )))


    if (pos$get_id() == tgt_position$get_id()) {
      max_shares <- 


      base_position[[1]]$get_qty() * sma_scale_factor
      tgt_shares_remaining <- tgt_shares_remaining - pos$get_qty()
    } else {
      tgt_shares_remaining <- tgt_shares_remaining + pos$get_qty()
    }
  }

  # END NEW --------------------------------------------------------------------

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
  create_position(
    portfolio_short_name = sma$get_short_name(),
    id = sma_position_id,
    qty = rebal_qty
  )
}
