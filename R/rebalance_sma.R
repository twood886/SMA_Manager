rebalance <- function(sma) {
  # Check that SMA is supplied
  if (is.null(sma)) {
    stop("SMA must be supplied")
  }
  # Check that SMA is a valid object
  if (!inherits(sma, "SMA")) {
    stop("SMA must be a valid object")
  }
  # Get target portfolio
  tgt_ptfl <- sma$get_target_portfolio()
  # Get current positions
  positions <- sma$get_position()
  # Get target positions
  target_positions <- tgt_ptfl$get_position()
  # Compare current positions to target positions
  for (i in seq_along(positions)) {
    pos <- positions[[i]]
    tgt_pos <- target_positions[[i]]
    if (pos$get_weight() != tgt_pos$get_weight()) {
      pos$rebalance(tgt_pos)
    }
  }
}