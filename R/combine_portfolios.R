combine_portfolios <- function() {
  funds <- mget(ls(.portfolio_registry), envir = .portfolio_registry)
  # get all the smas in the registry
  smas <- mget(ls(.sma_registry), envir = .sma_registry)

  # Calc NAVs
  nav <- sum(sapply(c(funds, smas), \(x) x$get_nav()))
  # get all positions in Funds
  all_positions <- unlist(
    lapply(
      c(funds, smas),
      \(x) x$get_position()
    ),
    recursive = FALSE
  )
  uniq_sec <- unique(lapply(all_positions, \(x) x$get_security()))

  positions <- lapply(
    uniq_sec,
    combine_positions,
    positions = all_positions
  )

  lapply(positions, \(x) x$calc_stock_pct_nav, nav = nav)
  lapply(positions, \(x) x$calc_delta_pct_nav, nav = nav)

  return(positions)
}

combine_positions <- function(security, positions) {
  p <- positions[
    which(sapply(positions, \(x) x$get_security()$id) == security$id)
  ]
  comb_qty <- sum(sapply(p, \(x) x$qty))
  comb_delta_qty <- sum(sapply(p, \(x) x$delta_qty))
  comb_mkt_val <- sum(sapply(p, \(x) x$mkt_val))
  comb_delta_val <- sum(sapply(p, \(x) x$delta_val))

  comb_sec <- create_position(
    id = security$id,
    desc = security$desc,
    qty = comb_qty,
    delta_qty = comb_delta_qty,
    mkt_val = comb_mkt_val,
    delta_val = comb_delta_val,
    stock_pct_nav = NULL,
    delta_pct_nav = NULL
  )
  return(comb_sec)
}
