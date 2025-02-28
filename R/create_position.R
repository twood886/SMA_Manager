#' @title Create Position
#' @description
#' Create an R6 Position object
#' @param portfolio_short_name Portfolio Short Name the security belongs to
#' @param id Security Ticker
#' @param desc Security Description
#' @param qty Quantity
#' @param delta_qty Delta Quantity
#' @param mkt_val Market Value
#' @param delta_val Delta Value
#' @param stock_pct_nav Stock Percent of NAV
#' @param delta_pct_nav Delta Percent of NAV
#' @return A \code{Position} object.
#' @include class-position.R
#' @export
create_position <- function(
  portfolio_short_name, id,
  qty, delta_qty,
  mkt_val, delta_val,
  stock_pct_nav, delta_pct_nav
) {
  id <- tolower(id)

  Position$new(
    portfolio_short_name = portfolio_short_name,
    id = id,
    qty = qty,
    delta_qty = delta_qty,
    mkt_val = mkt_val,
    delta_val = delta_val,
    stock_pct_nav = stock_pct_nav,
    delta_pct_nav = delta_pct_nav,
    sec = get_security(id)
  )
}

#' @importFrom dplyr case_when
create_position_from_enfusion <- function(x, portfolio_short_name = NULL) {
  # Values to create security
  bb_pos <- x[["BB Yellow Key Position"]]
  bb_underlying <- x[["BB Yellow Key Underlying"]]

  ticker <- x[["Ticker"]]
  instrument_type <- x[["Instrument Type"]]
  description <- x[["Description"]]
  occ_code <- x[["OCC Code"]]
  exch_cntry <- x[["Exchange Country Code"]]
  ric <- x[["RIC"]]

  id <- dplyr::case_when(
    instrument_type == "Bond" ~ description,
    instrument_type == "Listed Option" ~ bb_pos,
    instrument_type == "Equity" ~ bb_pos,
    .default = description
  )
  id <- tolower(id)

  gics_sec <- x[["GIC Sector"]]
  gics_ig <- x[["GIC Industry Group"]]
  gics_ind <- x[["GIC Industry"]]
  gics_sub_ind <- x[["GIC Sub-Industry"]]
  mlp_flag <- x[["Is MLP"]]

  # Values to create position
  if (instrument_type == "Listed Option") {
    qty <- as.numeric(x[["Option Quantity"]])
  } else {
    qty <- as.numeric(x[["Stock Quantity"]])
  }
  delta_qty <- as.numeric(x[["Delta Quantity"]])
  mkt_val <- as.numeric(x[["Market value"]])
  delta_val <- as.numeric(x[["Delta Value"]])
  stock_pct_nav <- as.numeric(x[["Stock % NAV"]])
  delta_pct_nav <- as.numeric(x[["Delta % NAV"]])

  sec <- get_or_create_security(
    id,
    description = description,
    bb_underlying = bb_underlying,
    instrument_type = instrument_type,
    occ_code = occ_code,
    gics_sec = gics_sec,
    gics_ig = gics_ig,
    gics_ind = gics_ind,
    gics_sub_ind = gics_sub_ind,
    mlp_flag = as.logical(mlp_flag)
  )

  pos <- Position$new(
    portfolio_short_name = portfolio_short_name,
    id = id,
    qty = qty,
    delta_qty = delta_qty,
    mkt_val = mkt_val,
    delta_val = delta_val,
    stock_pct_nav = stock_pct_nav,
    delta_pct_nav = delta_pct_nav,
    sec = sec
  )
  return(pos)
}
