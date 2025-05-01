#' @title Create Position
#' @description
#' Create an R6 Position object
#' @param portfolio_short_name Portfolio Short Name the security belongs to
#' @param id Security Ticker
#' @param qty Quantity
#' @param sec_id Security ID (optional). If not provided, \code{id} is used.
#' @return A \code{Position} object.
#' @include class-position.R
#' @include get_or_create_security.R
#' @export
create_position <- function(portfolio_short_name, id, qty, sec_id = NULL, swap = FALSE) {
  id <- tolower(id)
  if (is.null(sec_id)) {
    sec_id <- id
  } else {
    sec_id <- tolower(sec_id)
  }
  pos <- Position$new(
    portfolio_short_name = portfolio_short_name,
    id = id,
    qty = qty,
    sec = get_or_create_security(sec_id),
    swap = swap
  )
  pos$calc_delta_qty()
  pos$calc_mkt_val()
  pos$calc_delta_val()
  pos$calc_stock_pct_nav()
  pos$calc_delta_pct_nav()
  return(pos)
}

#' @importFrom dplyr case_when
#' @export
create_position_from_enfusion <- function(x, portfolio_short_name, nav) {
  # Values to create security
  instrument_type <- x[["Instrument Type"]]
  id <- dplyr::case_when(
    instrument_type == "Bond" ~ Rblpapi::bdp(x[["FIGI"]], "DX194")$DX194,
    instrument_type == "Listed Option" ~ x[["BB Yellow Key Position"]],
    instrument_type == "Equity" ~ x[["BB Yellow Key Position"]],
    .default = x[["Description"]]
  )
  id <- tolower(id)
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
  swap <- as.logical(x[["Is Financed"]])

  sec <- get_or_create_security(id = id)

  pos <- Position$new(
    portfolio_short_name = portfolio_short_name,
    id = id,
    qty = qty,
    sec = sec,
    swap = swap
  )
  pos$calc_delta_qty()
  pos$calc_mkt_val()
  pos$calc_delta_val()
  pos$calc_stock_pct_nav(nav)
  pos$calc_delta_pct_nav(nav)
  return(pos)
}
