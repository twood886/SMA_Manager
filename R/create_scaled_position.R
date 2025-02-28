#' @title Create a Scaled Position (R6 Object)
#' @param position position to be scaled
#' @param scaling_factor scaling_factor
create_scaled_position <- function(position, scaling_factor) {
  # Check position supplied is a "Position" object
  if (!any(class(position) == "Position")) {
    stop("position must be a Position object")
  }
  # Create scaled position
  create_position(
    id = position$id,
    qty = position$qty * scaling_factor,
    delta_qty = position$delta_qty * scaling_factor,
    mkt_val = position$mkt_val * scaling_factor,
    delta_val = position$delta_val * scaling_factor,
    stock_pct_nav = position$stock_pct_nav,
    delta_pct_nav = position$delta_pct_nav
  )
}
