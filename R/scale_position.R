setGeneric("scale_position",
  function(object, scale_factor, ...) standardGeneric("scale_position")
)

#' @include position.R
setMethod("scale_position",
  signature(
    object = "position",
    scale_factor = "numeric"
  ),
  function(object, scale_factor, ...) {
    new("position",
      ticker = object@ticker,
      desc = object@desc,
      stock_qty = round(object@stock_qty * scale_factor, 0),
      delta_qty = round(object@delta_qty * scale_factor, 0),
      total_qty = round(object@total_qty * scale_factor, 0),
      mkt_val = round(object@mkt_val * scale_factor, 2),
      delta_val = round(object@delta_val * scale_factor, 2),
      stock_pct_nav = object@stock_pct_nav,
      delta_pct_nav = object@delta_pct_nav
    )
  }
)