setGeneric("get_position_shares",
  function(object, ...) standardGeneric("get_position_shares")
)

#' @include position.R
setMethod("get_position_shares",
  signature(object = "position"),
  function(object, ...) object@total_qty
)

#' @include portfolio.R
#' @include get_position.R
#' @include get_position_shares.R
setMethod("get_position_shares",
  signature(object = "portfolio"),
  function(object, ticker, ...) {
    p <- get_position(object, ticker, ...)
    if (is.null(p)) {
      return(0)
    } else {
      return(get_position_shares(p, ...))
    }
  }
)
