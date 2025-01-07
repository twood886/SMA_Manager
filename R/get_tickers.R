setGeneric("get_tickers",
  function(object, ...) standardGeneric("get_tickers")
)

#' @include security.R
setMethod("get_tickers",
  signature(object = "security"),
  function(object, ...) {
    return(object@ticker)
  }
)

#' @include position.R
setMethod("get_tickers",
  signature(object = "position"),
  function(object, ...) {
    return(object@ticker)
  }
)

#' @include portfolio.R
setMethod("get_tickers",
  signature(object = "portfolio"),
  function(object, ...) {
    return(as.character(sapply(object@positions, get_tickers)))
  }
)