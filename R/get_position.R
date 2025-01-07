setGeneric("get_position",
  function(object, ticker, ...) standardGeneric("get_position")
)

#' @include portfolio.R
#' @include get_tickers.R
#' @include create_position.R
setMethod("get_position",
  signature(object = "portfolio"),
  function(object, ticker = NULL, ...) {
    if (is.null(ticker)) {
      return(object@positions)
    }
    tickers <- get_tickers(object)
    if (ticker %in% tickers) {
      return(object@positions[[which(tickers == ticker)]])
    } else {
      return(create_position(ticker, NA_character_, 0, 0, 0, 0, 0, 0, 0))
    }
  }
)
