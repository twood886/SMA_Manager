setGeneric("add_security_data",
  function(object, security, ...) standardGeneric("add_security_data")
)

add_security_data <- function(object, security, ...) UseMethod("add_security_data")

#' @include security.R
#' @include position.R
setMethod("add_security_data",
  signature(
    object = "position",
    security = "security"
  ),
  function(object, security, ...) {
    if (object@ticker == security@ticker) {
      object@security <- security
      object@adv_days <- abs(object@total_qty) / security@adv
      return(object)
    } else {
      return(object)
    }
  }
)

#' @include security.R
#' @include position.R
#' @include portfolio.R
#' @include get_tickers.R
setMethod("add_security_data",
  signature(
    object = "portfolio",
    security = "list"
  ),
  function(object, security, ...) {
    tickers_ptfl <- get_tickers(object)
    tickers_sec <- sapply(security, get_tickers)
    sub_tickers_sec <- security[match(tickers_ptfl, tickers_sec)]

    positions <- mapply(
      add_security_data,
      object@positions,
      sub_tickers_sec
    )

    object@positions <- positions
    return(object)
  }
)