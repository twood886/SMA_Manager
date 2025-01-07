source(file.path("R", "security.R"))
source(file.path("R", "position.R"))
source(file.path("R", "portfolio.R"))
source(file.path("R", "get_position.R"))

setGeneric("get_position_weight",
  function(object, ...) standardGeneric("get_position_weight")
)

#' @include position.R
setMethod("get_position_weight",
  signature(object = "position"),
  function(object, ...) object@delta_pct_nav
)

#' @include portfolio.R
#' @include get_position.R
#' @include get_position_weight.R
setMethod("get_position_weight",
  signature(object = "portfolio"),
  function(object, ticker, ...) {
    p <- get_position(object, ticker, ...)
    if (is.null(p)) {
      return(0)
    } else {
      return(get_position_weight(p, ...))
    }
  }
)
