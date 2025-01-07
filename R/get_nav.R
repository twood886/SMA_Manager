source(file.path("R", "portfolio.R"))

setGeneric("get_nav",
  function(portfolio, ...) standardGeneric("get_nav")
)

setMethod("get_nav",
  signature(portfolio = "portfolio"),
  function(portfolio, ...) {
    return(portfolio@nav)
  }
)