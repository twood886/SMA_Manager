#' @title Get or Create Security (R6 Object)
#' @description
#' Function to get or create R6 Security Object.
#' If security already exists, return object.
#' If securtiy does not already exist, create object
#' @include security.R
#' @include get_security.R
#' @include create_security.R
#' @export
get_or_create_security <- function(ticker = NULL) {
  if (is.null(ticker)) {
    stop("Ticker must be supplied")
  }
  if (exists(ticker, envir = .security_registry, inherits = FALSE)) {
    get(ticker, envir = .security_registry, inherits = FALSE)
  } else {
    sec <- SecurityR6$new(ticker = ticker)
    assign(ticker, sec, envir = .security_registry)
  }
}