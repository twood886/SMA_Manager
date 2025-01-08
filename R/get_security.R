#' @title Get Security (R6 Object)
#' @description
#' Function to get R6 Security Object from envir
#' @param ticker Security Ticker
#' @include security.R
#' @export
get_security <- function(ticker = NULL) {
  if (is.null(ticker)) {
    stop("Ticker must be supplied")
  }
  # Check if the ticker already exists in the registry
  if (exists(ticker, envir = .security_registry, inherits = FALSE)) {
    get(ticker, envir = .security_registry, inherits = FALSE)
  }
}