#' @title Create Security
#' @description
#' Function to create an R6 Security Object
#' @param ticker Security Ticker
#' @include class-security.R
#' @return A \code{Security} object.
#' @export
create_security <- function(ticker = NULL) {
  if (is.null(ticker)) {
    stop("Ticker must be supplied")
  }
  # Check if the ticker already exists in the registry
  if (exists(ticker, envir = .security_registry, inherits = FALSE)) {
    stop("Security already exists")
  }
  # Create new security
  sec <- Security$new(ticker = ticker)
  assign(ticker, sec, envir = .security_registry)
}