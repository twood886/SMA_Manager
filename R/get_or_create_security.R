#' @title Get or Create Security
#' @description
#' Function to get or create R6 Security Object.
#' If security already exists, return object.
#' If securtiy does not already exist, create object
#' @include class-security.R
#' @include get_security.R
#' @include create_security.R
#' @return A \code{Security} object.
#' @export
get_or_create_security <- function(ticker = NULL) {
  if (is.null(ticker)) {
    stop("Ticker must be supplied")
  }
  if (exists(ticker, envir = .security_registry, inherits = FALSE)) {
    get(ticker, envir = .security_registry, inherits = FALSE)
  } else {
    sec <- Security$new(ticker = ticker)
    assign(ticker, sec, envir = .security_registry)
  }
}