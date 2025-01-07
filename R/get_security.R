get_security <- function(ticker = NULL) {
  if (is.null(ticker)) {
    stop("Ticker must be supplied")
  }
  # Check if the ticker already exists in the registry
  if (exists(ticker, envir = .security_registry, inherits = FALSE)) {
    get(ticker, envir = .security_registry, inherits = FALSE)
  }
}