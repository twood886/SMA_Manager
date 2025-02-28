#' @title Get Security (R6 Object)
#' @description
#' Function to get R6 Security Object from envir
#' @param id Security Ticker
#' @include class-security.R
#' @export
get_security <- function(id = NULL) {
  if (is.null(id)) {
    stop("id must be supplied")
  }
  id <- tolower(id)
  # Check if the ticker already exists in the registry
  if (exists(id, envir = .security_registry, inherits = FALSE)) {
    get(id, envir = .security_registry, inherits = FALSE)
  }
}