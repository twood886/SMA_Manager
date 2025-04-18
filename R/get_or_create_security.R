#' @title get or Create Security
#' @description
#' Function to get or create R6 Security Object.
#' If security already exists, return object.
#' If securtiy does not already exist, create object
#' @include class-security.R
#' @include get_security.R
#' @include create_security.R
#' @return A \code{Security} object.
#' @export
get_or_create_security <- function(id = NULL) {
  if (is.null(id)) {
    stop("id must be supplied")
  }
  id <- tolower(id)
  if (!exists(id, envir = .security_registry, inherits = FALSE)) {
    sec <- create_security(id)
    assign(id, sec, envir = .security_registry)
  }
  get_security(id)
}