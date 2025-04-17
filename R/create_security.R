#' @title Create Security
#' @description
#' Function to create an R6 Security Object from Bloomberg id
#' @param bbid Bloomberg Id
#' @import Rblpapi
#' @include class-security.R
#' @return A \code{Security} object.
#' @export
create_security <- function(bbid = NULL) {
  # Validators
  if (is.null(bbid)) {
    stop("Bloomberg Id must be supplied")
  }
  id <- Rblpapi::bdp(bbid, "DX194")
  if (id$DX194 == "") {
    stop("Security not found")
  }
  id <- tolower(bbid)
  # Check if the ticker already exists in the registry
  if (exists(id, envir = .security_registry, inherits = FALSE)) {
    stop("Security already exists")
  }
  # Create security object
  sec <- Security$new(id = id)
  # Assign security object to the registry
  assign(id, sec, envir = .security_registry)
}