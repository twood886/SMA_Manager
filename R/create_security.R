#' @title Create Security
#' @description
#' Function to create an R6 Security Object
#' @param id Security Ticker
#' @param description Security Description
#' @param instrument_type security instrument type
#' @param occ_code security occ_code
#' @param ... Additional data
#' @include class-security.R
#' @return A \code{Security} object.
#' @export
create_security <- function(
  id = NULL,
  description = NULL,
  instrument_type = NULL,
  occ_code = NULL,
  ...
) {
  if (is.null(id)) {
    stop("id must be supplied")
  }
  id <- tolower(id)
  # Check if the ticker already exists in the registry
  if (exists(id, envir = .security_registry, inherits = FALSE)) {
    stop("Security already exists")
  }
  id <- tolower(id)
  # If Bond use description as id
  if (instrument_type == "Bond") {
    id <- tolower(description)
    ts_data <- list(NULL)
  } else {
    if (instrument_type == "Listed Option") {
      id <- tolower(occ_code)
    }
    # Download time series data from yahoo finance
    #ts_data <- get_yahoo_history(id)
    ts_data <- NULL
  }

  # Create empty data list
  data <- list(
    description = description,
    instrument_type = instrument_type
  )

  args_list <- list(...)
  data <- c(data, args_list)

  # Create new security
  sec <- Security$new(
    id = id,
    ts_data = ts_data,
    data = data
  )
  assign(id, sec, envir = .security_registry)
}