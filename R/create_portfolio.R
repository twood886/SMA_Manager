#' @title Create Portfolio
#' @description
#' Function to create an R6 Portfolio Object
#' @param long_name Portfolio Long Name
#' @param short_name Portfolio Short Name
#' @param enfusion_url Enfusion Web URL to
#'  Consolidated Position Listing Report
#' @include class-portfolio.R
#' @return A \code{Portfolio} object.
#' @export
create_portfolio <- function(long_name, short_name = NULL, enfusion_url) {
  if (is.null(short_name)) {
    stop("Portfolio Short Name must be supplied")
  }
  # Check if the portfolio already exists in the registry
  if (exists(short_name, envir = .portfolio_registry, inherits = FALSE)) {
    stop("Portfolio already exists")
  }
  # Create new portfolio
  port <- Portfolio$new(
    long_name = long_name,
    short_name = short_name,
    enfusion_url = enfusion_url
  )
  assign(short_name, port, envir = .portfolio_registry)
}