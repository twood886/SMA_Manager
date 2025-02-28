#' @title Get Portfolio
#' @description
#' Function to get R6 Portfolio from environment
#' @param short_name Portfolio Short Name
#' @include class-portfolio.R
#' @export
get_portfolio <- function(short_name = NULL) {
  if (is.null(short_name)) {
    stop("Portfolio short name must be supplied")
  }
  # Check if the portfolio already exists in the registry
  if (!exists(short_name, envir = .portfolio_registry, inherits = FALSE)) {
    stop("Specified portfolio has not been created")
  }
  get(short_name, envir = .portfolio_registry, inherits = FALSE)
}