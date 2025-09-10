#' Get Tracking SMAs
#'
#' This function retrieves all SMA (Separately Managed Account) objects that are 
#' tracking a given portfolio. The function checks if the input is a valid 
#' `Portfolio` object and filters SMAs based on their base portfolio's short name.
#'
#' @param portfolio An object of class `Portfolio`. The portfolio for which 
#'   tracking SMAs are to be retrieved.
#'
#' @return A list of SMA objects that are tracking the given portfolio. If no 
#'   tracking SMAs are found, the function returns `NULL`.
#'
#' @details The function first validates that the input is of class `Portfolio`. 
#'   It then retrieves all portfolios from the `registries$portfolios` environment 
#'   and filters those that are of class `SMA`. Finally, it checks which SMAs 
#'   have the given portfolio as their base portfolio and returns them.
#'
#' @include utils.R
#' @import checkmate
#' @export
get_tracking_smas <- function(portfolio) {
  checkmate::assert_r6(portfolio, "Portfolio")
  if (inherits(portfolio, "SMA")) {
    stop("The input must be an object of class 'Portfolio'.")
  }
  portfolios <- mget(
    ls(envir = registries$portfolios, all.names = TRUE),
    envir = registries$portfolios,
    inherits = TRUE
  )
  smas <- portfolios[sapply(portfolios, function(x) inherits(x, "SMA"))]
  sma_base <- sapply(smas, \(sma) sma$get_base_portfolio()$get_short_name())
  tracking_smas <- smas[sma_base == portfolio$get_short_name()]
  if (length(tracking_smas) == 0) {
    return(NULL)
  }
  tracking_smas
}
