#' @title Get Tracking SMAs for a Portfolio
#' @description Get tracking SMAs for a given portfolio.
#' @param portfolio Portfolio object. The portfolio for which to get the tracking SMAs.
#' @return A list of tracking SMAs for the given portfolio. If no tracking SMAs are found, NULL is returned.
get_tracking_smas <- function(portfolio = NULL) {
  if (!inherits(portfolio, "Portfolio") || inherits(portfolio, "SMA")) {
    stop("The input must be an object of class 'Portfolio'.")
  }
  portfolios <- mget(
    ls(envir = .portfolio_registry, all.names = TRUE),
    envir = .portfolio_registry,
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