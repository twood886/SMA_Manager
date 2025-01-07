#' @title Create Fake Portfolio
#' @description A function to create a fake portfolio based on a specified
#'  NAV and weights in an existing portfolio
#' @param base_portfolio Real portfolio to base fake one on.
#' @param long_name Fake portfolio logn name
#' @param short_name Fake portfolio short name
#' @param nav Target NAV of fake portfolio
#' @include get_nav.R
#' @include sma.R
#' @include scale_position.R
create_fake_portfolio <- function(
  base_portfolio, long_name, short_name, nav
) {
  nav_base <- get_nav(base_portfolio)
  scale_factor <- nav / nav_base
  new("portfolio",
    long_name = long_name,
    short_name = short_name,
    nav = nav,
    positions = lapply(
      base_portfolio@positions,
      scale_position,
      scale_factor = scale_factor
    )
  )
}