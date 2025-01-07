#' @title Create Portfolio
#' @param id Id
#' @param long_name Long Name
#' @param short_name Short Name
#' @param NAV NAV
#' @param enfusion_url enfusion url
#' @include portfolio.R
create_portfio <- function(
  id, long_name, short_name, nav, enfusion_url
) {
  new("portfolio",
    id = id,
    long_name = long_name,
    short_name = short_name,
    nav = nav,
    enfusion_url = enfusion_url
  )
}