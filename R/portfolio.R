# Object Definition ------------------------------------------------------------
#' @title Portfolio (S4 Object)
#' @description S4 Class representing a portfolio object
#' @slot id Account Id
#' @slot long_name Portfolio Long Name
#' @slot short_name Portfolio Short Name
#' @slot enfusion_url Enfusion Web URL to Consolidated Position Listing Report
#' @slot positions List of Positions
setClass(
  "portfolio",
  representation(
    id = "character",
    long_name = "character",
    short_name = "character",
    nav = "numeric",
    enfusion_url = "character",
    positions_current = "list",
    positions_target = "list"
  )
)

# Object Creator ---------------------------------------------------------------
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