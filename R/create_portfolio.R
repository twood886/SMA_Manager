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

#' @title Create Portfolio from Consoldated Position Report
#' @description Function to create a Portfolio S4 object from an enfusion
#'  Consolodated Positions Report.
#'  Specific to the Consolodated Positions Report.
#' @param long_name Portfolio Long Name
#' @param short_name Portfolio Short Name
#' @param enfusion_url Enfusion WebServiceURL to Consolodated Positions Report
#' @returns portfolio S4 object
#' @import enfusion
#' @importFrom dplyr filter
#' @include create_position.R
#' @include portfolio.R
#' @export
create_portfolio_from_consolodated_position_report <- function( #nolint
  id, long_name, short_name, enfusion_url
) {
  enfusion_rep <- dplyr::filter(
    enfusion::get_enfusion_report(enfusion_url),
    !is.na(`Description`)
  )
  nav <- as.numeric(enfusion_rep$`$ GL NAV`[[1]])
  positions <- apply(
    enfusion_rep,
    1,
    \(x) {
      create_position(
        as.character(x["Ticker"]),
        as.character(x["Description"]),
        as.numeric(x["Stock Quantity"]),
        as.numeric(x["Delta Quantity"]),
        as.numeric(x["Total Quantity"]),
        as.numeric(x["Market value"]),
        as.numeric(x["Delta Value"]),
        as.numeric(x["Stock % NAV"]),
        as.numeric(x["Delta % NAV"])
      )
    }
  )
  names(positions) <- as.character(enfusion_rep$Ticker)
  new("portfolio",
    id = id,
    long_name = long_name,
    short_name = short_name,
    nav = nav,
    enfusion_url = enfusion_url,
    positions_current = positions,
    positions_target = positions
  )
}
