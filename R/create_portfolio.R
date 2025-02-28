#' @title Create Portfolio
#' @description
#' Create an R6 Portfolio object
#' @param long_name portofolio long name
#' @param short_name portfolio short name
#' @param nav Portfolio NAV
#' @param positions List of positions
create_portfolio <- function(
  long_name, short_name = NULL, nav = NULL, positions = NULL
) {
  if (is.null(short_name)) {
    stop("Portfolio Short Name must be supplied")
  }
  # Check if the portfolio already exists in the registry
  if (exists(short_name, envir = .portfolio_registry, inherits = FALSE)) {
    stop("Portfolio already exists")
  }

  portfolio <- Portfolio$new(
    long_name = long_name,
    short_name = short_name,
    nav = nav,
    positions = positions
  )
  # Assign the portfolio to the portfolio registry
  assign(short_name, portfolio, envir = .portfolio_registry)
  # Return the portfolio R6 object
  return(portfolio)
}


#' @title Create Portfolio from Enfusion
#' @description
#' Function to create an R6 Portfolio Object
#' @param long_name Portfolio Long Name
#' @param short_name Portfolio Short Name
#' @param enfusion_url Enfusion Web URL to
#' @include class-portfolio.R
#' @return A \code{Portfolio} object.
#' @export
create_portfolio_from_enfusion <- function(long_name, short_name = NULL, enfusion_url) {

  # Download the enfusion report
  enfusion_rep <- dplyr::filter(
    enfusion::get_enfusion_report(enfusion_url),
    !is.na(`Description`)
  )
  # get the portfolio NAV from the enfusion report
  nav <- as.numeric(enfusion_rep$`$ GL NAV`[[1]])
  # Create a postion for each row in the enfusion file
  positions <- apply(
    enfusion_rep,
    1,
    create_position_from_enfusion,
    portfolio_short_name = short_name,
    simplify = FALSE
  )
  create_portfolio(long_name, short_name, nav, positions)
}