#' @title Portfolio (R6 Object)
#' @description
#' R6 Class representing a portfolio object.
#' @import R6
#' @import enfusion
#' @importFrom dplyr filter
#' @include create_position.R
Portfolio <- R6::R6Class( #nolint
  "Portfolio",
  public = list(
    #' @field id Portfolio Id
    id = NULL,
    #' @field long_name Portfolio Long Name
    long_name = NULL,
    #' @field short_name Portfolio Short Name
    short_name = NULL,
    #' @field nav Portfolio NAV
    nav = NULL,
    #' @field enfusion_url Enfusion Web URL to
    #'  Consolidated Position Listing Report
    enfusion_url = NULL,
    #' @field positions List of Positions
    positions = NULL,

    #' @description
    #' Create New PortfolioR6 object
    #' @param long_name Portfolio Long Name
    #' @param short_name Portfolio Short Name
    #' @param enfusion_url Enfusion URL
    initialize = function(
      long_name, short_name, enfusion_url
    ) {
      self$id <- length(ls(envir = .portfolio_registry)) + 1
      self$long_name <- long_name
      self$short_name <- short_name
      # Download Enfusion Report
      enfusion_rep <- dplyr::filter(
        enfusion::get_enfusion_report(enfusion_url),
        !is.na(`Description`)
      )
      self$nav <- as.numeric(enfusion_rep$`$ GL NAV`[[1]])
      # Create Positions
      positions <- apply(
        enfusion_rep,
        1,
        \(x) {
          create_position(
            ticker = as.character(x["Ticker"]),
            desc = as.character(x["Description"]),
            stock_qty = as.numeric(x["Stock Quantity"]),
            delta_qty = as.numeric(x["Delta Quantity"]),
            total_qty = as.numeric(x["Total Quantity"]),
            mkt_val = as.numeric(x["Market value"]),
            delta_val = as.numeric(x["Delta Value"]),
            stock_pct_nav = as.numeric(x["Stock % NAV"]),
            delta_pct_nav = as.numeric(x["Delta % NAV"])
          )
        }
      )
      self$positions <- positions
    }
  )
)