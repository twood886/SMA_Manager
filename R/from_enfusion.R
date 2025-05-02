#' Create a Position from Enfusion Data
#'
#' This function generates a position object based on input data from Enfusion.
#'
#' @param x A named list or data frame containing the input data. Expected fields include:
#'   - `"Instrument Type"`: The type of financial instrument (e.g., "Bond", "Listed Option", "Equity").
#'   - `"FIGI"`: The Financial Instrument Global Identifier (used for bonds).
#'   - `"BB Yellow Key Position"`: Bloomberg Yellow Key Position (used for listed options and equities).
#'   - `"Description"`: A fallback description for other instrument types.
#'   - `"Option Quantity"`: The quantity of options (used for listed options).
#'   - `"Stock Quantity"`: The quantity of stocks (used for equities and other instruments).
#'   - `"Is Financed"`: A logical value indicating whether the position is financed.
#' @param portfolio_short_name A string representing the short name of the portfolio.
#'
#' @return A position object created using the `position()` function.
#'
#' @details
#' The function determines the `id` of the security based on the instrument type:
#' - For bonds, it retrieves the Bloomberg ID using the `Rblpapi::bdp()` function.
#' - For listed options and equities, it uses the Bloomberg Yellow Key Position.
#' - For other instrument types, it defaults to the `Description` field.
#'
#' The quantity (`qty`) is determined based on the instrument type:
#' - For listed options, it uses the `"Option Quantity"` field.
#' - For other instruments, it uses the `"Stock Quantity"` field.
#'
#' The `swap` parameter is set based on the `"Is Financed"` field.
#'
#' @examples
#' # Example usage:
#' data <- list(
#'   "Instrument Type" = "Equity",
#'   "BB Yellow Key Position" = "AAPL US Equity",
#'   "Stock Quantity" = "100",
#'   "Is Financed" = FALSE
#' )
#' create_position_from_enfusion(data, "Portfolio1", 1000000)
#' @importFrom dplyr case_when
#' @include api-functions.R
#' @export
create_position_from_enfusion <- function(x, portfolio_short_name) {
  # Values to create security
  instrument_type <- x[["Instrument Type"]]
  id <- switch(
    instrument_type,
    "Bond" = Rblpapi::bdp(x[["FIGI"]], "DX194")$DX194,
    "Listed Option" = x[["BB Yellow Key Position"]],
    "Equity" = x[["BB Yellow Key Position"]],
    x[["Description"]]
  )
  id <- tolower(id)
  # Values to create position
  if (instrument_type == "Listed Option") {
    qty <- as.numeric(x[["Option Quantity"]])
  } else {
    qty <- as.numeric(x[["Stock Quantity"]])
  }
  swap <- as.logical(x[["Is Financed"]])
  .position(portfolio_short_name, id, qty, swap = swap)
}


#' @title Create Portfolio from Enfusion
#' @description
#' Function to create an R6 Portfolio Object
#' @param long_name Portfolio Long Name
#' @param short_name Portfolio Short Name
#' @param enfusion_url Enfusion Web URL to
#' @include class-portfolio.R
#' @import parallel
#' @return A \code{Portfolio} object.
#' @export
create_portfolio_from_enfusion <- function(long_name, short_name = NULL, enfusion_url) {

  enfusion_rep <- dplyr::filter(
    enfusion::get_enfusion_report(enfusion_url),
    !is.na(`Description`)
  )
  nav <- as.numeric(enfusion_rep$`$ GL NAV`[[1]])

  # parallelize the positions
  nCores <- parallel::detectCores(logical = FALSE)
  cl <- parallel::makeCluster(nCores - 1)
  parallel::clusterEvalQ(cl,{
    library(Rblpapi)
    #library(SMAManager)
    source("R/zzz.R")
    source("R/utils.R")
    source("R/class-security.R")
    source("R/class-position.R")
    source("R/class-portfolio.R")
    source("R/api-functions.R")
    source("R/from_enfusion.R")
    blpConnect()
  })
  short_name <- short_name

  parallel::clusterExport(
    cl,
    varlist = "create_position_from_enfusion",
    envir   = .GlobalEnv
  )

  parallel::clusterExport(
    cl,
    varlist = c("enfusion_rep", "short_name", "registries"),
    envir   = environment()
  )
  # Create a postion for each row in the enfusion file
  positions <- parallel::parLapply(
    cl,
    X = seq_len(nrow(enfusion_rep)),
    fun = function(i) {
      create_position_from_enfusion(
        x = enfusion_rep[i, , drop = FALSE],
        portfolio_short_name = short_name
      )
    }
  )
  parallel::stopCluster(cl)

  # Add securities to registry
  securities <- lapply(
    positions,
    \(pos) pos$get_security()
  )

  for (sec in securities) {
    if (!exists(sec$get_id(), envir = registries$securities)) {
      assign(sec$get_id(), sec, envir = registries$securities)
    }
  }
  .portfolio(short_name, long_name, nav, positions, create = TRUE)
}


#' @title Create Portfolio from Enfusion
#' @description
#' Function to create an R6 Portfolio Object
#' @param long_name Portfolio Long Name
#' @param short_name Portfolio Short Name
#' @param enfusion_url Enfusion Web URL to
#' @include class-portfolio.R
#' @import parallel
#' @return A \code{Portfolio} object.
#' @export
create_sma_from_enfusion <- function(long_name, short_name = NULL, base_portfolio, enfusion_url) {

  enfusion_rep <- dplyr::filter(
    enfusion::get_enfusion_report(enfusion_url),
    !is.na(`Description`)
  )
  nav <- as.numeric(enfusion_rep$`$ GL NAV`[[1]])

  # parallelize the positions
  nCores <- parallel::detectCores(logical = FALSE)
  cl <- parallel::makeCluster(nCores - 1)
  parallel::clusterEvalQ(cl,{
    library(Rblpapi)
    library(SMAManager)
    blpConnect()
  })
  short_name <- short_name
  base_portfolio <- base_portfolio

  parallel::clusterExport(
    cl,
    varlist = "create_position_from_enfusion",
    envir   = .GlobalEnv
  )

  parallel::clusterExport(
    cl,
    varlist = c("enfusion_rep", "short_name", "registries", "base_portfolio"),
    envir   = environment()
  )
  # Create a postion for each row in the enfusion file
  positions <- parallel::parLapply(
    cl,
    X = seq_len(nrow(enfusion_rep)),
    fun = function(i) {
      create_position_from_enfusion(
        x = enfusion_rep[i, , drop = FALSE],
        portfolio_short_name = short_name
      )
    }
  )
  parallel::stopCluster(cl)

  # Add securities to registry
  securities <- lapply(
    positions,
    \(pos) pos$get_security()
  )

  for (sec in securities) {
    if (!exists(sec$get_id(), envir = registries$securities)) {
      assign(sec$get_id(), sec, envir = registries$securities)
    }
  }
  .sma(short_name, long_name, nav, positions, base_portfolio ,create = TRUE)
}
