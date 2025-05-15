#' Internal helper: create many positions from Enfusion report
#' @param enfusion_report Data frame of Enfusion report rows
#' @param short_name Character portfolio short name
#' @include utils.R
#' @include api-functions.R
#' @import Rblpapi
.bulk_security_positions <- function(enfusion_report, portfolio_short_name) {
  con <- tryCatch(Rblpapi:::defaultConnection(), error = function(e) NULL)
  if (is.null(con)) con <- Rblpapi::blpConnect()
  
  ids <- vapply(
    split(enfusion_report, seq_len(nrow(enfusion_report))),
    function(x) {
      switch(
        x[["Instrument Type"]],
        "Bond" = Rblpapi::bdp(x[["FIGI"]], "DX194")$DX194,
        "Listed Option" = x[["BB Yellow Key Position"]],
        "Equity" = x[["BB Yellow Key Position"]],
        x[["Description"]]
      )
    },
    character(1),
    USE.NAMES = FALSE
  )

  descriptions <- Rblpapi::bdp(ids, "DX615")
  inst_types <- Rblpapi::bdp(ids, "EX028")
  prices <- Rblpapi::bdp(ids, "PX_LAST")

  env <- registries$securities
  
  for (id in ids) {
    bbid <- tolower(id)
    if (exists(bbid, envir = env, inherits = FALSE)) next

    security <- Security$new(
      bbid = bbid,
      description = descriptions[id, "DX615"],
      instrument_type = inst_types[id, "EX028"],
      price = ifelse(inst_types[id, "EX028"] == "FixedIncome", 1, prices[id, "PX_LAST"])
    )
    assign(bbid, security, envir = env)
  }

  positions <- lapply(
    split(enfusion_report, seq_len(nrow(enfusion_report))),
    function(x) {
      instrument_type <- x[["Instrument Type"]]
      id <- switch(
        instrument_type,
        "Bond" = Rblpapi::bdp(x[["FIGI"]], "DX194")$DX194,
        "Listed Option" = x[["BB Yellow Key Position"]],
        "Equity" = x[["BB Yellow Key Position"]],
        x[["Description"]]
      )
      id <- tolower(id)
      if (instrument_type == "Listed Option") {
        qty <- as.numeric(x[["Option Quantity"]])
      } else {
        qty <- as.numeric(x[["Stock Quantity"]])
      }
      swap <- as.logical(x[["Is Financed"]]) %||% FALSE
      pos <- .position(portfolio_short_name, id, qty, swap = swap)
      pos
    }
  )
  return(positions)
}


#' Internal helper: create positions in parallel
#'
#' @param enfusion_report Data frame of Enfusion report rows
#' @param short_name Character portfolio short name
#' @param position_fn Function(x, portfolio_short_name) that returns a Position
#' @import furrr
#' @import parallel
#' @include utils.R
#'
#' @return List of Position objects
.make_positions <- function(enfusion_report, short_name, position_fn) {
  ncores <- parallel::detectCores(logical = FALSE)
  cl <- parallel::makeCluster(
    ncores - 1,
    type = "PSOCK",
    rscriptArgs = c("--vanilla")
  )
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterEvalQ(cl, options(renv.verbose = FALSE))
  parallel::clusterEvalQ(cl, {
    suppressPackageStartupMessages(library(SMAManager))
    suppressPackageStartupMessages(library(Rblpapi))
    blpConnect()
  })
  parallel::clusterExport(
    cl,
    varlist = c("enfusion_report", "short_name", "registries", "position_fn"),
    envir   = environment()
  )
  positions <- parallel::parLapply(
    cl,
    X = seq_len(nrow(enfusion_report)),
    fun = function(i) {
      position_fn(
        x = enfusion_report[i, , drop = FALSE],
        portfolio_short_name = short_name
      )
    }
  )
  positions
}

#' Create a Position from Enfusion Data
#'
#' @param x A list or single-row data.frame of enfusion data
#' @param portfolio_short_name Character short name of the portfolio
#' @return Position R6 object
#' @import Rblpapi
#' @include api-functions.R
#' @export
create_position_from_enfusion <- function(x, portfolio_short_name) {

  if (is.null(tryCatch(Rblpapi:::defaultConnection(), error = function(e) NULL))) #nolint
    Rblpapi::blpConnect()

  if (!is.list(x) && !is.data.frame(x)) {
    stop("`x` must be a list or data.frame row", call. = FALSE)
  }
  if (length(portfolio_short_name) != 1 || !nzchar(portfolio_short_name)) {
    stop("`portfolio_short_name` must be a non-empty string", call. = FALSE)
  }

  instrument_type <- x[["Instrument Type"]]
  id <- switch(
    instrument_type,
    "Bond" = Rblpapi::bdp(x[["FIGI"]], "DX194")$DX194,
    "Listed Option" = x[["BB Yellow Key Position"]],
    "Equity" = x[["BB Yellow Key Position"]],
    x[["Description"]]
  )

  id <- tolower(id)

  # determine quantity
  qty <- if (instrument_type == "Listed Option") {
    as.numeric(x[["Option Quantity"]])
  } else {
    as.numeric(x[["Stock Quantity"]])
  }
  if (is.na(qty)) {
    stop("Quantity is not numeric for row", call. = FALSE)
  }

  swap <- as.logical(x[["Is Financed"]]) %||% FALSE
  .position(portfolio_short_name, id, qty, swap = swap)
}

#' Create Portfolio from Enfusion
#'
#' @param long_name Character long name
#' @param short_name Character short name
#' @param enfusion_url URL to fetch Enfusion report
#' @return Portfolio R6 object
#' @export
create_portfolio_from_enfusion <- function(
  long_name, short_name, enfusion_url
) {
  enfusion_report <- dplyr::filter(
    enfusion::get_enfusion_report(enfusion_url),
    !is.na(.data$Description) #nolint
  )
  nav <- as.numeric(enfusion_report[["$ GL NAV"]][1])
  positions <- .bulk_security_positions(
    enfusion_report = enfusion_report,
    portfolio_short_name = short_name
  )
  .portfolio(short_name, long_name, nav, positions, create = TRUE)
}

#' Create SMA from Enfusion
#'
#' @param long_name Character long name
#' @param short_name Character short name
#' @param base_portfolio Portfolio object
#' @param enfusion_url URL to fetch Enfusion report
#' @return SMA R6 object
#' @export
create_sma_from_enfusion <- function(
  long_name, short_name, base_portfolio, enfusion_url
) {
  enfusion_report <- dplyr::filter(
    enfusion::get_enfusion_report(enfusion_url),
    !is.na(.data$Description) #nolint
  )
  nav <- as.numeric(enfusion_report[["$ GL NAV"]][1])

  positions <- .bulk_security_positions(
    enfusion_report = enfusion_report,
    portfolio_short_name = short_name
  )
  .sma(short_name, long_name, nav, positions, base_portfolio, create = TRUE)
}


#' Register Securities in the Securities Registry
#'
#' This function iterates over a list of `Position` objects, validates their
#'  type and registers their associated `Security` objects in the securities
#'  registry if they are not already present.
#'
#' @param positions A list of `Position` objects. Each `Position` object must
#'  have a method `get_security()` that returns a `Security` object, and each
#' `Security` object must have a method `get_id()` that returns a unique
#'  identifier.
#'
#' @return Returns `NULL` invisibly.
#'
#' @details The function ensures that each `Position` object in the input list
#'  is of the correct class by calling `SMAManager:::assert_inherits`. If the
#'  associated `Security` object is not already registered in the
#'  `registries$securities` environment, it is added using its unique
#'  identifier as the key.
#'
#' @note This function relies on the `registries$securities` environment being
#'  pre-defined and accessible. It also assumes that the `SMAManager` package
#'  provides the `assert_inherits` function for type validation.
#'
#' @examples
#' # Assuming `positions` is a list of Position objects:
#' .register_securities(positions)
.register_securities <- function(positions) {
  for (pos in positions) {
    SMAManager:::assert_inherits(pos, "Position", "pos")
    sec <- pos$get_security()
    if (!exists(sec$get_id(), envir = registries$securities)) {
      assign(sec$get_id(), sec, envir = registries$securities)
    }
  }
  invisible(NULL) 
}