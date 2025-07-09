#' Internal helper: create many positions from Enfusion report
#' @param enfusion_report Data frame of Enfusion report rows
#' @param short_name Character portfolio short name
#' @include utils.R
#' @include api-functions.R
#' @import Rblpapi
#' @export
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

  if (length(ids) == 0) {
    return(list())
  }

  descriptions <- Rblpapi::bdp(ids, "DX615")
  inst_types <- Rblpapi::bdp(ids, "EX028")
  prices <- Rblpapi::bdp(ids, "PX_LAST")
  bics_lvl2 <- Rblpapi::bdp(ids, "BI012")
  bics_lvl3 <- Rblpapi::bdp(ids, "BI013")

  env <- registries$securities

  for (id in ids) {
    bbid <- tolower(id)
    if (exists(bbid, envir = env, inherits = FALSE)) next

    security <- Security$new(
      bbid = bbid,
      description = descriptions[id, "DX615"],
      instrument_type = inst_types[id, "EX028"],
      price = ifelse(
        inst_types[id, "EX028"] == "FixedIncome",
        prices[id, "PX_LAST"] / 100,
        prices[id, "PX_LAST"]
      ),
      bics_level_2 = bics_lvl2[id, "BI012"],
      bics_level_3 = bics_lvl3[id, "BI013"]
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
  setNames(positions, NULL)
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
  long_name, short_name, holdings_url, trade_url
) {
  enfusion_report <- dplyr::filter(
    enfusion::get_enfusion_report(holdings_url),
    !is.na(.data$Description))
  nav <- as.numeric(enfusion_report[["$ GL NAV"]][1])
  enfusion_report <- dplyr::filter(
    enfusion_report,
    .data$`Instrument Type` != "Cash"
  )
  positions <- .bulk_security_positions(
    enfusion_report = enfusion_report,
    portfolio_short_name = short_name
  )
  .portfolio(short_name, long_name, holdings_url, trade_url, nav, positions, create = TRUE)
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
  long_name, short_name, base_portfolio, holdings_url, trade_url
) {
  enfusion_report <- dplyr::filter(
    enfusion::get_enfusion_report(holdings_url),
    !is.na(.data$Description) #nolint
  )
  nav <- as.numeric(enfusion_report[["$ GL NAV"]][1])
  if (is.na(nav)) {
    nav <- 0
  }
  positions <- .bulk_security_positions(
    enfusion_report = enfusion_report,
    portfolio_short_name = short_name
  )
  .sma(short_name, long_name, holdings_url, trade_url, nav, positions, base_portfolio, create = TRUE)
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

#' Internal helper: create many positions from Enfusion report
#' @param enfusion_report Data frame of Enfusion report rows
#' @param short_name Character portfolio short name
#' @include utils.R
#' @include api-functions.R
#' @import Rblpapi
#' @export
.bulk_trade_positions <- function(trade_url, portfolio) {
  con <- tryCatch(Rblpapi:::defaultConnection(), error = function(e) NULL)
  if (is.null(con)) con <- Rblpapi::blpConnect()

  trade_report <- dplyr::filter(
    enfusion::get_enfusion_report(trade_url),
    !is.na(.data$Description) &
    .data$`Order Remaining Quantity` != 0
  )

  if (nrow(trade_report) == 0) {
    return(invisible(NULL))
  }

  for (i in 1:nrow(trade_report)) {
    x <- trade_report[i, ]
    id <- switch(
      x[["Instrument Type"]],
      "Bond" = Rblpapi::bdp(x[["FIGI"]], "DX194")$DX194,
      "Listed Option" = x[["BB Yellow Key"]],
      "Equity" = x[["BB Yellow Key"]],
      x[["Description"]]
    )

    remain <- as.numeric(x[["Order Remaining Quantity"]])
    total <- as.numeric(x[["Notional Quantity"]])
    if (total < 0) {
      remain <- -remain
    }

    .trade(
      security_id = id,
      portfolio_id = portfolio$get_short_name(),
      qty = remain,
      swap = x[["Is Financed"]],
      create = TRUE,
      assign_to_registry = TRUE
    )
  }
}