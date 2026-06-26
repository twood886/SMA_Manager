#' @title Connect to Postgres Database
#' @description Connects to a Postgres database using environment variables for
#' connection parameters and caches the connection in the package environment so
#' it does not need to be passed to subsequent functions. Call this once at
#' startup (or at the top of an API handler) and all db functions will use it
#' automatically via \code{get_db_connection()}.
#' @param pg_password The password for the Postgres user.
#' @return The DBI connection object, invisibly.
#' @importFrom DBI dbConnect dbDisconnect dbIsValid
#' @importFrom RPostgres Postgres
#' @export
db_connect <- function(pg_password = Sys.getenv("PG_PASSWORD")) {
  if (!requireNamespace("RPostgres", quietly = TRUE)) {
    stop("Package 'RPostgres' is required but not installed.")
  }
  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname   = Sys.getenv("PGDATABASE", "postgres"),
    host     = Sys.getenv("PG_HOST", "aws-1-us-east-1.pooler.supabase.com"),
    port     = as.integer(Sys.getenv("PGPORT", "6543")),
    user     = Sys.getenv("PGUSER", "postgres.wpgzjoydsqnbojxtmemt"),
    password = pg_password,
    sslmode  = Sys.getenv("PGSSLMODE", "require")
  )
  if (!DBI::dbIsValid(con)) stop("db_connect: connection is not valid after opening.")
  .pkg_state$con <- con
  invisible(con)
}


#' @title Retrieve the Cached Database Connection
#' @description Returns the connection stored by \code{db_connect()}. Errors if
#' no connection has been established or the connection is no longer valid.
#' @return A DBI connection object.
#' @importFrom DBI dbIsValid
#' @export
get_db_connection <- function() {
  con <- .pkg_state$con
  if (is.null(con)) {
    stop("No database connection. Call db_connect() first.")
  }
  if (!DBI::dbIsValid(con)) {
    stop("Database connection is no longer valid. Call db_connect() again.")
  }
  con
}


#' @title Disconnect from Postgres Database
#' @description Disconnects the cached connection and clears it from the package
#' environment.
#' @return \code{TRUE} invisibly.
#' @importFrom DBI dbDisconnect
#' @export
db_disconnect <- function() {
  con <- .pkg_state$con
  if (!is.null(con)) {
    try(DBI::dbDisconnect(con), silent = TRUE)
    .pkg_state$con <- NULL
  }
  invisible(TRUE)
}


#' @title Update Holdings Data in Database
#' @description Updates the holdings data in the database by fetching the latest
#' Enfusion report and writing it to the database. This function requires a
#' valid database connection.
#' @param holdings_url URL to fetch the Enfusion holdings report. Default is set
#' to the shared Taylor SMA Manager Reports Position report.
#' @param con A valid DBI connection object to the Postgres database.
#' @return Returns TRUE invisibly if the update is successful, or FALSE if the
#' connection is NULL.
#' @importFrom DBI dbWithTransaction dbExecute dbWriteTable
.update_db_holdings_data <- function(
  holdings_url = "https://webservices.enfusionsystems.com/mobile/rest/reportservice/exportReport?name=shared%2FTaylor%2FSMA_Mgr_Reports%2FPosition.ppr", #nolint
  con = get_db_connection()
) {
  if (is.null(con)) return(invisible(FALSE))

  raw_holdings <- tryCatch(
    get_enfusion_report(
      holdings_url
    ),
    error = function(e) {
      message("Could not download holdings report: ", conditionMessage(e))
      invisible(FALSE)
    }
  )

  now <- Sys.time()

  holdings_date <- unique(format(
    as.Date(raw_holdings[["Position Scenario Date Adjusted"]], "%m/%d/%Y"),
    "%Y-%m-%d"
  ))

  if (length(holdings_date) > 1) {
    message("Multiple dates detected in holdings report")
    return(invisible(FALSE))
  }

  holdings_upload <- data.frame(
    date                     = holdings_date,
    portfolio_id             = as.integer(raw_holdings[["Fund Id"]]),
    instrument_id            = as.integer(raw_holdings[["Position Instrument Id"]]), #nolint
    custodian_acct_id        = as.integer(raw_holdings[["Custodian Acct Id"]]),
    instrument_type          = as.character(raw_holdings[["Instrument Type"]]),
    description              = as.character(raw_holdings[["Description"]]),
    bb_yellow_key                = as.character(raw_holdings[["BB Yellow Key"]]), #nolint
    bb_yellow_key_underlying     = as.character(raw_holdings[["BB Yellow Key Underlying"]]), #nolint
    figi                         = as.character(raw_holdings[["FIGI"]]),
    cusip                        = as.character(raw_holdings[["CUSIP"]]),
    underlying_instrument_id     = as.integer(raw_holdings[["Underlying Instrument Id"]]), #nolint
    underlying_instrument_type   = as.character(raw_holdings[["Underlying Instrument Type"]]), #nolint
    underlying_description       = as.character(raw_holdings[["Underlying Description"]]), #nolint
    underlying_cusip             = as.character(raw_holdings[["Underlying CUSIP"]]), #nolint
    quantity_stock               = as.integer(raw_holdings[["Stock Quantity"]]),
    quantity_option          = as.integer(raw_holdings[["Option Quantity"]]),
    is_financed              = as.logical(raw_holdings[["Is Financed"]]),
    trs_custodian_id         = as.integer(raw_holdings[["TRS Custodian ID"]]),
    update                   = now,
    stringsAsFactors = FALSE
  )

  DBI::dbWithTransaction(con, {
    DBI::dbExecute(
      con,
      "DELETE FROM holdings WHERE date = $1",
      params = list(as.Date(holdings_date))
    )
    DBI::dbWriteTable(
      con,
      "holdings",
      holdings_upload,
      append = TRUE
    )
  })

  invisible(TRUE)
}

#' @title Update NAV Data in Database
#' @description Updates the NAV data in the database by fetching the latest
#' Enfusion report and writing it to the database. This function requires a
#' valid database connection.
#' @param nav_url URL to fetch the Enfusion NAV report. Default is set
#' to the shared Taylor SMA Manager Reports NAV report.
#' @param con A valid DBI connection object to the Postgres database.
#' @return Returns TRUE invisibly if the update is successful, or FALSE if the
#' connection is NULL.
#' @importFrom DBI dbWithTransaction dbExecute dbWriteTable
.update_db_nav_data <- function(
  nav_url = "https://webservices.enfusionsystems.com/mobile/rest/reportservice/exportReport?name=shared%2FTaylor%2FSMA_Mgr_Reports%2FNAV.ppr", #nolint
  con = get_db_connection()
) {
  if (is.null(con)) return(invisible(FALSE))

  raw_nav <- tryCatch(
    get_enfusion_report(nav_url, trim = FALSE),
    error = function(e) {
      message("Could not download NAV report: ", conditionMessage(e))
      invisible(FALSE)
    }
  )

  raw_nav <- raw_nav[!is.na(raw_nav[["Fund Id"]]), ]
  now <- Sys.time()

  nav_date <- unique(format(
    as.Date(raw_nav[["Position Scenario Date Adjusted"]], "%m/%d/%Y"),
    "%Y-%m-%d"
  ))

  if (length(nav_date) > 1) {
    message("Multiple dates detected in NAV report")
    return(invisible(FALSE))
  }

  nav_upload <- data.frame(
    date = nav_date,
    portfolio_id = as.integer(raw_nav[["Fund Id"]]),
    nav = as.numeric(raw_nav[["$ GL NAV"]]),
    update = now,
    stringsAsFactors = FALSE
  )

  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, "DELETE FROM portfolio_nav WHERE date = $1",
                   params = list(as.Date(nav_date)))
    DBI::dbWriteTable(con, "portfolio_nav", nav_upload, append = TRUE)
  })

  invisible(TRUE)
}


#' @title Update Trade Data in Database
#' @description Updates the trade data in the database by fetching the latest
#' Enfusion report and writing it to the database. This function requires a
#' valid database connection.
#' @param trades_url URL to fetch the Enfusion trades report. Default is set
#' to the shared Taylor SMA Manager Reports Trade Detail report.
#' #' @param con A valid DBI connection object to the Postgres database.
#' @return Returns TRUE invisibly if the update is successful, or FALSE if the
#' connection is NULL.
#' @importFrom DBI dbWithTransaction dbExecute dbWriteTable
.update_db_trade_data <- function(
  trades_url = "https://webservices.enfusionsystems.com/mobile/rest/reportservice/exportReport?name=shared%2FTaylor%2FSMA_Mgr_Reports%2FSMA_Trade_Detail.trb", #nolint
  con = get_db_connection()
) {
  if (is.null(con)) return(invisible(FALSE))
  raw_trades <- tryCatch(
    get_enfusion_report(trades_url),
    error = function(e) {
      message("Could not download trades report: ", conditionMessage(e))
      NULL
    }
  )
  now <- Sys.time()

  if (!is.null(raw_trades)) {
    raw_trades <- raw_trades[!is.na(raw_trades[["Trade Id"]]), ]
  }

  if (is.null(raw_trades) || nrow(raw_trades) == 0) {
    return(invisible(TRUE))
    DBI::dbWithTransaction(con, {
      DBI::dbExecute(con, "DELETE FROM trades WHERE trade_id IS NOT NULL")
    })
  } else {
    # Trade Allocation Percent arrives as "39.40%" — strip % sign
    alloc_pct     <- as.numeric(gsub("%", "", raw_trades[["Trade Allocation Percent"]])) / 100
    qty_completed <- as.numeric(raw_trades[["Notional Quantity"]])
    parent_qty    <- as.numeric(raw_trades[["Parent Total Quantity"]])
    parent_qty[is.na(parent_qty)] <- 0
    txn_side      <- ifelse(as.character(raw_trades[["Txn Type"]]) %in% c("Sell", "Short"), -1L, 1L)
    qty_target    <- txn_side * abs(parent_qty) * alloc_pct

    trades_upload <- data.frame(
      trade_id                 = as.integer(raw_trades[["Trade Id"]]),
      instrument_id            = suppressWarnings(as.integer(raw_trades[["Instrument Id"]])),
      portfolio_id             = as.integer(raw_trades[["Portfolio Id"]]),
      trade_date               = as.Date(raw_trades[["Trade Date"]], "%m/%d/%Y"),
      txn_type                 = as.character(raw_trades[["Txn Type"]]),
      quantity_completed       = qty_completed,
      quantity_target          = qty_target,
      custodian_acct_id        = suppressWarnings(as.integer(raw_trades[["Custodian Acct Id"]])),
      instrument_type          = as.character(raw_trades[["Order Instrument Type"]]),
      description              = as.character(raw_trades[["Description"]]),
      bb_yellow_key                = as.character(raw_trades[["BB Yellow Key"]]),
      bb_yellow_key_underlying     = as.character(raw_trades[["Underlying BB Yellow Key"]]),
      is_financed                  = as.logical(raw_trades[["Is Financed"]]),
      figi                         = as.character(raw_trades[["FIGI"]]),
      cusip                        = as.character(raw_trades[["CUSIP"]]),
      underlying_instrument_id     = suppressWarnings(as.integer(raw_trades[["Underlying Instrument Id"]])),
      underlying_instrument_type   = suppressWarnings(as.character(raw_trades[["UnderlyingInstrumentType"]])),
      underlying_description       = suppressWarnings(as.character(raw_trades[["Underlying Description"]])),
      underlying_cusip             = suppressWarnings(as.character(raw_trades[["Underlying CUSIP"]])),
      trs_custodian_id             = suppressWarnings(as.integer(raw_trades[["TRS Custodian ID"]])),
      updated_at               = now,
      stringsAsFactors = FALSE
    )
    trades_upload$is_financed[is.na(trades_upload$is_financed)] <- FALSE

    DBI::dbWithTransaction(con, {
      DBI::dbExecute(con, "DELETE FROM trades WHERE trade_date IS NOT NULL")
      DBI::dbWriteTable(con, "trades", trades_upload, append = TRUE)
    })
  }
  invisible(TRUE)
}

#' @title Update Database with Latest Enfusion Data
#' @description Updates the database with the latest holdings and trade data
#' from Enfusion. This function requires a valid database connection.
#' @param con A valid DBI connection object to the Postgres database.
#' @return Returns TRUE invisibly if the update is successful, or FALSE if the
#' connection is NULL. If the connection is NULL, the function will not attempt
#' to update the database and will return FALSE immediately.
#' @export
update_db_data <- function(con = get_db_connection()) {
  if (!checkmate::test_class(con, "DBIConnection")) {
    message(
      "Invalid db connection. Please provide a valid DBIConnection object."
    )
    return(invisible(FALSE))
  }
  if (!DBI::dbIsValid(con)) {
    message("Database connection is not valid.")
    return(invisible(FALSE))
  }
  update_holdings <- .update_db_holdings_data(con = con)
  update_nav <- .update_db_nav_data(con = con)
  update_trades <- .update_db_trade_data(con = con)

  if (!update_holdings) {
    message("Failed to update holdings data in the database.")
  }
  if (!update_nav) {
    message("Failed to update NAV data in the database.")
  }
  if (!update_trades) {
    message("Failed to update trade data in the database.")
  }
  if (!update_holdings || !update_nav || !update_trades) {
    return(invisible(FALSE))
  }
  invisible(TRUE)
}


#' @title Check if Portfolio Exists in Database
#' @description Checks if a portfolio with the given portfolio_db_id exists in
#' the database.
#' @param portfolio_db_id The portfolio_id to check for existence in the
#'  database.
#' @param con A valid DBI connection object to the Postgres database.
#' @return Returns TRUE if the portfolio exists, FALSE otherwise.
.check_portfolio_db_id <- function(portfolio_db_id, con = get_db_connection()) {
  if (!is.numeric(portfolio_db_id)) stop("portfolio_db_id must be numeric.")
  if (length(portfolio_db_id) != 1) stop("portfolio_db_id must be a single value.") #nolint
  if (is.na(portfolio_db_id)) stop("portfolio_db_id cannot be NA.")
  db_id <- DBI::dbGetQuery(
    con,
    "SELECT portfolio_id FROM portfolios WHERE portfolio_id = $1",
    params = list(portfolio_db_id)
  )
  if (nrow(db_id) == 0) {
    message("No portfolio found with portfolio_db_id = ", portfolio_db_id)
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

#' @title Fetch Portfolio ID by Short Name
#' @description Retrieves the portfolio_id from the database for a given
#' portfolio short name.
#' @param short_name The short name of the portfolio to look up.
#' @param con A valid DBI connection object to the Postgres database.
#' @return Returns the portfolio_id as an integer if found, otherwise throws an
#' error.
.fetch_portfolio_id_by_short_name <- function(
  short_name,
  con = get_db_connection()
) {
  if (!is.character(short_name)) stop("short_name must be a character string.")
  if (length(short_name) != 1) stop("short_name must be a single value.")
  if (is.na(short_name)) stop("short_name cannot be NA.")
  db_id <- DBI::dbGetQuery(
    con,
    "SELECT portfolio_id FROM portfolios WHERE name_short = $1",
    params = list(short_name)
  )
  if (nrow(db_id) == 0) {
    stop("No portfolio found with short_name = ", short_name)
  }
  return(as.integer(db_id$portfolio_id[[1]]))
}


#' @title Fetch Portfolio Short Name by ID
#' @description Retrieves the short name of a portfolio from the database for a
#' given portfolio_id.
#' @param portfolio_id The portfolio_id to look up.
#' @param con A valid DBI connection object to the Postgres database.
#' @return Returns the short name of the portfolio as a character string if 
#' found, otherwise throws an error.
.fetch_portfolio_short_name_by_id <- function(
  portfolio_id,
  con = get_db_connection()
) {
  if (!is.numeric(portfolio_id)) stop("portfolio_id must be numeric.")
  if (length(portfolio_id) != 1) stop("portfolio_id must be a single value.")
  if (is.na(portfolio_id)) stop("portfolio_id cannot be NA.")
  db_name <- DBI::dbGetQuery(
    con,
    "SELECT name_short FROM portfolios WHERE portfolio_id = $1",
    params = list(portfolio_id)
  )
  if (nrow(db_name) == 0) {
    stop("No portfolio found with portfolio_id = ", portfolio_id)
  }
  return(as.character(db_name$name_short[[1]]))
}

#' Load Securities into Registry from Database
#'
#' Populates \code{registries$securities} with \code{Security} objects for all
#' instruments appearing in \code{holdings_target}. Securities are registered
#' with placeholder price and delta of 1; call \code{update_bloomberg_fields()}
#' after loading to hydrate prices, deltas, and rule fields from Bloomberg.
#' Non-option securities are registered first so underlyings exist when options
#' are created.
#'
#' @param con DBI connection (from \code{db_connect()})
#' @return \code{NULL} invisibly
#' @importFrom DBI dbGetQuery
#' @export
load_securities_from_db <- function(con = get_db_connection()) {
  sec_rows <- DBI::dbGetQuery(con, "
    SELECT DISTINCT
      s.instrument_id,
      s.instrument_type,
      s.description,
      lower(s.identifier) AS bbid
    FROM holdings_target h
    JOIN securities s ON h.instrument_id = s.instrument_id
    WHERE s.identifier IS NOT NULL
      AND s.identifier <> ''
  ")

  if (nrow(sec_rows) == 0) return(invisible(NULL))

  # Underlying bbid for options, keyed by the option's bbid
  underlying_rows <- DBI::dbGetQuery(con, "
    SELECT DISTINCT
      lower(s.identifier)               AS bbid,
      lower(h.bb_yellow_key_underlying) AS underlying_bbid
    FROM holdings h
    JOIN securities s ON h.instrument_id = s.instrument_id
    WHERE h.bb_yellow_key_underlying IS NOT NULL
      AND h.bb_yellow_key_underlying <> ''
  ")
  underlying_map <- setNames(
    underlying_rows$underlying_bbid,
    underlying_rows$bbid
  )

  env <- registries$securities

  register_sec <- function(bbid, instrument_type, description) {
    if (exists(bbid, envir = env, inherits = FALSE)) return(invisible(NULL))

    underlying_sec <- NULL
    if (identical(instrument_type, "Option")) {
      u_bbid <- underlying_map[[bbid]]
      if (!is.null(u_bbid) && exists(u_bbid, envir = env, inherits = FALSE)) {
        underlying_sec <- get(u_bbid, envir = env)
      }
    }

    tryCatch(
      assign(
        bbid,
        Security$new(
          bbid                = bbid,
          description         = description,
          instrument_type     = instrument_type,
          price               = 1,
          delta               = 1,
          underlying_security = underlying_sec
        ),
        envir = env
      ),
      error = function(e) {
        warning(
          "Failed to register security '", bbid, "': ", conditionMessage(e)
        )
      }
    )
    invisible(NULL)
  }

  # Non-options first so underlyings exist for options
  non_opt <- sec_rows[sec_rows$instrument_type != "Option", ]
  opts    <- sec_rows[sec_rows$instrument_type == "Option", ]

  for (i in seq_len(nrow(non_opt))) {
    register_sec(non_opt$bbid[i], non_opt$instrument_type[i], non_opt$description[i])
  }
  for (i in seq_len(nrow(opts))) {
    register_sec(opts$bbid[i], opts$instrument_type[i], opts$description[i])
  }

  invisible(NULL)
}



.load_holdings_from_db <- function(portfolio_id, con = get_db_connection()) {
  if (!checkmate::test_class(con, "DBIConnection")) {
    stop("Invalid db connection. Please provide a valid DBIConnection object.")
  }

  portfolio_db_id <- NULL
  if (checkmate::test_character(portfolio_id, min.len = 1)) {
    portfolio_db_id <- .fetch_portfolio_id_by_short_name(portfolio_id, con)
    portfolio_short_name <- portfolio_id
  }

  if (checkmate::test_number(portfolio_id, lower = 1)) {
    if (!.check_portfolio_db_id(portfolio_id, con)) {
      stop("No portfolio found with portfolio_id = ", portfolio_id)
    }
    portfolio_db_id <- portfolio_id
    portfolio_short_name <- .fetch_portfolio_short_name_by_id(portfolio_db_id, con) #nolint
  }

  holdings_data <- DBI::dbGetQuery(con, "
    SELECT
      h.instrument_type,
      h.quantity_stock,
      h.quantity_option,
      h.is_financed,
      h.custodian_acct_id,
      h.trs_custodian_id,
      lower(s.identifier) AS bbid
    FROM holdings_actual h
    JOIN securities s ON h.instrument_id = s.instrument_id
    WHERE h.portfolio_id = $1
      AND s.identifier IS NOT NULL
      AND s.identifier <> ''
  ", params = list(portfolio_db_id))

  if (nrow(holdings_data) == 0) return(invisible(NULL))

  for (i in seq_len(nrow(holdings_data))) {
    r <- holdings_data[i, ]
    qty <- NA
    custodian_acct_id <- NULL
    trs_custodian_id <- NULL

    qty <- if (identical(r$instrument_type, "Listed Option")) {
      as.numeric(r$quantity_option)
    } else {
      as.numeric(r$quantity_stock)
    }

    if (is.na(qty)) next

    if (!is.na(r$custodian_acct_id)) {
      custodian_acct_id <- as.character(r$custodian_acct_id)
    }

    if (!is.na(r$trs_custodian_id)) {
      trs_custodian_id <- as.character(r$trs_custodian_id)
    }

    tryCatch(
      .holding(
        portfolio_name      = portfolio_short_name,
        sec_id              = r$bbid,
        qty                 = qty,
        swap                = isTRUE(r$is_financed),
        custodian_acct_id   = custodian_acct_id,
        trs_custodian_id    = trs_custodian_id,
        create              = TRUE,
        assign_to_portfolio = TRUE
      ),
      error = function(e) {
        warning(
          "Skipped holding ", r$bbid, " in ", portfolio_short_name,
          ": ", conditionMessage(e)
        )
      }
    )
  }
  invisible(NULL)
}


#' @title Load SMA Rules from DB 
#' @param porfolios_id Portfolio_id
#' @param con connection
.load_sma_rules_from_db <- function(portfolio_id, con = get_db_connection()) {
  if (!checkmate::test_class(con, "DBIConnection")) {
    stop("Invalid db connection. Please provide a valid DBIConnection object.")
  }

  portfolio_db_id <- NULL
  if (checkmate::test_character(portfolio_id, min.len = 1)) {
    portfolio_db_id <- .fetch_portfolio_id_by_short_name(portfolio_id, con)
    portfolio_short_name <- portfolio_id
  }

  if (checkmate::test_number(portfolio_id, lower = 1)) {
    if (!.check_portfolio_db_id(portfolio_id, con)) {
      stop("No portfolio found with portfolio_id = ", portfolio_id)
    }
    portfolio_db_id <- portfolio_id
    portfolio_short_name <- .fetch_portfolio_short_name_by_id(portfolio_db_id, con) #nolint
  }

  sma <- .sma(portfolio_short_name, create = FALSE)

  rules_db <- DBI::dbGetQuery(con, "
    SELECT l.*, d.*
    FROM sma_rule_link l
    JOIN sma_rule_definitions d ON l.definition_id = d.definition_id
    WHERE l.portfolio_id = $1
      AND l.active       = TRUE
  ", params = list(portfolio_db_id))

  if (nrow(rules_db) == 0) return(invisible(NULL))
  for (i in seq_len(nrow(rules_db))) {
    r <- rules_db[i, ]
    bbfields <- NULL
    exclusions <- NULL
    if (!is.null(r$bbfields) && !is.na(r$bbfields) && nzchar(r$bbfields)) {
      bbfields <- unlist(jsonlite::fromJSON(r$bbfields))
    }

    tryCatch({
      rule <- .sma_rule(
        sma_name       = portfolio_short_name,
        rule_name      = r$rule_name,
        scope          = r$scope,
        definition     = eval(parse(text = r$definition)),
        bbfields       = bbfields,
        max_threshold  = r$max_threshold %||% Inf,
        min_threshold  = r$min_threshold %||% -Inf,
        swap_only      = isTRUE(r$swap_only),
        gross_exposure = isTRUE(r$gross_exposure),
        relative_to    = r$relative_to %||% "nav",
        exclusions     = exclusions,
        include        = r$include %||% "all"
      )
      sma$add_rule(rule)
    }, error = function(e) {
      warning("Failed to load rule '", r$rule_name, "': ", conditionMessage(e))
    })
  }
  invisible(NULL)
}


#' Reload NAV and Positions for an Existing Portfolio from Database
#'
#' Refreshes a Portfolio or SMA object's NAV and positions from the database
#' without recreating the object or reloading rules. Useful for intraday
#' updates when you already have a fully configured object in memory.
#'
#' @param portfolio A Portfolio or SMA R6 object, or a character short name.
#' @param con DBI connection (from \code{db_connect()})
#' @param date Date to load holdings for. Defaults to today.
#' @return The portfolio object invisibly.
#' @importFrom DBI dbGetQuery
#' @export
reload_portfolio <- function(
  portfolio,
  con = get_db_connection(),
  date = Sys.Date()
) {
  if (is.character(portfolio)) {
    portfolio <- .portfolio(portfolio, create = FALSE)
  }
  short_name <- portfolio$get_short_name()
  id_row <- DBI::dbGetQuery(
    con,
    "SELECT portfolio_id FROM portfolios WHERE name_short = $1",
    params = list(short_name)
  )
  if (nrow(id_row) == 0) stop("Portfolio '", short_name, "' not found in database.")
  db_id <- id_row$portfolio_id[[1]]

  nav_row <- DBI::dbGetQuery(
    con,
    "SELECT nav FROM portfolio_nav WHERE portfolio_id = $1 AND date = $2",
    params = list(db_id, date)
  )
  if (nrow(nav_row) == 1) portfolio$set_nav(as.numeric(nav_row$nav[[1]]))

  portfolio$clear_positions()
  .load_holdings_from_db(con, short_name, db_id, date)

  invisible(portfolio)
}


#' Load a Single Portfolio or SMA from Database
#'
#' Creates or replaces a single portfolio in the in-memory registry, loading
#' its NAV, positions, and (for SMAs) rules. Prefer \code{load_all_portfolios_from_db()}
#' when you need everything; use this for targeted single-portfolio loads.
#'
#' @param short_name Character. The portfolio short name.
#' @param con DBI connection (from \code{db_connect()})
#' @param date Date to load holdings for. Defaults to today.
#' @return The Portfolio or SMA object invisibly.
#' @importFrom DBI dbGetQuery
#' @export
load_portfolio_from_db <- function(
  short_name,
  con = get_db_connection(),
  date = Sys.Date()
) {
  port_row <- DBI::dbGetQuery(
    con,
    "SELECT portfolio_id, name_long, name_short, type, base_portfolio_id
     FROM portfolios WHERE name_short = $1",
    params = list(short_name)
  )
  if (nrow(port_row) == 0) stop("No portfolio '", short_name, "' found in database.")

  load_securities_from_db(con, date)

  db_id <- port_row$portfolio_id[[1]]
  nav_row <- DBI::dbGetQuery(
    con,
    "SELECT nav
     FROM portfolio_nav 
     WHERE portfolio_id = $1
      AND \"update\" = (
        SELECT MAX(\"update\") 
        FROM portfolio_nav 
        WHERE portfolio_id = $1
      )",
    params = list(db_id)
  )
  nav <- if (nrow(nav_row) == 1) as.numeric(nav_row$nav[[1]]) else 0

  if (port_row$type == "base") {
    .portfolio(
      short_name   = short_name,
      long_name    = port_row$name_long,
      holdings_url = "",
      nav          = nav,
      positions    = list(),
      create       = TRUE
    )
    .load_holdings_from_db(con, short_name, db_id, date)
    return(invisible(.portfolio(short_name, create = FALSE)))
  }

  # SMA — resolve base portfolio(s)
  sma_wts <- tryCatch(
    DBI::dbGetQuery(con, "
      SELECT p_base.name_short AS base_name_short, w.weight
      FROM sma_base_portfolio_weights w
      JOIN portfolios p_base ON w.base_portfolio_id = p_base.portfolio_id
      JOIN portfolios p_sma  ON w.sma_portfolio_id  = p_sma.portfolio_id
      WHERE p_sma.name_short = $1
    ", params = list(short_name)),
    error = function(e) data.frame(base_name_short = character(0), weight = numeric(0))
  )

  base_portfolio_arg <- if (nrow(sma_wts) == 0) {
    base_row <- DBI::dbGetQuery(
      con,
      "SELECT name_short FROM portfolios WHERE portfolio_id = $1",
      params = list(port_row$base_portfolio_id[[1]])
    )
    if (nrow(base_row) == 0) stop("No base portfolio found for SMA '", short_name, "'.")
    base_row$name_short
  } else if (nrow(sma_wts) == 1) {
    sma_wts$base_name_short
  } else {
    setNames(sma_wts$weight, sma_wts$base_name_short)
  }

  .sma(
    short_name     = short_name,
    long_name      = port_row$name_long,
    holdings_url   = "",
    nav            = nav,
    positions      = list(),
    base_portfolio = base_portfolio_arg,
    create         = TRUE
  )
  .load_holdings_from_db(con, short_name, db_id, date)
  .load_rules_from_db(con, short_name, db_id)

  invisible(.sma(short_name, create = FALSE))
}


#' Load All Portfolios and SMAs from Database
#'
#' Clears the in-memory registries and repopulates them from Supabase.
#' Base portfolios are loaded before SMAs. After all positions are loaded,
#' Bloomberg rule fields are hydrated from \code{security_data}.
#'
#' Blended base portfolios are supported via the \code{sma_base_portfolio_weights}
#' junction table. Run the following DDL once before using blended bases:
#' \preformatted{
#' CREATE TABLE IF NOT EXISTS sma_base_portfolio_weights (
#'   sma_portfolio_id  INTEGER NOT NULL REFERENCES portfolios(portfolio_id),
#'   base_portfolio_id INTEGER NOT NULL REFERENCES portfolios(portfolio_id),
#'   weight            NUMERIC(8,6) NOT NULL CHECK (weight > 0 AND weight <= 1),
#'   PRIMARY KEY (sma_portfolio_id, base_portfolio_id)
#' );
#' -- Migrate existing single-base SMAs
#' INSERT INTO sma_base_portfolio_weights
#'   (sma_portfolio_id, base_portfolio_id, weight)
#' SELECT portfolio_id, base_portfolio_id, 1.0
#' FROM portfolios
#' WHERE type = 'sma' AND base_portfolio_id IS NOT NULL
#' ON CONFLICT DO NOTHING;
#' }
#' SMAs with no rows in the junction table fall back to \code{base_portfolio_id}
#' in the \code{portfolios} table for backward compatibility.
#'
#' @param con DBI connection
#' @param date Date to load holdings for. Defaults to today.
#' @return Named list of all Portfolio/SMA objects (invisibly)
#' @importFrom DBI dbGetQuery
#' @export
load_all_portfolios_from_db <- function(con = get_db_connection(), date = Sys.Date()) {
  all_ports <- DBI::dbGetQuery(con, "
    SELECT portfolio_id, name_long, name_short, type, base_portfolio_id
    FROM portfolios
    ORDER BY CASE WHEN type = 'base' THEN 0 ELSE 1 END
  ")

  if (nrow(all_ports) == 0) stop("No portfolios found in database")

  load_securities_from_db(con, date)

  nav_all <- DBI::dbGetQuery(con, "
    SELECT portfolio_id, nav
    FROM portfolio_nav
    WHERE date = $1
  ", params = list(date))

  nav_map <- setNames(
    as.numeric(nav_all$nav), as.character(nav_all$portfolio_id)
  )

  # Load blend weights from junction table (empty if table doesn't exist yet)
  base_weights <- tryCatch(
    DBI::dbGetQuery(con, "
      SELECT
        p_sma.name_short  AS sma_name_short,
        p_base.name_short AS base_name_short,
        w.weight
      FROM sma_base_portfolio_weights w
      JOIN portfolios p_sma  ON w.sma_portfolio_id  = p_sma.portfolio_id
      JOIN portfolios p_base ON w.base_portfolio_id = p_base.portfolio_id
    "),
    error = function(e) {
      warning(
        "sma_base_portfolio_weights table not found — ",
        "falling back to base_portfolio_id. ",
        "See load_all_portfolios_from_db() docs for migration DDL."
      )
      data.frame(
        sma_name_short  = character(0),
        base_name_short = character(0),
        weight          = numeric(0)
      )
    }
  )

  # Base portfolios
  bases <- all_ports[all_ports$type == "base", ]
  for (i in seq_len(nrow(bases))) {
    b   <- bases[i, ]
    nav <- nav_map[as.character(b$portfolio_id)]
    nav <- if (length(nav) == 1 && !is.na(nav)) as.numeric(nav) else 0
    .portfolio(
      short_name   = b$name_short,
      long_name    = b$name_long,
      holdings_url = "",
      nav          = nav,
      positions    = list(),
      create       = TRUE
    )
    .load_holdings_from_db(con, b$name_short, b$portfolio_id, date)
  }

  # SMAs
  smas <- all_ports[all_ports$type == "sma", ]
  for (i in seq_len(nrow(smas))) {
    s <- smas[i, ]
    nav <- nav_map[as.character(s$portfolio_id)]
    nav <- if (length(nav) == 1 && !is.na(nav)) as.numeric(nav) else 0

    # Resolve base portfolio(s) — junction table first, legacy FK as fallback
    sma_wts <- base_weights[base_weights$sma_name_short == s$name_short, ]
    base_portfolio_arg <- if (nrow(sma_wts) == 0) {
      base_row <- all_ports[all_ports$portfolio_id == s$base_portfolio_id, ]
      if (nrow(base_row) == 0) {
        warning(
          "No base portfolio found for SMA '", s$name_short, "' — skipping."
        )
        next
      }
      base_row$name_short
    } else if (nrow(sma_wts) == 1) {
      sma_wts$base_name_short
    } else {
      setNames(sma_wts$weight, sma_wts$base_name_short)
    }

    .sma(
      short_name     = s$name_short,
      long_name      = s$name_long,
      holdings_url   = "",
      nav            = nav,
      positions      = list(),
      base_portfolio = base_portfolio_arg,
      create         = TRUE
    )
    .load_holdings_from_db(con, s$name_short, s$portfolio_id, date)
    .load_rules_from_db(con, s$name_short, s$portfolio_id)
  }

  update_bloomberg_fields()

  invisible(
    mget(
      all_ports$name_short,
      envir    = get_registries()$portfolios,
      inherits = FALSE
    )
  )
}
