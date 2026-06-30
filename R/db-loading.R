#' @title Check connection to Enfusion
#' @description Checks connection to Enfusion based on whether a users is logged
#'  into Enfusion or not. Returns TRUE if logged in, FALSE if not.
#' @importFrom httr GET
#' @returns Bool
#' @export
check_enfusion_connection <- function() {
  tryCatch(
    {
      httr::GET("http://127.0.0.1:18443/exportReport")
      TRUE
    },
    error = function(cond) FALSE
  )
}

#' @title Download Enfusion Report Using API from Excel Add-In
#' @description This function downloads Enfusion reports using the same API as
#'  the Enfusion Excel Add-In. This cicumvents the need to use the REST API
#'  which is an additional cost the Enfusion License. It requires logging into
#'  the enfusion application which can be accomplished using the launch
#'  enfusion function.
#' @param reportWebServiceURL The Enfusion Report URL.
#'  Same as the one used when downloading reports in Excel.
#' @param trim Logical. If \code{TRUE} (default), rows where \code{Description}
#'  is \code{NA} are removed from the result.
#' @importFrom httr GET
#' @importFrom readr read_csv
#' @importFrom dplyr if_all
#' @importFrom dplyr everything
#' @export
get_enfusion_report <- function(reportWebServiceURL, trim = TRUE) { #nolint
  if (!check_enfusion_connection()) stop("Enfusion is not Running")
  # Change Web Service URL from rest API to app
  report_url <- gsub(
    "https://webservices.enfusionsystems.com/mobile/rest/reportservice/",
    "http://127.0.0.1:18443/",
    reportWebServiceURL
  )
  tryCatch({
    suppressMessages(
      raw_data <- readr::read_csv(report_url, show_col_types = FALSE)
    )
  }, error = function(e) {
    stop("No Response from Enfusion")
  })
  if (trim) {
    raw_data <- raw_data[!is.na(raw_data$Description), ]
  }
  raw_data
}



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
  if (!DBI::dbIsValid(con)) {
    stop("db_connect: connection is not valid after opening.")
  }
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
.fetch_portfolio_id_by_short_name <- function( #nolint
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
    register_sec(
      non_opt$bbid[i],
      non_opt$instrument_type[i],
      non_opt$description[i]
    )
  }
  for (i in seq_len(nrow(opts))) {
    register_sec(opts$bbid[i], opts$instrument_type[i], opts$description[i])
  }
  invisible(NULL)
}


#' @title Load Holdings from DB
#' @param portfolio_id Portfolio Id
#' @param con connection
.fetch_holdings_from_db <- function(portfolio_id, con = get_db_connection()) {
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
  holdings <- list()

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
      {
        holding <- .holding(
          sec_id              = r$bbid,
          qty                 = qty,
          swap                = isTRUE(r$is_financed),
          custodian_acct_id   = custodian_acct_id,
          trs_custodian_id    = trs_custodian_id
        )
        holdings <- c(holdings, holding)
      },
      error = function(e) {
        warning(
          "Skipped holding ", r$bbid, " in ", portfolio_short_name,
          ": ", conditionMessage(e)
        )
      }
    )
  }
  holdings
}




#' @title Load SMA Rules from DB
#' @param portfolio_id Portfolio Id (integer or short name string)
#' @param con connection
.fetch_sma_rules_from_db <- function(portfolio_id, con = get_db_connection()) {
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

  rules_db <- DBI::dbGetQuery(con, "
    SELECT l.*, d.*
    FROM sma_rule_link l
    JOIN sma_rule_definitions d ON l.definition_id = d.definition_id
    WHERE l.portfolio_id = $1
      AND l.active       = TRUE
  ", params = list(portfolio_db_id))

  if (nrow(rules_db) == 0) return(invisible(NULL))
  rules <- list()
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
      rules <- c(rules, rule)
    }, error = function(e) {
      warning("Failed to load rule '", r$rule_name, "': ", conditionMessage(e))
    })
  }
  invisible(rules)
}

#' @title Load Order Constructor data
#' @param portfolio_id Portfolio Id
#' @param con connection
.fetch_ordcon_from_db <- function(portfolio_id, con = get_db_connection()) {

  portfolio_db_id <- NULL
  if (checkmate::test_character(portfolio_id, min.len = 1)) {
    portfolio_db_id <- .fetch_portfolio_id_by_short_name(portfolio_id, con)
  }

  if (checkmate::test_number(portfolio_id, lower = 1)) {
    if (!.check_portfolio_db_id(portfolio_id, con)) {
      stop("No portfolio found with portfolio_id = ", portfolio_id)
    }
    portfolio_db_id <- portfolio_id
    portfolio_short_name <- .fetch_portfolio_short_name_by_id(portfolio_db_id, con) #nolint
  }

  pb_act <- DBI::dbGetQuery(
    con,
    "SELECT pb_name as name, pb_act_number as act, 'pb' as act_type
    FROM portfolio_pb_act
    WHERE portfolio_id = $1
    UNION ALL
    SELECT isda_name as name, isda_act_number as act, 'isda' as act_type
    FROM portfolio_isda_act
    WHERE portfolio_id = $1",
    params = list(portfolio_db_id)
  )

  selection_formulas <- DBI::dbGetQuery(
    con,
    "SELECT * FROM portfolio_act_selection WHERE portfolio_id = $1",
    params = list(portfolio_db_id)
  )

  list(
    pb_act_num =  lapply(
      split(pb_act, pb_act$name),
      \(x) setNames(as.list(x$act), toupper(x$act_type))
    ),
    pb_act_sel = eval(parse(text = selection_formulas$pb_act_sel[1])),
    isda_act_sel = eval(parse(text = selection_formulas$isda_act_sel[1]))
  )
}


#' Reload NAV and Positions for an Existing Portfolio from Database
#'
#' Refreshes a Portfolio or SMA object's NAV and positions from the database
#' without recreating the object or reloading rules. Useful for intraday
#' updates when you already have a fully configured object in memory.
#'
#' @param portfolio A Portfolio or SMA R6 object, or a character short name.
#' @param con DBI connection (from \code{db_connect()})
#' @return The portfolio object invisibly.
#' @importFrom DBI dbGetQuery
#' @export
reload_portfolio <- function(portfolio, con = get_db_connection()) {
  if (is.character(portfolio)) {
    portfolio <- .portfolio(portfolio, create = FALSE)
  }
  short_name <- portfolio$get_short_name()
  id_row <- DBI::dbGetQuery(
    con,
    "SELECT portfolio_id FROM portfolios WHERE name_short = $1",
    params = list(short_name)
  )
  if (nrow(id_row) == 0) {
    stop("Portfolio '", short_name, "' not found in database.")
  }
  db_id <- id_row$portfolio_id[[1]]

  nav_row <- DBI::dbGetQuery(
    con,
    "SELECT nav FROM portfolio_nav WHERE portfolio_id = $1 AND date = $2",
    params = list(db_id, date)
  )
  if (nrow(nav_row) == 1) portfolio$set_nav(as.numeric(nav_row$nav[[1]]))

  portfolio$clear_positions()
  load_portfolio_from_db(short_name, TRUE, con)
  update_bloomberg_fields()
  invisible(portfolio)
}


#' Load a Single Portfolio or SMA from Database
#'
#' Creates or replaces a single portfolio in the in-memory registry, loading
#' its NAV, positions, and (for SMAs) rules.
#' Prefer \code{load_all_portfolios_from_db()}
#' when you need everything; use this for targeted single-portfolio loads.
#'
#' @param portfolio_short_name Character. The portfolio short name.
#' @param load_securities Logical. If \code{TRUE}, calls
#'  \code{load_securities_from_db()} before loading holdings. Defaults to
#'  \code{FALSE}.
#' @param con DBI connection (from \code{db_connect()})
#' @return The Portfolio or SMA object invisibly.
#' @importFrom DBI dbGetQuery
#' @export
load_portfolio_from_db <- function(
  portfolio_short_name,
  load_securities = FALSE,
  con = get_db_connection()
) {

  port_id <- .fetch_portfolio_id_by_short_name(portfolio_short_name, con)
  p_row <- DBI::dbGetQuery(
    con,
    "SELECT p.*, n.nav
     FROM portfolios p
     JOIN portfolio_nav_latest n ON p.portfolio_id = n.portfolio_id 
     WHERE p.portfolio_id = $1",
    params = list(port_id)
  )

  if (nrow(p_row) == 0) {
    stop("No portfolio '", portfolio_short_name, "' found in database.")
  }

  if (load_securities) load_securities_from_db(con)

  if (p_row$type == "base") {
    port <- .portfolio(
      short_name          = portfolio_short_name,
      long_name           = p_row$name_long,
      nav                 = p_row$nav,
      positions           = list(),
      create              = TRUE,
      assign_to_registry  = TRUE
    )
  }

  if (p_row$type == "sma") {
    base <- .fetch_portfolio_short_name_by_id(p_row$base_portfolio_id)
    port <- .sma(
      short_name          = portfolio_short_name,
      long_name           = p_row$name_long,
      nav                 = p_row$nav,
      positions           = list(),
      base_portfolio      = base,
      create              = TRUE,
      assign_to_registry  = TRUE
    )

    rules <- .fetch_sma_rules_from_db(port_id, con)
    for (r in rules) port$add_rule(r)
  }
  holdings <- .fetch_holdings_from_db(port_id, con)
  for (h in holdings) port$add_holding(h)

  oc <- .fetch_ordcon_from_db(port_id, con)
  port$add_orderconstructor(oc$pb_act_num, oc$pb_act_sel, oc$isda_act_sel)

  invisible(port)
}


#' Load All Portfolios and SMAs from Database
#' @param con DBI connection
#' @return Named list of all Portfolio/SMA objects (invisibly)
#' @importFrom DBI dbGetQuery
#' @export
load_all_portfolios_from_db <- function(con = get_db_connection()) {
  all_ports <- DBI::dbGetQuery(con, "
    SELECT portfolio_id, name_long, name_short, type, base_portfolio_id
    FROM portfolios
    ORDER BY CASE WHEN type = 'base' THEN 0 ELSE 1 END
  ")
  if (nrow(all_ports) == 0) stop("No portfolios found in database")
  load_securities_from_db(con)
  portfolios <- lapply(all_ports$name_short, load_portfolio_from_db)
  update_bloomberg_fields()
  invisible(portfolios)
}
