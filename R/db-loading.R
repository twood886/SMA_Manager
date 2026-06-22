#' Connect to Supabase PostgreSQL
#' @importFrom DBI dbConnect
#' @importFrom RPostgres Postgres
#' @export
db_connect <- function() {
  DBI::dbConnect(
    RPostgres::Postgres(),
    dbname   = Sys.getenv("PGDATABASE", "postgres"),
    host     = Sys.getenv("PG_HOST", "aws-1-us-east-1.pooler.supabase.com"),
    port     = as.integer(Sys.getenv("PGPORT", "6543")),
    user     = Sys.getenv("PGUSER", "postgres.wpgzjoydsqnbojxtmemt"),
    password = Sys.getenv("PGPASSWORD"),
    sslmode  = Sys.getenv("PGSSLMODE", "require")
  )
}

#' Load Securities into Registry from Database
#'
#' Populates \code{registries$securities} with \code{Security} objects for all
#' instruments appearing in holdings on the given date. Non-option securities
#' are registered first so that option underlyings are available when options
#' are created.
#'
#' @param con DBI connection (from \code{db_connect()})
#' @param date Date to load. Defaults to today.
#' @return \code{NULL} invisibly
#' @importFrom DBI dbGetQuery
#' @export
load_securities_from_db <- function(con, date = Sys.Date()) {
  sec_rows <- DBI::dbGetQuery(con, "
    SELECT DISTINCT
      s.instrument_id,
      s.instrument_type,
      s.description,
      lower(s.identifier) AS bbid
    FROM holdings h
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

  instrument_ids <- sec_rows$instrument_id

  price_rows <- DBI::dbGetQuery(con, "
    SELECT DISTINCT ON (sd.instrument_id)
      lower(s.identifier) AS bbid, sd.value_numeric
    FROM security_data sd
    JOIN securities s ON sd.instrument_id = s.instrument_id
    WHERE sd.field_id = 'PX_LAST'
      AND sd.date   <= $1
      AND sd.instrument_id = ANY($2)
    ORDER BY sd.instrument_id, sd.date DESC
  ", params = list(date, I(instrument_ids)))
  price_map <- setNames(price_rows$value_numeric, price_rows$bbid)

  delta_rows <- DBI::dbGetQuery(con, "
    SELECT DISTINCT ON (sd.instrument_id)
      lower(s.identifier) AS bbid, sd.value_numeric
    FROM security_data sd
    JOIN securities s ON sd.instrument_id = s.instrument_id
    WHERE sd.field_id = 'OP006'
      AND sd.date   <= $1
      AND sd.instrument_id = ANY($2)
    ORDER BY sd.instrument_id, sd.date DESC
  ", params = list(date, I(instrument_ids)))
  delta_map <- setNames(delta_rows$value_numeric, delta_rows$bbid)

  env <- registries$securities

  register_sec <- function(bbid, instrument_type, description) {
    if (exists(bbid, envir = env, inherits = FALSE)) return(invisible(NULL))

    price <- price_map[[bbid]] %||% NA_real_
    # DB stores fixed income at full price (e.g. 101.5); convert to decimal
    if (!is.na(price) && identical(instrument_type, "FixedIncome")) {
      price <- price / 100
    }
    if (is.na(price) || !is.finite(price)) price <- 1

    delta <- delta_map[[bbid]] %||% NA_real_
    if (is.na(delta) || !is.finite(delta)) delta <- 1

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
          price               = price,
          delta               = delta,
          underlying_security = underlying_sec
        ),
        envir = env
      ),
      error = function(e) {
        warning("Failed to register security '", bbid, "': ", conditionMessage(e))
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


#' Update SMA Rule Fields from Database
#'
#' Reads Bloomberg field values from \code{security_data} and calls
#' \code{set_rule_data()} on every registered \code{Security} object.
#' Replaces \code{update_bloomberg_fields()} in the DB-backed workflow.
#'
#' @param con DBI connection
#' @param date Date to use. Defaults to today.
#' @return \code{TRUE} invisibly
#' @importFrom DBI dbGetQuery
#' @export
update_bloomberg_fields_from_db <- function(con, date = Sys.Date()) {
  rule_names <- ls(get_registries()$smarules)
  if (length(rule_names) == 0) return(invisible(TRUE))

  rules <- mget(rule_names, envir = get_registries()$smarules, inherits = TRUE)
  bbfields <- unique(unlist(lapply(rules, \(r) r$get_bbfields()), use.names = FALSE))
  bbfields <- bbfields[!is.null(bbfields) & !is.na(bbfields) & nzchar(bbfields)]
  if (length(bbfields) == 0) return(invisible(TRUE))

  sec_ids <- ls(get_registries()$securities)
  if (length(sec_ids) == 0) return(invisible(TRUE))

  id_map <- DBI::dbGetQuery(con, "
    SELECT lower(identifier) AS bbid, instrument_id
    FROM securities
    WHERE lower(identifier) = ANY($1)
  ", params = list(sec_ids))
  if (nrow(id_map) == 0) return(invisible(TRUE))

  sd <- DBI::dbGetQuery(con, "
    SELECT lower(s.identifier) AS bbid,
           sd.field_id,
           sd.value_text,
           sd.value_numeric,
           sd.value_boolean
    FROM security_data sd
    JOIN securities s ON sd.instrument_id = s.instrument_id
    WHERE sd.field_id    = ANY($1)
      AND sd.date        = $2
      AND sd.instrument_id = ANY($3)
  ", params = list(bbfields, date, id_map$instrument_id))

  for (i in seq_len(nrow(sd))) {
    sec <- tryCatch(
      .security(sd$bbid[i], create = FALSE),
      error = function(e) NULL
    )
    if (is.null(sec)) next
    value <- if (!is.na(sd$value_numeric[i])) {
      sd$value_numeric[i]
    } else if (!is.na(sd$value_boolean[i])) {
      sd$value_boolean[i]
    } else {
      sd$value_text[i]
    }
    tryCatch(
      sec$set_rule_data(sd$field_id[i], value),
      error = function(e) NULL
    )
  }
  invisible(TRUE)
}


#' Load Holdings from DB into a Portfolio's Registry Entry
#'
#' @param con DBI connection
#' @param portfolio_short_name Character portfolio short name
#' @param portfolio_db_id Integer portfolio_id in the \code{portfolios} table
#' @param date Date
#' @keywords internal
.load_holdings_from_db <- function(con, portfolio_short_name, portfolio_db_id, date) {
  # holdings_actual = EOD snapshot + completed intraday fills.
  # Falls back to raw holdings when no trades exist for today.
  rows <- DBI::dbGetQuery(con, "
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

  if (nrow(rows) == 0) return(invisible(NULL))

  for (i in seq_len(nrow(rows))) {
    r <- rows[i, ]
    qty <- if (identical(r$instrument_type, "Listed Option")) {
      as.numeric(r$quantity_option)
    } else {
      as.numeric(r$quantity_stock)
    }
    if (is.na(qty)) next

    custodian_acct_id <- if (!is.na(r$custodian_acct_id)) {
      as.character(r$custodian_acct_id)
    } else NULL
    trs_custodian_id <- if (!is.na(r$trs_custodian_id)) {
      as.character(r$trs_custodian_id)
    } else NULL

    tryCatch(
      .holding(
        portfolio_short_name,
        r$bbid, qty,
        swap              = isTRUE(r$is_financed),
        custodian_acct_id = custodian_acct_id,
        trs_custodian_id  = trs_custodian_id,
        create            = TRUE,
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


#' Load Rules for an SMA from Database and Attach to the SMA Object
#'
#' @param con DBI connection
#' @param portfolio_short_name Character short name of the SMA
#' @param portfolio_db_id Integer portfolio_id
#' @importFrom jsonlite fromJSON
#' @keywords internal
.load_rules_from_db <- function(con, portfolio_short_name, portfolio_db_id) {
  rules <- DBI::dbGetQuery(con, "
    SELECT r.*
    FROM sma_rules r
    JOIN sma_rule_link l ON r.rule_id = l.rule_id
    WHERE l.portfolio_id = $1
      AND r.active       = TRUE
  ", params = list(portfolio_db_id))

  if (nrow(rules) == 0) return(invisible(NULL))

  sma <- .sma(portfolio_short_name, create = FALSE)

  for (i in seq_len(nrow(rules))) {
    r <- rules[i, ]

    bbfields <- if (!is.null(r$bbfields) && !is.na(r$bbfields) && nzchar(r$bbfields)) {
      unlist(jsonlite::fromJSON(r$bbfields))
    } else NULL

    exclusions <- if (!is.null(r$exclusions) && !is.na(r$exclusions) && nzchar(r$exclusions)) {
      unlist(jsonlite::fromJSON(r$exclusions))
    } else NULL

    # definition may be NULL if not yet migrated from YAML
    if (is.null(r$definition) || is.na(r$definition) || !nzchar(r$definition)) {
      warning("Rule '", r$rule_name, "' has no definition — skipping.")
      next
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
        side           = if (!is.na(r$side)) r$side else NULL,
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


#' Load All Portfolios and SMAs from Database
#'
#' Clears the in-memory registries and repopulates them from Supabase.
#' Base portfolios are loaded before SMAs. After all positions are loaded,
#' Bloomberg rule fields are hydrated from \code{security_data}.
#'
#' @param con DBI connection
#' @param date Date to load holdings for. Defaults to today.
#' @return Named list of all Portfolio/SMA objects (invisibly)
#' @importFrom DBI dbGetQuery
#' @export
load_all_portfolios_from_db <- function(con, date = Sys.Date()) {
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

  # Base portfolios
  bases <- all_ports[all_ports$type == "base", ]
  for (i in seq_len(nrow(bases))) {
    b   <- bases[i, ]
    nav <- nav_map[as.character(b$portfolio_id)]
    nav <- if (length(nav) == 1 && !is.na(nav)) as.numeric(nav) else 0
    .portfolio(
      short_name       = b$name_short,
      long_name        = b$name_long,
      holdings_url     = "",
      nav              = nav,
      positions        = list(),
      create           = TRUE
    )
    .load_holdings_from_db(con, b$name_short, b$portfolio_id, date)
  }

  # SMAs
  smas <- all_ports[all_ports$type == "sma", ]
  for (i in seq_len(nrow(smas))) {
    s        <- smas[i, ]
    base_row <- all_ports[all_ports$portfolio_id == s$base_portfolio_id, ]
    if (nrow(base_row) == 0) {
      warning("No base portfolio found for SMA '", s$name_short, "' — skipping.")
      next
    }
    nav <- nav_map[as.character(s$portfolio_id)]
    nav <- if (length(nav) == 1 && !is.na(nav)) as.numeric(nav) else 0
    .sma(
      short_name     = s$name_short,
      long_name      = s$name_long,
      holdings_url   = "",
      nav            = nav,
      positions      = list(),
      base_portfolio = base_row$name_short,
      create         = TRUE
    )
    .load_holdings_from_db(con, s$name_short, s$portfolio_id, date)
    .load_rules_from_db(con, s$name_short, s$portfolio_id)
  }

  update_bloomberg_fields_from_db(con, date)

  invisible(
    mget(
      all_ports$name_short,
      envir    = get_registries()$portfolios,
      inherits = FALSE
    )
  )
}
